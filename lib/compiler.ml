type parser = {
  scanner : Scanner.t;
  chunk : Chunk.t;
  mutable previous : Token.t;
  mutable current : Token.t;
  mutable had_error : bool;
  mutable panic_mode : bool;
}

module Precedence = struct
  type t =
    | NoPrec
    | Assignment
    | Or
    | And
    | Equality
    | Comparison
    | Term
    | Factor
    | Unary
    | Call
    | Primary
  [@@deriving ord]

  let next = function
    | NoPrec -> Assignment
    | Assignment -> Or
    | Or -> And
    | And -> Equality
    | Equality -> Comparison
    | Comparison -> Term
    | Term -> Factor
    | Factor -> Unary
    | Unary -> Call
    | Call -> Primary
    | Primary -> failwith "Unexpected argument to `Precedence.next`: Primary"
end

type rule = {
  prefix : (parser -> unit) option;
  infix : (parser -> unit) option;
  precedence : Precedence.t;
}

let error_at parser (token : Token.t) (message : string) =
  let body () =
    parser.panic_mode <- true;
    Printf.eprintf "[line %d] Error" token.line;
    (match token.kind with
    | Token.Eof -> Printf.eprintf " at end"
    | Token.Error -> ()
    | _ -> Printf.eprintf " at '%s'" token.content);
    Printf.eprintf ": %s\n" message;
    parser.had_error <- true
  in
  if not parser.panic_mode then body ()

let error_at_current parser message = error_at parser parser.current message
let error parser message = error_at parser parser.previous message

let advance parser =
  parser.previous <- parser.current;
  let rec aux () =
    parser.current <- Scanner.next parser.scanner;
    match parser.current with
    | { kind = Token.Error; content; _ } ->
        error_at_current parser content;
        aux ()
    | _ -> ()
  in
  aux ()

let consume parser k msg : unit =
  match parser.current.kind with
  | k' when k = k' ->
      advance parser;
      ()
  | _ -> error_at_current parser msg

let emit_byte parser byte =
  Chunk.write_byte ~chunk:parser.chunk ~byte ~line:parser.previous.line

let emit_opcode parser opcode = emit_byte parser @@ Chunk.OpCode.to_byte opcode

let make_constant p value =
  let c = Chunk.add_constant ~chunk:p.chunk ~value in
  if c > 0xFF then (
    error p "Too many constants in one chunk.";
    Char.chr 0)
  else Char.chr c

let emit_constant p value =
  let c = make_constant p value in
  emit_opcode p Chunk.OpCode.Constant;
  emit_byte p c

let number p =
  let value = Value.Float (Float.of_string p.previous.content) in
  emit_constant p value

let rec expression parser : unit = parse_precedence parser Precedence.Assignment

and grouping p =
  expression p;
  consume p Token.RightParen "Expect ')' after expression."

and unary p =
  let op = p.previous.kind in
  parse_precedence p Precedence.Unary;
  match op with
  | Token.Minus -> emit_opcode p Chunk.OpCode.Negate
  | _ -> assert false

and binary p =
  let op = p.previous.kind in
  let rule = get_rule op in
  parse_precedence p @@ Precedence.next rule.precedence;
  let open Chunk.OpCode in
  emit_opcode p
  @@
  match op with
  | Token.Plus -> Add
  | Token.Minus -> Subtract
  | Token.Star -> Multiply
  | Token.Slash -> Divide
  | _ -> assert false

and parse_precedence p prec =
  advance p;
  let rule = get_rule p.previous.kind in
  (match rule.prefix with
  | None -> error p "Expect expression."
  | Some prefix_rule -> prefix_rule p);
  while Precedence.compare prec (get_rule p.current.kind).precedence <= 0 do
    advance p;
    match (get_rule p.previous.kind).infix with
    | Some infix_rule -> infix_rule p
    | None -> assert false
  done

and get_rule : Token.kind -> rule =
  let open Precedence in
  let make_rule ?(prefix = None) ?(infix = None) ?(prec = NoPrec) () =
    { prefix; infix; precedence = prec }
  in
  function
  | Token.LeftParen -> make_rule ~prefix:(Some grouping) ()
  | Token.Minus ->
      make_rule ~prefix:(Some unary) ~infix:(Some binary) ~prec:Term ()
  | Token.Plus -> make_rule ~infix:(Some binary) ~prec:Term ()
  | Token.Slash -> make_rule ~infix:(Some binary) ~prec:Factor ()
  | Token.Star -> make_rule ~infix:(Some binary) ~prec:Factor ()
  | Token.Number -> make_rule ~prefix:(Some number) ()
  | _ -> make_rule ()

let end_compiler p =
  emit_opcode p Chunk.OpCode.Return;
  if Debug.on () && not p.had_error then Debug.disassemble_chunk p.chunk "code"

let compile (source : string) : (Chunk.t, Err.t) result =
  let p : parser =
    {
      scanner = Scanner.of_string source;
      chunk = Chunk.make ();
      previous = Token.{ content = ""; kind = Token.Error; line = -1 };
      current = Token.{ content = ""; kind = Token.Error; line = -1 };
      had_error = false;
      panic_mode = false;
    }
  in
  advance p;
  expression p;
  consume p Token.Eof "Expect end of expression.";
  end_compiler p;
  Ok p.chunk
