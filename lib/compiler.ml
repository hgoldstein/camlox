type parser = {
  scanner : Scanner.t;
  chunk : Chunk.t;
  arena : String_arena.t;
  mutable previous : Token.t;
  mutable current : Token.t;
  mutable had_error : bool;
  mutable panic_mode : bool;
}

module Op = Chunk.OpCode

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

  let can_assign = function
    | NoPrec | Assignment -> true
    | Or | And | Equality | Comparison | Term | Factor | Unary | Call | Primary
      ->
        false
end

type rule = {
  prefix : (parser -> bool -> unit) option;
  infix : (parser -> bool -> unit) option;
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
  emit_opcode p Op.Constant;
  emit_byte p c

let number p _ =
  let value = Value.Float (Float.of_string p.previous.content) in
  emit_constant p value

let identifier_constant p (token : Token.t) =
  make_constant p @@ String_arena.get p.arena token.content

let rec expression parser : unit = parse_precedence parser Precedence.Assignment

and grouping p _ =
  expression p;
  consume p Token.RightParen "Expect ')' after expression."

and unary p _ =
  let op = p.previous.kind in
  parse_precedence p Precedence.Unary;
  match op with
  | Token.Minus -> emit_opcode p Op.Negate
  | Token.Bang -> emit_opcode p Op.Not
  | _ -> assert false

and binary p _ =
  let op = p.previous.kind in
  let rule = get_rule op in
  parse_precedence p @@ Precedence.next rule.precedence;
  let e = emit_opcode p in
  match op with
  | Token.BangEqual ->
      e Equal;
      e Not
  | Token.EqualEqual -> e Equal
  | Token.Greater -> e Greater
  | Token.GreaterEqual ->
      e Less;
      e Not
  | Token.Less -> e Less
  | Token.LessEqual ->
      e Greater;
      e Not
  | Token.Plus -> e Add
  | Token.Minus -> e Subtract
  | Token.Star -> e Multiply
  | Token.Slash -> e Divide
  | _ -> assert false

and parse_precedence p prec =
  advance p;
  let can_assign = Precedence.can_assign prec in
  let rule = get_rule p.previous.kind in
  (match rule.prefix with
  | None -> error p "Expect expression."
  | Some prefix_rule -> prefix_rule p can_assign);
  while Precedence.compare prec (get_rule p.current.kind).precedence <= 0 do
    advance p;
    match (get_rule p.previous.kind).infix with
    | Some infix_rule -> infix_rule p can_assign
    | None -> assert false
  done;
  match p.current.kind with
  | Token.Equal when can_assign ->
      advance p;
      error p "Invalid assignment target."
  | _ -> ()

and literal parser _ =
  match parser.previous.kind with
  | Token.False -> emit_opcode parser Op.False
  | Token.True -> emit_opcode parser Op.True
  | Token.Nil -> emit_opcode parser Op.Nil
  | _ -> assert false

and string_ parser _ =
  let v = parser.previous.content in
  emit_constant parser
  @@ String_arena.get parser.arena
  @@ String.sub v 1 (String.length v - 2)

and named_variable p tok can_assign =
  let arg = identifier_constant p tok in
  (match p.current.kind with
  | Token.Equal when can_assign ->
      advance p;
      expression p;
      emit_opcode p Op.SetGlobal
  | _ -> emit_opcode p Op.GetGlobal);
  emit_byte p arg

and variable p can_assign = named_variable p p.previous can_assign

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
  | Token.False -> make_rule ~prefix:(Some literal) ()
  | Token.True -> make_rule ~prefix:(Some literal) ()
  | Token.Nil -> make_rule ~prefix:(Some literal) ()
  | Token.Bang -> make_rule ~prefix:(Some unary) ()
  | Token.BangEqual -> make_rule ~infix:(Some binary) ~prec:Equality ()
  | Token.EqualEqual -> make_rule ~infix:(Some binary) ~prec:Equality ()
  | Token.Greater -> make_rule ~infix:(Some binary) ~prec:Comparison ()
  | Token.GreaterEqual -> make_rule ~infix:(Some binary) ~prec:Comparison ()
  | Token.Less -> make_rule ~infix:(Some binary) ~prec:Comparison ()
  | Token.LessEqual -> make_rule ~infix:(Some binary) ~prec:Comparison ()
  | Token.String -> make_rule ~prefix:(Some string_) ()
  | Token.Identifier -> make_rule ~prefix:(Some variable) ()
  | _ -> make_rule ()

let end_compiler p =
  emit_opcode p Op.Return;
  if Dbg.on () && not p.had_error then Dbg.disassemble_chunk p.chunk "code"

let print_statement p =
  expression p;
  consume p Token.Semicolon "Expect ';' after value.";
  emit_opcode p Op.Print

let expression_statement p =
  expression p;
  consume p Token.Semicolon "Expect ';' after expression.";
  emit_opcode p Op.Pop

let statement p =
  match p.current.kind with
  | Token.Print ->
      advance p;
      print_statement p
  | _ -> expression_statement p

let synchronize p =
  p.panic_mode <- false;
  let rec skip p =
    match (p.previous.kind, p.current.kind) with
    | Token.Semicolon, _
    | _, Token.Eof
    | _, Token.Class
    | _, Token.Fun
    | _, Token.Var
    | _, Token.For
    | _, Token.If
    | _, Token.While
    | _, Token.Print
    | _, Token.Return ->
        ()
    | _, _ ->
        advance p;
        skip p
  in
  skip p

let parse_variable p msg =
  consume p Token.Identifier msg;
  identifier_constant p p.previous

let define_variable p global =
  emit_opcode p Op.DefineGlobal;
  emit_byte p global

let var_declaration p =
  let global = parse_variable p "Expect variable name." in
  (match p.current.kind with
  | Token.Equal ->
      advance p;
      expression p
  | _ -> emit_opcode p Op.Nil);
  consume p Token.Semicolon "Expect ; after variable declaration";
  define_variable p global

let declaration p =
  match p.current.kind with
  | Token.Var ->
      advance p;
      var_declaration p
  | _ -> statement p

let rec declarations p =
  match p.current.kind with
  | Token.Eof ->
      advance p;
      ()
  | _ ->
      declaration p;
      if p.panic_mode then synchronize p;
      declarations p

let compile (source : string) : (Chunk.t * String_arena.t, Err.t) result =
  let p : parser =
    {
      scanner = Scanner.of_string source;
      chunk = Chunk.make ();
      arena = Table.make ();
      previous = Token.{ content = ""; kind = Token.Error; line = -1 };
      current = Token.{ content = ""; kind = Token.Error; line = -1 };
      had_error = false;
      panic_mode = false;
    }
  in
  advance p;
  declarations p;
  end_compiler p;
  Ok (p.chunk, p.arena)
