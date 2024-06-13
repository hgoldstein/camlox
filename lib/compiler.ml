type parser = {
  scanner : Scanner.t;
  chunk : Chunk.t;
  arena : String_arena.t;
  local_tracker : Locals.t;
  mutable previous : Token.t;
  mutable current : Token.t;
  mutable had_error : bool;
  mutable panic_mode : bool;
}

module Op = Opcode

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

let emit_opcode parser opcode = emit_byte parser @@ Op.to_byte opcode

let emit_jump parser opcode =
  emit_opcode parser opcode;
  emit_byte parser '\xFF';
  emit_byte parser '\xFF';
  Vector.length ~vec:parser.chunk.code - 2

let emit_loop parser loop_start =
  emit_opcode parser Op.Loop;
  let offset = Vector.length ~vec:parser.chunk.code - loop_start + 2 in
  if offset > 0xFFFF then error parser "Loop body too large.";
  emit_byte parser @@ Char.chr ((offset lsr 8) land 0xFF);
  emit_byte parser @@ Char.chr (offset land 0xFF)

let patch_jump parser offset =
  let jump = Vector.length ~vec:parser.chunk.code - offset - 2 in
  if jump > 0xFFFF then error parser "Too much code to jump over.";
  Vector.set ~vec:parser.chunk.code ~index:offset
    ~value:(Char.chr @@ ((jump lsr 8) land 0xFF));
  Vector.set ~vec:parser.chunk.code ~index:(offset + 1)
    ~value:(Char.chr @@ (jump land 0xFF))

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
  let value = Chunk.Float (Float.of_string p.previous.content) in
  emit_constant p value

let identifier_constant p (token : Token.t) =
  make_constant p @@ String_arena.get p.arena token.content

let add_local p =
  if List.length p.local_tracker.locals > 255 then
    error p "Too many local variables in function."
  else
    p.local_tracker.locals <-
      Locals.{ depth = -1; name = p.previous } :: p.local_tracker.locals

let identifiers_equal (a : Token.t) (b : Token.t) =
  String.equal a.content b.content

let declare_variable p =
  let rec detect_duplicate : Locals.local list -> unit = function
    | [] -> ()
    | l :: ls ->
        if l.depth <> -1 && l.depth < p.local_tracker.scope_depth then ()
        else if identifiers_equal l.name p.previous then
          error p "Already a variable with this name in this scope."
        else detect_duplicate ls
  in
  if p.local_tracker.scope_depth = 0 then ()
  else (
    detect_duplicate p.local_tracker.locals;
    add_local p)

let parse_variable p msg =
  consume p Token.Identifier msg;
  declare_variable p;
  if p.local_tracker.scope_depth > 0 then Char.chr 0
  else identifier_constant p p.previous

let mark_initialized p =
  match p.local_tracker.locals with
  | [] ->
      failwith
        "Attempted to mark a variable initialized with an empty local stack."
  | x :: xs ->
      p.local_tracker.locals <-
        { x with depth = p.local_tracker.scope_depth } :: xs

let define_variable p global =
  if p.local_tracker.scope_depth > 0 then mark_initialized p
  else (
    emit_opcode p Op.DefineGlobal;
    emit_byte p global)

let end_compiler p =
  emit_opcode p Op.Return;
  if Dbg.on () && not p.had_error then Dbg.disassemble_chunk p.chunk "code"

let begin_scope p = Locals.begin_scope p.local_tracker

let end_scope p =
  Locals.end_scope p.local_tracker;
  let rec pop_locals : Locals.local list -> Locals.local list = function
    | [] -> []
    | l :: ls when l.depth <= p.local_tracker.scope_depth -> l :: ls
    | _ :: ls ->
        emit_opcode p Op.Pop;
        pop_locals ls
  in
  p.local_tracker.locals <- pop_locals p.local_tracker.locals

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
  let rec resolve_local : Locals.local list -> int = function
    | [] -> -1
    | l :: ls ->
        if identifiers_equal tok l.name then List.length ls
        else resolve_local ls
  in
  let arg, get_op, set_op =
    let arg = resolve_local p.local_tracker.locals in
    if arg <> -1 then (Char.chr arg, Op.GetLocal, Op.SetLocal)
    else (identifier_constant p tok, Op.GetGlobal, Op.SetGlobal)
  in
  (match p.current.kind with
  | Token.Equal when can_assign ->
      advance p;
      expression p;
      emit_opcode p set_op
  | _ -> emit_opcode p get_op);
  emit_byte p arg

and variable p can_assign = named_variable p p.previous can_assign

and and_ p _ =
  let end_jump = emit_jump p Op.JumpIfFalse in
  emit_opcode p Op.Pop;
  parse_precedence p Precedence.And;
  patch_jump p end_jump

and or_ p _ =
  let else_jump = emit_jump p Op.JumpIfFalse in
  let end_jump = emit_jump p Op.Jump in
  patch_jump p else_jump;
  emit_opcode p Op.Pop;
  parse_precedence p Precedence.Or;
  patch_jump p end_jump

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
  | Token.And -> make_rule ~infix:(Some and_) ~prec:And ()
  | Token.Or -> make_rule ~infix:(Some or_) ~prec:Or ()
  | _ -> make_rule ()

and print_statement p =
  expression p;
  consume p Token.Semicolon "Expect ';' after value.";
  emit_opcode p Op.Print

and expression_statement p =
  expression p;
  consume p Token.Semicolon "Expect ';' after expression.";
  emit_opcode p Op.Pop

and if_statement p =
  consume p Token.LeftParen "Expect '(' after 'if'.";
  expression p;
  consume p Token.RightParen "Expect ')' after condition.";
  let then_jump = emit_jump p Op.JumpIfFalse in
  emit_opcode p Op.Pop;
  statement p;
  let else_jump = emit_jump p Op.Jump in
  patch_jump p then_jump;
  emit_opcode p Op.Pop;
  (match p.current.kind with
  | Token.Else ->
      advance p;
      statement p
  | _ -> ());
  patch_jump p else_jump

and while_statement p =
  let loop_start = Vector.length ~vec:p.chunk.code in
  consume p Token.LeftParen "Expect '(' after 'while'.";
  expression p;
  consume p Token.RightParen "Expect ')' after condition.";
  let exit_jump = emit_jump p Op.JumpIfFalse in
  emit_opcode p Op.Pop;
  statement p;
  emit_loop p loop_start;
  patch_jump p exit_jump;
  emit_opcode p Op.Pop

and for_statement p =
  begin_scope p;
  consume p Token.LeftParen "Expect '(' after 'for'.";
  (match p.current.kind with
  | Token.Semicolon -> advance p (* No initializer *)
  | Token.Var ->
      advance p;
      var_declaration p
  | _ -> expression_statement p);
  let loop_start = Vector.length ~vec:p.chunk.code in
  let exit_jump =
    match p.current.kind with
    | Token.Semicolon ->
        advance p;
        -1
    | _ ->
        expression p;
        consume p Token.Semicolon "Expect ';'.";
        let ret = emit_jump p Op.JumpIfFalse in
        emit_opcode p Op.Pop;
        ret
  in
  let loop_start =
    match p.current.kind with
    | Token.RightParen ->
        advance p;
        loop_start
    | _ ->
        let body_jump = emit_jump p Op.Jump in
        let increment_start = Vector.length ~vec:p.chunk.code in
        expression p;
        emit_opcode p Op.Pop;
        consume p Token.RightParen "Expect ')' after for clauses.";
        emit_loop p loop_start;
        patch_jump p body_jump;
        increment_start
  in
  statement p;
  emit_loop p loop_start;
  (* NOTE: This could be more idiomatic *)
  if exit_jump <> -1 then (
    patch_jump p exit_jump;
    emit_opcode p Op.Pop);
  end_scope p

and statement p =
  match p.current.kind with
  | Token.Print ->
      advance p;
      print_statement p
  | Token.If ->
      advance p;
      if_statement p
  | Token.While ->
      advance p;
      while_statement p
  | Token.For ->
      advance p;
      for_statement p
  | Token.LeftBrace ->
      advance p;
      begin_scope p;
      block p;
      end_scope p
  | _ -> expression_statement p

and block p =
  match p.current.kind with
  | Token.RightBrace | Token.Eof ->
      consume p Token.RightBrace "Expect '}' after block."
  | _ ->
      declaration p;
      block p

and var_declaration p =
  let global = parse_variable p "Expect variable name." in
  (match p.current.kind with
  | Token.Equal ->
      advance p;
      expression p
  | _ -> emit_opcode p Op.Nil);
  consume p Token.Semicolon "Expect ; after variable declaration";
  define_variable p global

and declaration p =
  match p.current.kind with
  | Token.Var ->
      advance p;
      var_declaration p
  | _ -> statement p

and declarations p =
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
      local_tracker = Locals.make ();
      previous = Token.{ content = ""; kind = Token.Error; line = -1 };
      current = Token.{ content = ""; kind = Token.Error; line = -1 };
      had_error = false;
      panic_mode = false;
    }
  in
  advance p;
  declarations p;
  end_compiler p;
  if p.had_error then Error Err.Compile else Ok (p.chunk, p.arena)
