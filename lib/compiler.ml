open Core

type upvalue = { is_local : bool; index : char }
type class_compiler = { mutable has_superclass : bool }
type function_kind = [ `Function | `Script | `Method | `Initializer ]

type compiler = {
  output : Chunk.function_;
  local_tracker : Locals.t;
  encloses : compiler option; [@warning "-69"]
  upvalues : upvalue Vector.t;
  kind : function_kind;
}

let make_compiler ~kind ~encloses ~arity ~name : compiler =
  let local_tracker = Locals.make () in
  (* Initialize the local tracker with a "hidden" local, representing the
   * function objects we push onto the stack.
   *)
  local_tracker.locals <-
    (match kind with
    | `Function | `Script ->
        [
          { name = { content = ""; kind = Token.Error; line = -1 }; depth = 0 };
        ]
    | `Method | `Initializer ->
        [
          {
            name = { content = "this"; kind = Token.Error; line = -1 };
            depth = 0;
          };
        ]);
  {
    output = { chunk = Chunk.make (); arity; name; upvalue_count = 0 };
    local_tracker;
    encloses;
    upvalues = Vector.empty ();
    kind;
  }

type parser = {
  scanner : Scanner.t;
  arena : String_arena.t;
  mutable compiler : compiler;
  mutable class_compiler : class_compiler list;
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
  | k' when Token.equal k k' ->
      advance parser;
      ()
  | _ -> error_at_current parser msg

let code_size (parser : parser) =
  Vector.length parser.compiler.output.chunk.code

let emit_byte (parser : parser) byte =
  Chunk.write_byte ~chunk:parser.compiler.output.chunk ~byte
    ~line:parser.previous.line

let emit_opcode parser opcode = emit_byte parser @@ Op.to_byte opcode

let emit_jump parser opcode =
  emit_opcode parser opcode;
  emit_byte parser '\xFF';
  emit_byte parser '\xFF';
  code_size parser - 2

let emit_loop parser loop_start =
  emit_opcode parser Op.Loop;
  let offset = code_size parser - loop_start + 2 in
  if offset > 0xFFFF then error parser "Loop body too large.";
  emit_byte parser @@ Char.of_int_exn ((offset lsr 8) land 0xFF);
  emit_byte parser @@ Char.of_int_exn (offset land 0xFF)

let patch_jump parser offset =
  let jump = code_size parser - offset - 2 in
  if jump > 0xFFFF then error parser "Too much code to jump over.";
  Vector.set parser.compiler.output.chunk.code ~index:offset
    ~value:(Char.of_int_exn @@ ((jump lsr 8) land 0xFF));
  Vector.set parser.compiler.output.chunk.code ~index:(offset + 1)
    ~value:(Char.of_int_exn @@ (jump land 0xFF))

let make_constant (p : parser) value =
  let c = Chunk.add_constant ~chunk:p.compiler.output.chunk ~value in
  if c > 0xFF then (
    error p "Too many constants in one chunk.";
    '\x00')
  else Char.of_int_exn c

let emit_constant (p : parser) value =
  let c = make_constant p value in
  emit_opcode p Op.Constant;
  emit_byte p c

let emit_closure (p : parser) value =
  let c = make_constant p value in
  emit_opcode p Op.Closure;
  emit_byte p c

let number p _ =
  let value = Chunk.Float (Float.of_string p.previous.content) in
  emit_constant p value

let identifier_constant (p : parser) (token : Token.t) =
  make_constant p @@ String_arena.value p.arena token.content

let add_local p name =
  if List.length p.compiler.local_tracker.locals > 255 then
    error p "Too many local variables in function."
  else
    p.compiler.local_tracker.locals <-
      Locals.{ depth = -1; name } :: p.compiler.local_tracker.locals

let identifiers_equal (a : Token.t) (b : Token.t) =
  String.equal a.content b.content

let declare_variable p =
  let rec detect_duplicate : Locals.local list -> unit = function
    | [] -> ()
    | l :: ls ->
        if l.depth <> -1 && l.depth < p.compiler.local_tracker.scope_depth then
          ()
        else if identifiers_equal l.name p.previous then
          error p "Already a variable with this name in this scope."
        else detect_duplicate ls
  in
  if p.compiler.local_tracker.scope_depth = 0 then ()
  else (
    detect_duplicate p.compiler.local_tracker.locals;
    add_local p p.previous)

let parse_variable p msg =
  consume p Token.Identifier msg;
  declare_variable p;
  if p.compiler.local_tracker.scope_depth > 0 then '\x00'
  else identifier_constant p p.previous

let mark_initialized c =
  if c.local_tracker.scope_depth <> 0 then
    match c.local_tracker.locals with
    | [] ->
        failwith
          "Attempted to mark a variable initialized with an empty local stack."
    | x :: xs ->
        c.local_tracker.locals <-
          { x with depth = c.local_tracker.scope_depth } :: xs

let define_variable p global =
  if p.compiler.local_tracker.scope_depth > 0 then mark_initialized p.compiler
  else (
    emit_opcode p Op.DefineGlobal;
    emit_byte p global)

let emit_return p =
  (* For returns, if we are compiling an initializer then we want to return
   * `this`, which is stored in the 0th local slot.
   *)
  (match p.compiler.kind with
  | `Initializer ->
      emit_opcode p Op.GetLocal;
      emit_byte p '\x00'
  | `Function | `Script | `Method -> emit_opcode p Op.Nil);
  emit_opcode p Op.Return

let end_compiler p =
  emit_return p;
  (if Dbg.on () && not p.had_error then
     let name =
       match p.compiler.output.name with
       | None -> "<script>"
       | Some f -> String_val.get f
     in
     Dbg.disassemble_chunk p.compiler.output.chunk name);
  p.compiler.output

let begin_scope p = Locals.begin_scope p.compiler.local_tracker

let end_scope p =
  Locals.end_scope p.compiler.local_tracker;
  let rec pop_locals : Locals.local list -> Locals.local list = function
    | [] -> []
    | l :: ls when l.depth <= p.compiler.local_tracker.scope_depth -> l :: ls
    | _ :: ls ->
        emit_opcode p Op.Pop;
        pop_locals ls
  in
  p.compiler.local_tracker.locals <- pop_locals p.compiler.local_tracker.locals

let synthetic_token content : Token.t =
  { content; kind = Token.Error; line = 0 }

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
  @@ String_arena.value parser.arena
  @@ String.sub v ~pos:1 ~len:(String.length v - 2)

and add_upvalue p compiler uv_index is_local =
  let upvalue_count = compiler.output.upvalue_count in
  let rec find_existing_upvalue i =
    if i < 0 then None
    else
      let uv = Vector.at compiler.upvalues ~index:i in
      if Char.equal uv.index uv_index && Bool.equal uv.is_local is_local then
        Some i
      else find_existing_upvalue (i - 1)
  in
  match find_existing_upvalue (upvalue_count - 1) with
  | None ->
      if upvalue_count > 0xFF then (
        error p "Too many closure variables in function.";
        0)
      else (
        Vector.set_extend compiler.upvalues ~index:upvalue_count
          ~value:{ is_local; index = uv_index };
        compiler.output.upvalue_count <- upvalue_count + 1;
        upvalue_count)
  | Some existing_uv_index -> existing_uv_index

and named_variable p tok can_assign =
  let rec resolve_local : Locals.local list -> int option = function
    | [] -> None
    | l :: ls ->
        if identifiers_equal tok l.name then (
          if l.depth = -1 then
            error p "Can't read local variable in its own initializer.";
          Some (List.length ls))
        else resolve_local ls
  in
  let rec resolve_upvalue compiler =
    let open Core in
    match compiler.encloses with
    | None -> None
    | Some enclosing -> (
        match resolve_local enclosing.local_tracker.locals with
        | Some local ->
            Some (add_upvalue p compiler (Char.of_int_exn local) true)
        | None -> (
            match resolve_upvalue enclosing with
            | None -> None
            | Some uv ->
                Some (add_upvalue p compiler (Char.of_int_exn uv) false)))
  in
  let arg, get_op, set_op =
    match resolve_local p.compiler.local_tracker.locals with
    | Some arg -> (Char.of_int_exn arg, Op.GetLocal, Op.SetLocal)
    | None -> (
        match resolve_upvalue p.compiler with
        | Some arg -> (Char.of_int_exn arg, Op.GetUpvalue, Op.SetUpvalue)
        | None -> (identifier_constant p tok, Op.GetGlobal, Op.SetGlobal))
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

and dot p can_assign =
  consume p Token.Identifier "Expect property name after '.'.";
  let name = identifier_constant p p.previous in
  match p.current.kind with
  | Token.Equal when can_assign ->
      advance p;
      expression p;
      emit_opcode p Op.SetProperty;
      emit_byte p name
  | Token.LeftParen ->
      advance p;
      let arg_count = argument_list p in
      emit_opcode p Op.Invoke;
      emit_byte p name;
      emit_byte p arg_count
  | _ ->
      emit_opcode p Op.GetProperty;
      emit_byte p name

and argument_list p =
  let rec gather_args count =
    expression p;
    if count = 256 then error p "Can't have more than 255 arguments.";
    match p.current.kind with
    | Token.Comma ->
        advance p;
        gather_args (count + 1)
    | _ -> count
  in
  let count =
    match p.current.kind with
    | Token.RightParen -> 0
    | _ -> gather_args 1 (* We know at least one arg will follow *)
  in
  consume p Token.RightParen "Expect ')' after arguments.";
  Char.of_int_exn (count land 0xFF)

and call p _ =
  let arg_count = argument_list p in
  emit_opcode p Op.Call;
  emit_byte p arg_count

and this_ p _ =
  if List.is_empty p.class_compiler then
    error p "Can't use 'this' outside of a class."
  else variable p false

and super_ p _ =
  (match p.class_compiler with
  | [] -> error p "Can't use 'super' outside of a class."
  | cc :: _ when not cc.has_superclass ->
      error p "Can't use 'super' in a class with no superclass."
  | _ -> ());
  consume p Token.Dot "Expect '.' after 'super'.";
  consume p Token.Identifier "Expect superclass method name.";
  let name = identifier_constant p p.previous in
  named_variable p (synthetic_token "this") false;
  match p.current.kind with
  | Token.LeftParen ->
      advance p;
      let arg_count = argument_list p in
      named_variable p (synthetic_token "super") false;
      emit_opcode p Op.SuperInvoke;
      emit_byte p name;
      emit_byte p arg_count
  | _ ->
      named_variable p (synthetic_token "super") false;
      emit_opcode p Op.GetSuper;
      emit_byte p name

and get_rule : Token.kind -> rule =
  let open Precedence in
  let make_rule ?(prefix = None) ?(infix = None) ?(prec = NoPrec) () =
    { prefix; infix; precedence = prec }
  in
  function
  | Token.LeftParen ->
      make_rule ~prefix:(Some grouping) ~infix:(Some call) ~prec:Call ()
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
  | Token.Dot -> make_rule ~infix:(Some dot) ~prec:Call ()
  | Token.This -> make_rule ~prefix:(Some this_) ()
  | Token.Super -> make_rule ~prefix:(Some super_) ()
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
  let loop_start = code_size p in
  consume p Token.LeftParen "Expect '(' after 'while'.";
  expression p;
  consume p Token.RightParen "Expect ')' after condition.";
  let exit_jump = emit_jump p Op.JumpIfFalse in
  emit_opcode p Op.Pop;
  statement p;
  emit_loop p loop_start;
  patch_jump p exit_jump;
  emit_opcode p Op.Pop

and for_statement (p : parser) =
  begin_scope p;
  consume p Token.LeftParen "Expect '(' after 'for'.";
  (match p.current.kind with
  | Token.Semicolon -> advance p (* No initializer *)
  | Token.Var ->
      advance p;
      var_declaration p
  | _ -> expression_statement p);
  let loop_start = code_size p in
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
        let increment_start = code_size p in
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

and return_statement p =
  if Option.is_none p.compiler.output.name then
    error p "Can't return from top-level code.";
  match p.current.kind with
  | Token.Semicolon ->
      advance p;
      emit_return p
  | _ ->
      (match p.compiler.kind with
      | `Initializer -> error p "Can't return a value from an initializer."
      | _ -> ());
      expression p;
      consume p Token.Semicolon "Expect ';' after return value.";
      emit_opcode p Op.Return

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
  | Token.Return ->
      advance p;
      return_statement p
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
  let rec declarations p =
    match p.current.kind with
    | Token.Eof | Token.RightBrace -> ()
    | _ ->
        declaration p;
        declarations p
  in
  declarations p;
  consume p Token.RightBrace "Expect '}' after block."

and var_declaration p =
  let global = parse_variable p "Expect variable name." in
  (match p.current.kind with
  | Token.Equal ->
      advance p;
      expression p
  | _ -> emit_opcode p Op.Nil);
  consume p Token.Semicolon "Expect ; after variable declaration";
  define_variable p global

and function_ ~kind p =
  let compiler =
    make_compiler ~kind ~encloses:(Some p.compiler) ~arity:0
      ~name:(Some (String_arena.get p.arena p.previous.content))
  in
  let old_compiler = p.compiler in
  p.compiler <- compiler;
  let rec collect_arguments arity =
    let arity = arity + 1 in
    if arity > 255 then
      error_at_current p "Can't have more than 255 parameters.";
    let c = parse_variable p "Expect parameter name." in
    define_variable p c;
    match p.current.kind with
    | Token.Comma ->
        advance p;
        collect_arguments arity
    | _ -> arity
  in
  begin_scope p;
  consume p Token.LeftParen "Expect '(' after function name.";
  let arity =
    match p.current.kind with Token.RightParen -> 0 | _ -> collect_arguments 0
  in
  consume p Token.RightParen "Expect ')' after parameters.";
  consume p Token.LeftBrace "Expect '{' before function body.";
  block p;
  let fn = end_compiler p in
  p.compiler <- old_compiler;
  emit_closure p @@ Chunk.Function { fn with arity };
  for i = 0 to fn.upvalue_count - 1 do
    let uv = Vector.at compiler.upvalues ~index:i in
    emit_byte p @@ if uv.is_local then '\x01' else '\x00';
    emit_byte p uv.index
  done

and fun_declaration p =
  let global = parse_variable p "Expect function name." in
  mark_initialized p.compiler;
  function_ p ~kind:`Function;
  define_variable p global

and method_ p =
  consume p Token.Identifier "Expect method name.";
  let const = identifier_constant p p.previous in
  let kind =
    match p.previous.content with "init" -> `Initializer | _ -> `Method
  in
  function_ p ~kind;
  emit_opcode p Op.Method;
  emit_byte p const

and class_declaration p =
  consume p Token.Identifier "Expect class name.";
  let class_name = p.previous in
  let name_const = identifier_constant p p.previous in
  declare_variable p;
  emit_opcode p Op.Class;
  emit_byte p name_const;
  define_variable p name_const;
  p.class_compiler <- { has_superclass = false } :: p.class_compiler;
  (match p.current.kind with
  | Token.Less ->
      advance p;
      consume p Token.Identifier "Expect superclass name.";
      variable p false;
      if identifiers_equal class_name p.previous then
        error p "A class can't inherit from itself.";
      begin_scope p;
      add_local p @@ synthetic_token "super";
      define_variable p '\x00';
      named_variable p class_name false;
      emit_opcode p Op.Inherit;
      (List.hd_exn p.class_compiler).has_superclass <- true
  | _ -> ());
  named_variable p class_name false;
  consume p Token.LeftBrace "Expect '{' before class body.";
  let rec collect_methods p =
    match p.current.kind with
    | Token.Eof | Token.RightBrace -> ()
    | _ ->
        method_ p;
        collect_methods p
  in
  collect_methods p;
  consume p Token.RightBrace "Expect '}' after class body.";
  emit_opcode p Op.Pop;
  if (List.hd_exn p.class_compiler).has_superclass then end_scope p;
  p.class_compiler <- List.tl_exn p.class_compiler

and declaration p =
  (match p.current.kind with
  | Token.Var ->
      advance p;
      var_declaration p
  | Token.Fun ->
      advance p;
      fun_declaration p
  | Token.Class ->
      advance p;
      class_declaration p
  | _ -> statement p);
  if p.panic_mode then synchronize p

let compile (source : string) (arena : String_arena.t) :
    (Chunk.function_, Err.t) result =
  let rec declarations p =
    match p.current.kind with
    | Token.Eof ->
        advance p;
        ()
    | _ ->
        declaration p;
        declarations p
  in
  let p : parser =
    {
      scanner = Scanner.of_string source;
      compiler = make_compiler ~arity:0 ~kind:`Script ~name:None ~encloses:None;
      class_compiler = [];
      arena;
      previous = Token.{ content = ""; kind = Token.Error; line = -1 };
      current = Token.{ content = ""; kind = Token.Error; line = -1 };
      had_error = false;
      panic_mode = false;
    }
  in
  advance p;
  declarations p;
  let output = end_compiler p in
  if p.had_error then Error Err.Compile else Ok output
