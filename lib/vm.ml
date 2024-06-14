open Core

type call_frame = {
  function_ : Chunk.function_;
  mutable ip : int;
  mutable stack : Chunk.value list;
}

type t = {
  strings : String_arena.t;
  globals : Chunk.value Table.t;
  frames : call_frame list;
}

type cycle_result = [ `Ok | `Error of Err.t | `Return ]

let hd vm = List.hd_exn vm.frames

let read_byte vm =
  let frame = hd vm in
  let index = frame.ip in
  frame.ip <- frame.ip + 1;
  Vector.at ~vec:frame.function_.chunk.code ~index

let read_short vm =
  let hi_byte = read_byte vm in
  let lo_byte = read_byte vm in
  (Char.to_int hi_byte lsl 8) lor Char.to_int lo_byte

let read_constant vm =
  let frame = hd vm in
  let index = Char.to_int @@ read_byte vm in
  Vector.at ~vec:frame.function_.chunk.constants ~index

let fatal_runtime_error (_ : t) msg =
  Printf.eprintf "FATAL: %s\n" msg;
  exit 2

let read_string vm =
  match read_constant vm with
  | Chunk.Object (Chunk.String s) -> s
  | v ->
      fatal_runtime_error vm
        (Printf.sprintf "Expected string constant, got: %s" (Chunk.show_value v))

let print_stack vm =
  let frame = hd vm in
  Printf.printf "          ";
  List.iter ~f:(fun v ->
      Printf.printf "[ ";
      Chunk.print_value v;
      Printf.printf " ]")
  @@ List.rev frame.stack;
  Printf.printf "\n";
  ()

let runtime_error (vm : t) msg : cycle_result =
  let frame = hd vm in
  Printf.eprintf "%s\n" msg;
  let line = Vector.at ~vec:frame.function_.chunk.lines ~index:frame.ip in
  Printf.eprintf "[line %d] in script\n" line;
  `Error Err.Runtime

let rec run vm : (unit, Err.t) result =
  let frame = hd vm in
  if Dbg.on () then (
    print_stack vm;
    ignore @@ Dbg.disassemble_instruction frame.function_.chunk frame.ip);
  let module Op = Opcode in
  let flip wrt i = List.length wrt - (Char.to_int i + 1) in
  let res, stack =
    match (Op.of_byte @@ read_byte vm, frame.stack) with
    | Op.Return, vs -> (`Return, vs)
    | Op.Constant, vs -> (`Ok, read_constant vm :: vs)
    | Op.Negate, Float f :: vs -> (`Ok, Chunk.Float (f *. -1.0) :: vs)
    | Op.Add, Float b :: Float a :: stack -> (`Ok, Float (a +. b) :: stack)
    | Op.Add, Object (String b) :: Object (String a) :: stack ->
        let a_chars = String_val.get a in
        let b_chars = String_val.get b in
        (`Ok, String_arena.get vm.strings (a_chars ^ b_chars) :: stack)
    | Op.Subtract, Float b :: Float a :: stack -> (`Ok, Float (a -. b) :: stack)
    | Op.Divide, Float b :: Float a :: stack -> (`Ok, Float (a /. b) :: stack)
    | Op.Multiply, Float b :: Float a :: stack -> (`Ok, Float (a *. b) :: stack)
    | Op.Nil, stack -> (`Ok, Chunk.Nil :: stack)
    | Op.True, stack -> (`Ok, Chunk.Bool true :: stack)
    | Op.False, stack -> (`Ok, Chunk.Bool false :: stack)
    | Op.Not, v :: vs -> (`Ok, Chunk.Bool (Chunk.is_falsey v) :: vs)
    | Op.Equal, b :: a :: stack -> (`Ok, Chunk.Bool (Chunk.equal a b) :: stack)
    | Op.Greater, Float b :: Float a :: stack ->
        (`Ok, Chunk.Bool (Float.compare a b > 0) :: stack)
    | Op.Less, Float b :: Float a :: stack ->
        (`Ok, Chunk.Bool (Float.compare a b < 0) :: stack)
    | Op.Print, v :: stack ->
        Chunk.print_line v;
        (`Ok, stack)
    | Op.Pop, _ :: stack -> (`Ok, stack)
    | Op.DefineGlobal, value :: stack ->
        let name = read_string vm in
        Table.set vm.globals name value;
        (`Ok, stack)
    | Op.GetGlobal, stack -> (
        let name = read_string vm in
        match Table.find vm.globals name with
        | None ->
            ( runtime_error vm
                (Printf.sprintf "Undefined variable '%s'." (String_val.get name)),
              stack )
        | Some v -> (`Ok, v :: stack))
    | Op.SetGlobal, v :: stack ->
        let name = read_string vm in
        if not @@ Table.mem vm.globals name then
          ( runtime_error vm
              (Printf.sprintf "Undefined variable '%s'." (String_val.get name)),
            v :: stack )
        else (
          Table.set vm.globals name v;
          (`Ok, v :: stack))
    | Op.GetLocal, stack ->
        let slot = flip stack @@ read_byte vm in
        (`Ok, List.nth_exn stack slot :: stack)
    | Op.SetLocal, (top :: _ as stack) ->
        let slot = flip stack @@ read_byte vm in
        let f idx v = if idx = slot then top else v in
        (`Ok, List.mapi stack ~f)
    | Op.JumpIfFalse, (top :: _ as stack) ->
        let offset = read_short vm in
        if Chunk.is_falsey top then frame.ip <- frame.ip + offset;
        (`Ok, stack)
    | Op.Jump, stack ->
        let offset = read_short vm in
        frame.ip <- frame.ip + offset;
        (`Ok, stack)
    | Op.Loop, stack ->
        let offset = read_short vm in
        frame.ip <- frame.ip - offset;
        (`Ok, stack)
    (* Error cases *)
    | ( ( Op.Negate | Op.Not | Op.Print | Op.Pop | Op.DefineGlobal
        | Op.SetGlobal | Op.SetLocal | Op.JumpIfFalse ),
        [] ) ->
        fatal_runtime_error vm "Not enough arguments on stack."
    | Op.Negate, stack -> (runtime_error vm "operand must be a number", stack)
    | ( ( Op.Add | Op.Subtract | Op.Divide | Op.Multiply | Op.Equal | Op.Greater
        | Op.Less ),
        ([] | [ _ ]) ) ->
        fatal_runtime_error vm "Not enough arguments on stack."
    | Op.Add, vs ->
        (runtime_error vm "Operands must be two numbers or two strings.", vs)
    | (Op.Less | Op.Greater), vs ->
        (runtime_error vm "operands must be numbers", vs)
    | (Op.Subtract | Op.Divide | Op.Multiply), vs ->
        (runtime_error vm "operands must be two numbers", vs)
  in
  frame.stack <- stack;
  match res with `Error e -> Error e | `Ok -> run vm | `Return -> Ok ()

let interpret_function (function_ : Chunk.function_) (strings : String_arena.t)
    : (unit, Err.t) result =
  run
  @@ {
       strings;
       globals = Table.make ();
       frames =
         [
           {
             function_;
             ip = 0;
             stack = [ Chunk.Object (Chunk.Function function_) ];
           };
         ];
     }

let interpret source =
  match Compiler.compile source with
  | Error e -> Error e
  | Ok (fn, arena) -> interpret_function fn arena
