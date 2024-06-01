open Core

type t = { chunk : Chunk.t; mutable ip : int; mutable stack : Value.t list }
type cycle_result = [ `Ok | `Error of Err.t | `Return ]

let read_byte vm =
  let index = vm.ip in
  vm.ip <- vm.ip + 1;
  Vector.at ~vec:vm.chunk.code ~index

let read_constant vm =
  let index = Char.to_int @@ read_byte vm in
  Vector.at ~vec:vm.chunk.constants ~index

let print_stack vm =
  Printf.printf "          ";
  List.iter ~f:(fun v ->
      Printf.printf "[ ";
      Value.print v;
      Printf.printf " ]")
  @@ List.rev vm.stack;
  Printf.printf "\n";
  ()

let fatal_runtime_error (_ : t) msg =
  Printf.eprintf "FATAL: %s\n" msg;
  exit 2

let runtime_error (vm : t) msg : cycle_result =
  Printf.eprintf "%s\n" msg;
  let line = Vector.at ~vec:vm.chunk.lines ~index:vm.ip in
  Printf.eprintf "[line %d] in script\n" line;
  `Error Err.Runtime

let rec run vm : (unit, Err.t) result =
  if Dbg.on () then (
    print_stack vm;
    ignore @@ Dbg.disassemble_instruction vm.chunk vm.ip);
  let module Op = Chunk.OpCode in
  let opcode =
    match Op.of_byte @@ read_byte vm with
    | Ok op -> op
    | Error c ->
        Printf.eprintf "Unknown bytecode instruction %d" (Char.to_int c);
        exit 1
  in
  let res, stack =
    match (opcode, vm.stack) with
    | Op.Return, v :: vs ->
        Value.print v;
        Printf.printf "\n";
        (`Return, vs)
    | Op.Constant, vs -> (`Ok, read_constant vm :: vs)
    | Op.Negate, Float f :: vs -> (`Ok, Value.Float (f *. -1.0) :: vs)
    | Op.Add, Float b :: Float a :: stack -> (`Ok, Float (a +. b) :: stack)
    | ( Op.Add,
        Object (String { chars = chars_b })
        :: Object (String { chars = chars_a })
        :: stack ) ->
        (`Ok, Object (String { chars = chars_a ^ chars_b }) :: stack)
    | Op.Subtract, Float b :: Float a :: stack -> (`Ok, Float (a -. b) :: stack)
    | Op.Divide, Float b :: Float a :: stack -> (`Ok, Float (a /. b) :: stack)
    | Op.Multiply, Float b :: Float a :: stack -> (`Ok, Float (a *. b) :: stack)
    | Op.Nil, stack -> (`Ok, Value.Nil :: stack)
    | Op.True, stack -> (`Ok, Value.Bool true :: stack)
    | Op.False, stack -> (`Ok, Value.Bool false :: stack)
    | Op.Not, v :: vs -> (`Ok, Value.Bool (Value.is_falsey v) :: vs)
    | Op.Equal, b :: a :: stack -> (`Ok, Value.Bool (Value.equal a b) :: stack)
    | Op.Greater, Float b :: Float a :: stack ->
        (`Ok, Value.Bool (Float.compare a b > 0) :: stack)
    | Op.Less, Float b :: Float a :: stack ->
        (`Ok, Value.Bool (Float.compare a b < 0) :: stack)
    | (Op.Negate | Op.Not | Op.Return), [] ->
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
  vm.stack <- stack;
  match res with `Error e -> Error e | `Ok -> run vm | `Return -> Ok ()

let interpret_chunk (chunk : Chunk.t) : (unit, Err.t) result =
  run @@ { chunk; ip = 0; stack = [] }

let interpret source =
  match Compiler.compile source with
  | Error e -> Error e
  | Ok chunk -> interpret_chunk chunk
