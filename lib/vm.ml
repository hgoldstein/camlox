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

let push vm value =
  vm.stack <- value :: vm.stack;
  ()

let pop vm =
  match vm.stack with
  | [] -> failwith "pop empty stack unimplemented"
  | v :: vs ->
      vm.stack <- vs;
      v

let print_stack vm =
  Printf.printf "          ";
  List.iter ~f:(fun v ->
      Printf.printf "[ ";
      Value.print v;
      Printf.printf " ]")
  @@ List.rev vm.stack;
  Printf.printf "\n";
  ()

let reset_stack _ = failwith "reset_stack unimplemented"

let fatal_runtime_error (_ : t) msg =
  Printf.eprintf "FATAL: %s\n" msg;
  exit 2

let runtime_error (vm : t) msg : cycle_result =
  Printf.eprintf "%s\n" msg;
  let line = Vector.at ~vec:vm.chunk.lines ~index:vm.ip in
  Printf.eprintf "[line %d] in script\n" line;
  reset_stack vm

let binary_op vm f =
  let b = pop vm in
  let a = pop vm in
  match (a, b) with
  | Float x, Float y ->
      push vm @@ f x y;
      `Ok
  | _, _ -> runtime_error vm "Operands must be numbers."

let rec run vm : (unit, Err.t) result =
  if Dbg.on () then (
    print_stack vm;
    ignore @@ Dbg.disassemble_instruction vm.chunk vm.ip);
  let module Op = Chunk.OpCode in
  let cycle =
    match Op.of_byte @@ read_byte vm with
    | Ok Op.Return ->
        Value.print (pop vm);
        Printf.printf "\n";
        `Return
    | Ok Op.Constant ->
        push vm @@ read_constant vm;
        `Ok
    | Ok Op.Negate -> (
        match vm.stack with
        | [] -> fatal_runtime_error vm "Not enough arguments on stack."
        | Value.Float f :: _ ->
            let new_v = Value.Float (f *. -1.0) in
            let _ = pop vm in
            push vm new_v;
            `Ok
        | _ -> runtime_error vm "operand must be a number")
    | Ok Op.Add -> binary_op vm (fun a b -> Float (a +. b))
    | Ok Op.Subtract -> binary_op vm (fun a b -> Float (a -. b))
    | Ok Op.Divide -> binary_op vm (fun a b -> Float (a /. b))
    | Ok Op.Multiply -> binary_op vm (fun a b -> Float (a *. b))
    | Ok Op.Nil ->
        push vm Value.Nil;
        `Ok
    | Ok Op.True ->
        push vm (Value.Bool true);
        `Ok
    | Ok Op.False ->
        push vm (Value.Bool false);
        `Ok
    | Ok Op.Not ->
        push vm @@ Value.Bool (Value.is_falsey @@ pop vm);
        `Ok
    | Ok Op.Equal ->
        let b = pop vm in
        let a = pop vm in
        push vm @@ Value.Bool (Value.equal a b);
        `Ok
    | Ok Op.Greater ->
        binary_op vm (fun a b -> Value.Bool (Float.compare a b > 0))
    | Ok Op.Less -> binary_op vm (fun a b -> Value.Bool (Float.compare a b < 0))
    | Error c ->
        Printf.eprintf "Unknown bytecode instruction %d" (Char.to_int c);
        exit 1
  in
  match cycle with `Error e -> Error e | `Ok -> run vm | `Return -> Ok ()

let interpret_chunk (chunk : Chunk.t) : (unit, Err.t) result =
  run @@ { chunk; ip = 0; stack = [] }

let interpret source =
  match Compiler.compile source with
  | Error e -> Error e
  | Ok chunk -> interpret_chunk chunk
