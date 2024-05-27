type t = { chunk : Chunk.t; mutable ip : int; mutable stack : Value.t list }
type error = CompileError | RuntimeError

let read_byte vm =
  let index = vm.ip in
  vm.ip <- vm.ip + 1;
  Vector.at ~vec:vm.chunk.code ~index

let read_constant vm =
  let index = Char.code @@ read_byte vm in
  Vector.at ~vec:vm.chunk.constants ~index

let push vm value =
  vm.stack <- value :: vm.stack;
  ()

let pop vm =
  match vm.stack with
  | [] -> failwith "unimplemented"
  | v :: vs ->
      vm.stack <- vs;
      v

let print_stack vm =
  Printf.printf "          ";
  List.iter
    (fun v ->
      Printf.printf "[ ";
      Value.print v;
      Printf.printf " ]")
    vm.stack;
  Printf.printf "\n";
  ()

let rec run vm =
  if Debug.trace_execution then (
    ignore @@ print_stack vm;
    ignore @@ Debug.disassemble_instruction vm.chunk vm.ip);
  match Chunk.OpCode.of_byte @@ read_byte vm with
  | Ok Chunk.OpCode.Return ->
      Value.print (pop vm);
      Printf.printf "\n";
      Ok ()
  | Ok Chunk.OpCode.Constant ->
      push vm @@ read_constant vm;
      run vm
  | Ok Chunk.OpCode.Negate ->
      (match pop vm with Float f -> push vm @@ Float (f *. -1.0));
      run vm
  | Error c ->
      Printf.eprintf "Unknown bytecode instruction %d" (Char.code c);
      exit 1

let interpret (chunk : Chunk.t) : (unit, error) result =
  run @@ { chunk; ip = 0; stack = [] }
