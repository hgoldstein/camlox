type t = { chunk : Chunk.t; mutable ip : int; mutable stack : Value.t list }

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
  List.iter (fun v ->
      Printf.printf "[ ";
      Value.print v;
      Printf.printf " ]")
  @@ List.rev vm.stack;
  Printf.printf "\n";
  ()

let binary_op vm f =
  let b = pop vm in
  let a = pop vm in
  push vm @@ f a b;
  ()

let rec run vm =
  if Debug.on () then (
    print_stack vm;
    ignore @@ Debug.disassemble_instruction vm.chunk vm.ip);
  let module Op = Chunk.OpCode in
  match Op.of_byte @@ read_byte vm with
  | Ok Op.Return ->
      Value.print (pop vm);
      Printf.printf "\n";
      Ok ()
  | Ok Op.Constant ->
      push vm @@ read_constant vm;
      run vm
  | Ok Op.Negate ->
      (match pop vm with Float f -> push vm @@ Float (f *. -1.0));
      run vm
  | Ok Op.Add ->
      binary_op vm Value.add;
      run vm
  | Ok Op.Subtract ->
      binary_op vm Value.sub;
      run vm
  | Ok Op.Divide ->
      binary_op vm Value.div;
      run vm
  | Ok Op.Multiply ->
      binary_op vm Value.mul;
      run vm
  | Error c ->
      Printf.eprintf "Unknown bytecode instruction %d" (Char.code c);
      exit 1

let interpret_chunk (chunk : Chunk.t) : (unit, Err.t) result =
  run @@ { chunk; ip = 0; stack = [] }

let interpret source =
  match Compiler.compile source with
  | Error e -> Error e
  | Ok chunk -> interpret_chunk chunk
