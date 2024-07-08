open Core

type call_frame = {
  closure : Chunk.closure;
  mutable stack : Chunk.value list;
  mutable ip : int;
}

type t = {
  strings : String_arena.t;
  globals : Chunk.value Table.t;
  mutable frame : call_frame;
  mutable frame_stack : call_frame list;
}

type cycle_result = [ `Ok | `Error of Err.t | `End ]

let read_byte vm =
  let index = vm.frame.ip in
  vm.frame.ip <- vm.frame.ip + 1;
  Vector.at ~vec:vm.frame.closure.function_.chunk.code ~index

let read_short vm =
  let hi_byte = read_byte vm in
  let lo_byte = read_byte vm in
  (Char.to_int hi_byte lsl 8) lor Char.to_int lo_byte

let read_constant vm =
  let index = Char.to_int @@ read_byte vm in
  Vector.at ~vec:vm.frame.closure.function_.chunk.constants ~index

let fatal_runtime_error (_ : t) msg =
  Printf.eprintf "FATAL: %s\n" msg;
  exit 2

let define_native vm name defn =
  let name = String_arena.get vm.strings name in
  Table.set vm.globals name @@ ref (Chunk.Native defn)

let read_string vm =
  match read_constant vm with
  | Chunk.String s -> s
  | v ->
      fatal_runtime_error vm
        (Printf.sprintf "Expected string constant, got: %s" (Chunk.show_value v))

let print_stack vm =
  Printf.printf "          ";
  List.iter ~f:(fun v ->
      Printf.printf "[ ";
      Chunk.print_value !v;
      Printf.printf " ]")
  @@ List.rev vm.frame.stack;
  Printf.printf "\n";
  ()

let runtime_error (vm : t) msg : cycle_result =
  let print_frame frame =
    Printf.eprintf "[line %d] in "
      (Vector.at ~vec:frame.closure.function_.chunk.lines ~index:(frame.ip - 1));
    match frame.closure.function_.name with
    | None -> Printf.eprintf "script\n"
    | Some s -> Printf.eprintf "%s()\n" (String_val.get s)
  in
  let print_frames = List.iter ~f:print_frame in
  Printf.eprintf "%s\n" msg;
  print_frame vm.frame;
  print_frames vm.frame_stack;
  `Error Err.Runtime

let rec run (vm : t) : (unit, Err.t) result =
  if Dbg.on () then (
    print_stack vm;
    ignore
    @@ Dbg.disassemble_instruction vm.frame.closure.function_.chunk vm.frame.ip);
  let module Op = Opcode in
  let nth_from_back wrt i = List.length wrt - Char.to_int i - 1 in
  let push_frame vm frame =
    vm.frame_stack <- vm.frame :: vm.frame_stack;
    vm.frame <- frame
  in
  let call_value vm stack callee arg_count =
    match !callee with
    | Chunk.Native fn ->
        let new_stack, old_stack = List.split_n stack (arg_count + 1) in
        let result = fn arg_count new_stack in
        (`Ok, result :: old_stack)
    | Chunk.Closure closure ->
        if arg_count <> closure.function_.arity then
          ( runtime_error vm
            @@ Printf.sprintf "Expected %d arguments but got %d"
                 closure.function_.arity arg_count,
            stack )
        else
          let new_stack, old_stack = List.split_n stack (arg_count + 1) in
          vm.frame.stack <- old_stack;
          push_frame vm
          @@ {
               ip = 0;
               closure;
               stack = [];
               (* We take every argument *and* one more for the function object*)
             };
          (`Ok, new_stack)
    | Chunk.Class class_ ->
        let instance = Chunk.instance { class_; fields = Table.make () } in
        let _, old_stack = List.split_n stack (arg_count + 1) in
        (`Ok, instance :: old_stack)
    | _ -> (runtime_error vm "Can only call functions and classes.", stack)
  in
  let res, stack =
    match (Op.of_byte @@ read_byte vm, vm.frame.stack) with
    | Op.Return, result :: _ -> (
        match vm.frame_stack with
        | [] -> (`End, []) (* End of execution *)
        | f :: fs ->
            vm.frame <- f;
            vm.frame_stack <- fs;
            (`Ok, result :: vm.frame.stack))
    | Op.Constant, vs -> (`Ok, ref (read_constant vm) :: vs)
    | Op.Negate, { contents = Float f } :: vs ->
        (`Ok, Chunk.float (f *. -1.0) :: vs)
    | Op.Add, { contents = Float b } :: { contents = Float a } :: stack ->
        (`Ok, Chunk.float (a +. b) :: stack)
    | Op.Add, { contents = String b } :: { contents = String a } :: stack ->
        let a_chars = String_val.get a in
        let b_chars = String_val.get b in
        (`Ok, ref (String_arena.value vm.strings (a_chars ^ b_chars)) :: stack)
    | Op.Subtract, { contents = Float b } :: { contents = Float a } :: stack ->
        (`Ok, Chunk.float (a -. b) :: stack)
    | Op.Divide, { contents = Float b } :: { contents = Float a } :: stack ->
        (`Ok, Chunk.float (a /. b) :: stack)
    | Op.Multiply, { contents = Float b } :: { contents = Float a } :: stack ->
        (`Ok, Chunk.float (a *. b) :: stack)
    | Op.Nil, stack -> (`Ok, ref Chunk.Nil :: stack)
    | Op.True, stack -> (`Ok, Chunk.bool true :: stack)
    | Op.False, stack -> (`Ok, Chunk.bool false :: stack)
    | Op.Not, v :: vs -> (`Ok, Chunk.bool (Chunk.is_falsey v) :: vs)
    | Op.Equal, b :: a :: stack -> (`Ok, Chunk.bool (Chunk.equal a b) :: stack)
    | Op.Greater, { contents = Float b } :: { contents = Float a } :: stack ->
        (`Ok, Chunk.bool (Float.compare a b > 0) :: stack)
    | Op.Less, { contents = Float b } :: { contents = Float a } :: stack ->
        (`Ok, Chunk.bool (Float.compare a b < 0) :: stack)
    | Op.Print, v :: stack ->
        Chunk.print_line !v;
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
        let slot = nth_from_back stack @@ read_byte vm in
        (`Ok, List.nth_exn stack slot :: stack)
    | Op.SetLocal, (top :: _ as stack) ->
        let slot = nth_from_back stack @@ read_byte vm in
        let f idx v = if idx = slot then top else v in
        (`Ok, List.mapi stack ~f)
    | Op.JumpIfFalse, (top :: _ as stack) ->
        let offset = read_short vm in
        if Chunk.is_falsey top then vm.frame.ip <- vm.frame.ip + offset;
        (`Ok, stack)
    | Op.Jump, stack ->
        let offset = read_short vm in
        vm.frame.ip <- vm.frame.ip + offset;
        (`Ok, stack)
    | Op.Loop, stack ->
        let offset = read_short vm in
        vm.frame.ip <- vm.frame.ip - offset;
        (`Ok, stack)
    | Op.Call, stack ->
        let arg_count = Char.to_int @@ read_byte vm in
        (* This is meant to be `peek(n)`, which for a stack is equivalent to
         * `List.nth`
         *)
        let callee = List.nth_exn stack arg_count in
        call_value vm stack callee arg_count
    | Op.Closure, stack -> (
        match read_constant vm with
        | Chunk.Function function_ ->
            let upvalues =
              Array.create ~len:function_.upvalue_count (ref Chunk.Nil)
            in
            let rec collect_upvals i =
              if i < Array.length upvalues then (
                let is_local = read_byte vm in
                let index = read_byte vm in
                let uv =
                  match is_local with
                  | '\x00' ->
                      Array.get vm.frame.closure.upvalues (Char.to_int index)
                  | _ -> List.nth_exn stack @@ nth_from_back stack index
                in
                Array.set upvalues i uv;
                collect_upvals (i + 1))
              else ()
            in
            collect_upvals 0;
            (`Ok, Chunk.closure { function_; upvalues } :: stack)
        | v ->
            fatal_runtime_error vm
              ("Cannot make into closure: " ^ Chunk.show_value v))
    | Op.GetUpvalue, stack ->
        let slot = read_byte vm in
        (* NOTE: I think this is the correct semantics ... *)
        let v = !(Array.get vm.frame.closure.upvalues (Char.to_int slot)) in
        (`Ok, ref v :: stack)
    | Op.SetUpvalue, (top :: _ as stack) ->
        let slot = read_byte vm in
        let uv = Array.get vm.frame.closure.upvalues (Char.to_int slot) in
        (* NOTE: I should figure out how best to test these semantics *)
        uv := !top;
        (`Ok, stack)
    | Op.Class, stack ->
        let name = read_string vm in
        (`Ok, Chunk.cls { name } :: stack)
    | Op.GetProperty, { contents = Chunk.Instance inst } :: stack -> (
        let prop = read_string vm in
        match Table.find inst.fields prop with
        | None ->
            ( runtime_error vm
                (Printf.sprintf "Undefined property '%s'." (String_val.get prop)),
              stack )
        | Some v -> (`Ok, ref v :: stack))
    | Op.SetProperty, v :: { contents = Chunk.Instance inst } :: stack ->
        let prop = read_string vm in
        Table.set inst.fields prop !v;
        (`Ok, v :: stack)
    (* Error cases *)
    | ( ( Op.Negate | Op.Not | Op.Print | Op.Pop | Op.DefineGlobal
        | Op.SetGlobal | Op.SetLocal | Op.JumpIfFalse | Op.Return
        | Op.SetUpvalue | Op.GetProperty ),
        [] ) ->
        fatal_runtime_error vm "Not enough arguments on stack."
    | Op.Negate, stack -> (runtime_error vm "operand must be a number", stack)
    | ( ( Op.Add | Op.Subtract | Op.Divide | Op.Multiply | Op.Equal | Op.Greater
        | Op.Less | Op.SetProperty ),
        ([] | [ _ ]) ) ->
        fatal_runtime_error vm "Not enough arguments on stack."
    | Op.Add, vs ->
        (runtime_error vm "Operands must be two numbers or two strings.", vs)
    | Op.GetProperty, vs ->
        (runtime_error vm "Only instances have properties.", vs)
    | Op.SetProperty, vs -> (runtime_error vm "Only instances have fields.", vs)
    | (Op.Less | Op.Greater), vs ->
        (runtime_error vm "operands must be numbers", vs)
    | (Op.Subtract | Op.Divide | Op.Multiply), vs ->
        (runtime_error vm "operands must be two numbers", vs)
  in
  vm.frame.stack <- stack;
  match res with `Error e -> Error e | `Ok -> run vm | `End -> Ok ()

let interpret_function (function_ : Chunk.function_) (strings : String_arena.t)
    : (unit, Err.t) result =
  let clock _ _ =
    Chunk.float
      (Int63.to_float (Time_now.nanoseconds_since_unix_epoch ()) /. 1e9)
  in
  let closure =
    Chunk.
      {
        function_;
        upvalues = Array.create ~len:function_.upvalue_count (ref Chunk.Nil);
      }
  in
  let vm =
    {
      strings;
      globals = Table.make ();
      frame = { closure; ip = 0; stack = [ Chunk.closure closure ] };
      frame_stack = [];
    }
  in
  define_native vm "clock" clock;
  run vm

let interpret source =
  match Compiler.compile source with
  | Error e -> Error e
  | Ok (fn, arena) -> interpret_function fn arena
