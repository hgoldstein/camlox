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

type cycle_result = [ `Ok of Chunk.value list | `Error of Err.t | `End ]

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
    let call_closure (closure : Chunk.closure) (bound_this : Chunk.value option)
        =
      if arg_count <> closure.function_.arity then
        runtime_error vm
        @@ Printf.sprintf "Expected %d arguments but got %d"
             closure.function_.arity arg_count
      else
        (*
         * We need to slice the current stack into three sections:
         *
         *   | Stack values not in the function call | function obj | function call arguments |
         *
         * - The lower stack values are put back in place
         * - The function call arguments always make up the new stack
         * - Depending on if we're calling a function or a method, we replace
         *   the function object with `this`
         *)
        (* We take every argument *and* one more for the function object*)
        let rec loop n acc = function
          | [] -> failwith "Unexpected end of stack"
          | hd :: tl when n = 0 -> (tl, hd, acc)
          | hd :: tl -> loop (n - 1) (hd :: acc) tl
        in
        let old_stack, obj, new_stack_reversed = loop arg_count [] stack in
        let new_stack =
          List.rev
          @@ (match bound_this with None -> obj | Some m -> m)
             :: new_stack_reversed
        in
        vm.frame.stack <- old_stack;
        push_frame vm @@ { ip = 0; closure; stack = [] };
        `Ok new_stack
    in
    match !callee with
    | Chunk.Native fn ->
        let new_stack, old_stack = List.split_n stack (arg_count + 1) in
        let result = fn arg_count new_stack in
        `Ok (result :: old_stack)
    | Chunk.Closure closure -> call_closure closure None
    | Chunk.Class class_ ->
        let instance = Chunk.instance { class_; fields = Table.make () } in
        let _, old_stack = List.split_n stack (arg_count + 1) in
        `Ok (instance :: old_stack)
    | Chunk.BoundMethod bm -> call_closure bm.method_ (Some bm.receiver)
    | _ -> runtime_error vm "Can only call functions and classes."
  in
  let res : cycle_result =
    match (Op.of_byte @@ read_byte vm, vm.frame.stack) with
    | Op.Return, result :: _ -> (
        match vm.frame_stack with
        | [] -> `End
        | f :: fs ->
            vm.frame <- f;
            vm.frame_stack <- fs;
            `Ok (result :: vm.frame.stack))
    | Op.Constant, vs -> `Ok (ref (read_constant vm) :: vs)
    | Op.Negate, { contents = Float f } :: vs ->
        `Ok (Chunk.float (f *. -1.0) :: vs)
    | Op.Add, { contents = Float b } :: { contents = Float a } :: stack ->
        `Ok (Chunk.float (a +. b) :: stack)
    | Op.Add, { contents = String b } :: { contents = String a } :: stack ->
        let a_chars = String_val.get a in
        let b_chars = String_val.get b in
        `Ok (ref (String_arena.value vm.strings (a_chars ^ b_chars)) :: stack)
    | Op.Subtract, { contents = Float b } :: { contents = Float a } :: stack ->
        `Ok (Chunk.float (a -. b) :: stack)
    | Op.Divide, { contents = Float b } :: { contents = Float a } :: stack ->
        `Ok (Chunk.float (a /. b) :: stack)
    | Op.Multiply, { contents = Float b } :: { contents = Float a } :: stack ->
        `Ok (Chunk.float (a *. b) :: stack)
    | Op.Nil, stack -> `Ok (ref Chunk.Nil :: stack)
    | Op.True, stack -> `Ok (Chunk.bool true :: stack)
    | Op.False, stack -> `Ok (Chunk.bool false :: stack)
    | Op.Not, v :: vs -> `Ok (Chunk.bool (Chunk.is_falsey v) :: vs)
    | Op.Equal, b :: a :: stack -> `Ok (Chunk.bool (Chunk.equal a b) :: stack)
    | Op.Greater, { contents = Float b } :: { contents = Float a } :: stack ->
        `Ok (Chunk.bool (Float.compare a b > 0) :: stack)
    | Op.Less, { contents = Float b } :: { contents = Float a } :: stack ->
        `Ok (Chunk.bool (Float.compare a b < 0) :: stack)
    | Op.Print, v :: stack ->
        Chunk.print_line !v;
        `Ok stack
    | Op.Pop, _ :: stack -> `Ok stack
    | Op.DefineGlobal, value :: stack ->
        let name = read_string vm in
        Table.set vm.globals name value;
        `Ok stack
    | Op.GetGlobal, stack -> (
        let name = read_string vm in
        match Table.find vm.globals name with
        | None ->
            runtime_error vm
              (Printf.sprintf "Undefined variable '%s'." (String_val.get name))
        | Some v -> `Ok (v :: stack))
    | Op.SetGlobal, v :: stack ->
        let name = read_string vm in
        if not @@ Table.mem vm.globals name then
          runtime_error vm
            (Printf.sprintf "Undefined variable '%s'." (String_val.get name))
        else (
          Table.set vm.globals name v;
          `Ok (v :: stack))
    | Op.GetLocal, stack ->
        let slot = nth_from_back stack @@ read_byte vm in
        `Ok (List.nth_exn stack slot :: stack)
    | Op.SetLocal, (top :: _ as stack) ->
        let slot = nth_from_back stack @@ read_byte vm in
        let f idx v = if idx = slot then top else v in
        `Ok (List.mapi stack ~f)
    | Op.JumpIfFalse, (top :: _ as stack) ->
        let offset = read_short vm in
        if Chunk.is_falsey top then vm.frame.ip <- vm.frame.ip + offset;
        `Ok stack
    | Op.Jump, stack ->
        let offset = read_short vm in
        vm.frame.ip <- vm.frame.ip + offset;
        `Ok stack
    | Op.Loop, stack ->
        let offset = read_short vm in
        vm.frame.ip <- vm.frame.ip - offset;
        `Ok stack
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
            `Ok (Chunk.closure { function_; upvalues } :: stack)
        | v ->
            fatal_runtime_error vm
              ("Cannot make into closure: " ^ Chunk.show_value v))
    | Op.GetUpvalue, stack ->
        let slot = read_byte vm in
        (* NOTE: I think this is the correct semantics ... *)
        let v = !(Array.get vm.frame.closure.upvalues (Char.to_int slot)) in
        `Ok (ref v :: stack)
    | Op.SetUpvalue, (top :: _ as stack) ->
        let slot = read_byte vm in
        let uv = Array.get vm.frame.closure.upvalues (Char.to_int slot) in
        (* NOTE: I should figure out how best to test these semantics *)
        uv := !top;
        `Ok stack
    | Op.Class, stack ->
        let name = read_string vm in
        `Ok (Chunk.cls { name; methods = Table.make () } :: stack)
    | Op.GetProperty, { contents = Chunk.Instance inst } :: stack -> (
        let bind_method (cls : Chunk.class_) p =
          match Table.find cls.methods p with
          | None ->
              runtime_error vm
                (Printf.sprintf "Undefined property '%s'." (String_val.get p))
          | Some m ->
              `Ok
                (ref
                   (Chunk.BoundMethod
                      { method_ = m; receiver = ref (Chunk.Instance inst) })
                :: stack)
        in
        let prop = read_string vm in
        match Table.find inst.fields prop with
        | None -> bind_method inst.class_ prop
        | Some v -> `Ok (ref v :: stack))
    | Op.SetProperty, v :: { contents = Chunk.Instance inst } :: stack ->
        let prop = read_string vm in
        Table.set inst.fields prop !v;
        `Ok (v :: stack)
    | ( Op.Method,
        { contents = Chunk.Closure v }
        :: ({ contents = Chunk.Class cls } as class_ref)
        :: rest ) ->
        let method_name = read_string vm in
        Table.set cls.methods method_name v;
        `Ok (class_ref :: rest)
    (* Error cases *)
    | ( ( Op.Negate | Op.Not | Op.Print | Op.Pop | Op.DefineGlobal
        | Op.SetGlobal | Op.SetLocal | Op.JumpIfFalse | Op.Return
        | Op.SetUpvalue | Op.GetProperty ),
        [] ) ->
        fatal_runtime_error vm "Not enough arguments on stack."
    | Op.Negate, _ -> runtime_error vm "operand must be a number"
    | ( ( Op.Add | Op.Subtract | Op.Divide | Op.Multiply | Op.Equal | Op.Greater
        | Op.Less | Op.SetProperty | Op.Method ),
        ([] | [ _ ]) ) ->
        fatal_runtime_error vm "Not enough arguments on stack."
    | Op.Add, _ ->
        runtime_error vm "Operands must be two numbers or two strings."
    | Op.GetProperty, _ -> runtime_error vm "Only instances have properties."
    | Op.SetProperty, _ -> runtime_error vm "Only instances have fields."
    | Op.Method, _ ->
        fatal_runtime_error vm "Unexpected operands for OP_METHOD."
    | (Op.Less | Op.Greater), _ -> runtime_error vm "operands must be numbers"
    | (Op.Subtract | Op.Divide | Op.Multiply), _ ->
        runtime_error vm "operands must be two numbers"
  in
  match res with
  | `Error e -> Error e
  | `End -> Ok ()
  | `Ok stack ->
      vm.frame.stack <- stack;
      run vm

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
