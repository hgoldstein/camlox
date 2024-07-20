open Core

type call_frame = {
  closure : Chunk.closure;
  mutable stack : Chunk.value list;
  mutable ip : int;
}

type t = {
  strings : String_arena.t;
  globals : Chunk.value Table.t;
  init_string : String_val.t;
  mutable frame : call_frame;
  mutable frame_stack : call_frame list;
  mutable frame_count : int;
}

type cycle_result = [ `Ok of Chunk.value list | `Error of Err.t | `End ]

let max_frames = 64

let read_byte vm =
  let index = vm.frame.ip in
  vm.frame.ip <- vm.frame.ip + 1;
  Vector.at vm.frame.closure.function_.chunk.code ~index

let read_short vm =
  let hi_byte = read_byte vm in
  let lo_byte = read_byte vm in
  (Char.to_int hi_byte lsl 8) lor Char.to_int lo_byte

let read_constant vm =
  let index = Char.to_int @@ read_byte vm in
  Vector.at vm.frame.closure.function_.chunk.constants ~index

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
      (Vector.at frame.closure.function_.chunk.lines ~index:(frame.ip - 1));
    match frame.closure.function_.name with
    | None -> Printf.eprintf "script\n"
    | Some s -> Printf.eprintf "%s()\n" (String_val.get s)
  in
  let print_frames = List.iter ~f:print_frame in
  Printf.eprintf "%s\n" msg;
  print_frame vm.frame;
  print_frames vm.frame_stack;
  `Error Err.Runtime

let push_frame vm frame =
  vm.frame_stack <- vm.frame :: vm.frame_stack;
  vm.frame <- frame

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
let split_stack stack arg_count new_base =
  let old_stack = ref [] in
  let rec loop n = function
    | [] -> failwith "Unexpected end of stack"
    | hd :: tl when n = 0 -> (
        old_stack := tl;
        match new_base with None -> [ hd ] | Some m -> [ m ])
    | hd :: tl -> hd :: loop (n - 1) tl
  in
  let new_stack = loop arg_count stack in
  (!old_stack, new_stack)

let call_closure vm stack (closure : Chunk.closure) arg_count
    (bound_this : Chunk.value option) =
  if arg_count <> closure.function_.arity then
    runtime_error vm
    @@ Printf.sprintf "Expected %d arguments but got %d."
         closure.function_.arity arg_count
  else if vm.frame_count = max_frames then runtime_error vm "Stack overflow."
  else
    let old_stack, new_stack = split_stack stack arg_count bound_this in
    vm.frame.stack <- old_stack;
    vm.frame_count <- vm.frame_count + 1;
    push_frame vm @@ { ip = 0; closure; stack = [] };
    `Ok new_stack

let call_value vm stack callee arg_count =
  match !callee with
  | Chunk.Native fn ->
      let new_stack, old_stack = List.split_n stack (arg_count + 1) in
      `Ok (fn arg_count new_stack :: old_stack)
  | Chunk.Closure closure -> call_closure vm stack closure arg_count None
  | Chunk.Class class_ -> (
      let instance = Chunk.Instance { class_; fields = Table.make () } in
      match Table.find class_.methods vm.init_string with
      | None when arg_count = 0 ->
          (* The head of our stack should be the class ptr *)
          `Ok (ref instance :: List.tl_exn stack)
      | None ->
          runtime_error vm
            (Printf.sprintf "Expected 0 arguments but got %d." arg_count)
      | Some m -> call_closure vm stack m arg_count (Some (ref instance)))
  | Chunk.BoundMethod bm ->
      call_closure vm stack bm.method_ arg_count (Some bm.receiver)
  | _ -> runtime_error vm "Can only call functions and classes."

let invoke (vm : t) stack method_ arg_count =
  let callee = List.nth_exn stack arg_count in
  match !callee with
  | Chunk.Instance inst -> (
      match Table.find inst.fields method_ with
      | Some m ->
          let rec set i = function
            | [] -> ()
            | hd :: tl -> if i = 0 then hd := m else set (i - 1) tl
          in
          set arg_count stack;
          call_value vm stack (ref m) arg_count
      | None -> (
          match Table.find inst.class_.methods method_ with
          | None ->
              runtime_error vm
                (Printf.sprintf "Undefined property '%s'."
                   (String_val.get method_))
          | Some m -> call_closure vm stack m arg_count (Some callee)))
  | _ -> runtime_error vm "Only instances have methods."

let rec run (vm : t) : (unit, Err.t) result =
  if Dbg.on () then (
    print_stack vm;
    ignore
    @@ Dbg.disassemble_instruction vm.frame.closure.function_.chunk vm.frame.ip);
  let module Op = Opcode in
  let nth_from_back wrt i = List.length wrt - Char.to_int i - 1 in
  let res : cycle_result =
    match (Op.of_byte @@ read_byte vm, vm.frame.stack) with
    | Op.Return, result :: _ -> (
        match vm.frame_stack with
        | [] -> `End
        | f :: fs ->
            vm.frame <- f;
            vm.frame_stack <- fs;
            vm.frame_count <- vm.frame_count - 1;
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
        (* When we get a local, we are allocating a new slot on the stack, so
         * the correct semantics are to create a new reference.
         *)
        let local = !(List.nth_exn stack slot) in
        `Ok (ref local :: stack)
    | Op.SetLocal, (top :: _ as stack) ->
        let slot = nth_from_back stack @@ read_byte vm in
        (* When we *set* a local, we are replacing an allocated stack
         * slot, so the correct references are to write to the slot.
         *)
        List.nth_exn stack slot := !top;
        `Ok stack
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
        (* When we allocate a new object onto the stack, it should be a
         * separate reference.
         *)
        let v = !(Array.get vm.frame.closure.upvalues (Char.to_int slot)) in
        `Ok (ref v :: stack)
    | Op.SetUpvalue, (top :: _ as stack) ->
        let slot = read_byte vm in
        let uv = Array.get vm.frame.closure.upvalues (Char.to_int slot) in
        (* When we *set* an upvalue, we are replacing an allocated stack
         * slot which may be used elsewhere, so the correct semantics
         * are to write to the slot.
         *)
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
    | Op.Invoke, stack ->
        let method_ = read_string vm in
        let arg_count = Char.to_int @@ read_byte vm in
        invoke vm stack method_ arg_count
    | ( Op.Inherit,
        { contents = Chunk.Class child }
        :: ({ contents = Chunk.Class parent } as r)
        :: stack ) ->
        Table.add_all ~src:parent.methods ~dest:child.methods;
        `Ok (r :: stack)
    | ( Op.GetSuper,
        { contents = Chunk.Class supercls }
        :: { contents = Chunk.Instance inst }
        :: stack ) -> (
        let method_ = read_string vm in
        (* NOTE: This could be combined with the impl for GetProperty *)
        match Table.find supercls.methods method_ with
        | None ->
            runtime_error vm
              (Printf.sprintf "Undefined property '%s'."
                 (String_val.get method_))
        | Some m ->
            `Ok
              (ref
                 (Chunk.BoundMethod
                    { method_ = m; receiver = ref (Chunk.Instance inst) })
              :: stack))
    | Op.SuperInvoke, { contents = Chunk.Class cls } :: stack -> (
        let method_ = read_string vm in
        let arg_count = Char.to_int @@ read_byte vm in
        match Table.find cls.methods method_ with
        | None ->
            runtime_error vm
              (Printf.sprintf "Undefined property '%s'."
                 (String_val.get method_))
        | Some m_ -> call_closure vm stack m_ arg_count None)
    (* Error cases *)
    | ( ( Op.Negate | Op.Not | Op.Print | Op.Pop | Op.DefineGlobal
        | Op.SetGlobal | Op.SetLocal | Op.JumpIfFalse | Op.Return
        | Op.SetUpvalue | Op.GetProperty ),
        [] ) ->
        fatal_runtime_error vm "Not enough arguments on stack."
    | Op.Negate, _ -> runtime_error vm "Operand must be a number."
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
    | (Op.Less | Op.Greater), _ -> runtime_error vm "Operands must be numbers."
    | (Op.Subtract | Op.Divide | Op.Multiply), _ ->
        runtime_error vm "Operands must be numbers."
    | Op.Inherit, _ -> runtime_error vm "Superclass must be a class."
    | Op.GetSuper, _ -> fatal_runtime_error vm "Cannot get super of non-class"
    | Op.SuperInvoke, _ ->
        fatal_runtime_error vm "Cannot invoke super of non-class "
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
      init_string = String_arena.get strings "init";
      globals = Table.make ();
      frame = { closure; ip = 0; stack = [ Chunk.closure closure ] };
      frame_stack = [];
      frame_count = 1;
    }
  in
  define_native vm "clock" clock;
  run vm

let interpret source =
  match Compiler.compile source with
  | Error e -> Error e
  | Ok (fn, arena) -> interpret_function fn arena
