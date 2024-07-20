open Core

let on = false

let simple_instruction name offset =
  Printf.printf "%s\n" name;
  offset + 1

let byte_instruction (chunk : Chunk.t) name offset =
  let byte = Char.to_int @@ Bytes.get chunk.code (offset + 1) in
  Printf.printf "%-16s %4d\n" name byte;
  offset + 2

let constant_instruction (chunk : Chunk.t) name offset =
  let const = Char.to_int @@ Bytes.get chunk.code (offset + 1) in
  Printf.printf "%-16s %4d '" name const;
  Chunk.print_value @@ Array.get chunk.constants const;
  Printf.printf "'\n";
  offset + 2

let jump_instruction (chunk : Chunk.t) name sign offset =
  let jump =
    ((Char.to_int @@ Bytes.get chunk.code (offset + 1)) lsl 8)
    lor (Char.to_int @@ Bytes.get chunk.code (offset + 2))
  in
  Printf.printf "%-16s %4d -> %d\n" name offset (offset + 3 + (sign * jump));
  offset + 3

let invoke_instruction (chunk : Chunk.t) name offset =
  let const = Char.to_int @@ Bytes.get chunk.code (offset + 1) in
  let arg_count = Char.to_int @@ Bytes.get chunk.code (offset + 2) in
  Printf.printf "%-16s (%d args) %4d '" name arg_count const;
  Chunk.print_value @@ Array.get chunk.constants const;
  Printf.printf "'\n";
  offset + 3

let print_line_number (c : Chunk.t) (offset : int) =
  let f = Array.get c.lines in
  if offset > 0 && f offset = f (offset - 1) then Printf.printf "   | "
  else Printf.printf "%4d " (f offset)

let disassemble_instruction (c : Chunk.t) (offset : int) : int =
  let module Op = Opcode in
  Printf.printf "%04d " offset;
  print_line_number c offset;
  match Op.of_byte (Bytes.get c.code offset) with
  | Op.Constant -> constant_instruction c "OP_CONSTANT" offset
  | Op.Return -> simple_instruction "OP_RETURN" offset
  | Op.Negate -> simple_instruction "OP_NEGATE" offset
  | Op.Add -> simple_instruction "OP_ADD" offset
  | Op.Subtract -> simple_instruction "OP_SUBTRACT" offset
  | Op.Multiply -> simple_instruction "OP_MULTIPLY" offset
  | Op.Divide -> simple_instruction "OP_DIVIDE" offset
  | Op.Nil -> simple_instruction "OP_NIL" offset
  | Op.True -> simple_instruction "OP_TRUE" offset
  | Op.False -> simple_instruction "OP_FALSE" offset
  | Op.Not -> simple_instruction "OP_NOT" offset
  | Op.Less -> simple_instruction "OP_LESS" offset
  | Op.Greater -> simple_instruction "OP_GREATER" offset
  | Op.Equal -> simple_instruction "OP_EQUAL" offset
  | Op.Print -> simple_instruction "OP_PRINT" offset
  | Op.Pop -> simple_instruction "OP_POP" offset
  | Op.DefineGlobal -> constant_instruction c "OP_DEFINE_GLOBAL" offset
  | Op.GetGlobal -> constant_instruction c "OP_GET_GLOBAL" offset
  | Op.SetGlobal -> constant_instruction c "OP_SET_GLOBAL" offset
  | Op.GetLocal -> byte_instruction c "OP_GET_LOCAL" offset
  | Op.SetLocal -> byte_instruction c "OP_SET_LOCAL" offset
  | Op.JumpIfFalse -> jump_instruction c "OP_JUMP_IF_FALSE" 1 offset
  | Op.Jump -> jump_instruction c "OP_JUMP" 1 offset
  | Op.Loop -> jump_instruction c "OP_LOOP" (-1) offset
  | Op.Call -> byte_instruction c "OP_CALL" offset
  | Op.GetUpvalue -> byte_instruction c "OP_GET_UPVALUE" offset
  | Op.SetUpvalue -> byte_instruction c "OP_SET_UPVALUE" offset
  | Op.Closure -> (
      let constant = Char.to_int @@ Bytes.get c.code (offset + 1) in
      let value = Array.get c.constants constant in
      Printf.printf "%-16s %4d " "OP_CLOSURE" constant;
      Chunk.print_value value;
      Printf.printf "\n";
      match value with
      | Chunk.Function function_ ->
          let rec print_captures j offset =
            if j >= function_.upvalue_count then offset
            else
              let is_local = Bytes.get c.code offset in
              let index = Bytes.get c.code (offset + 1) in
              Printf.printf "%04d      |                     %s %d\n" offset
                (if Char.equal is_local '\x01' then "local" else "upvalue")
                (Char.to_int index);
              print_captures (j + 1) (offset + 2)
          in
          print_captures 0 (offset + 2)
      | v -> failwith ("Unexpected closure argument: " ^ Chunk.show_value v))
  | Op.Class -> constant_instruction c "OP_CLASS" offset
  | Op.GetProperty -> constant_instruction c "OP_GET_PROPERTY" offset
  | Op.SetProperty -> constant_instruction c "OP_SET_PROPERTY" offset
  | Op.Method -> constant_instruction c "OP_METHOD" offset
  | Op.Invoke -> invoke_instruction c "OP_INVOKE" offset
  | Op.Inherit -> simple_instruction "OP_INHERIT" offset
  | Op.GetSuper -> constant_instruction c "OP_GET_SUPER" offset
  | Op.SuperInvoke -> invoke_instruction c "OP_SUPER_INVOKE" offset

let disassemble_chunk (c : Chunk.t) (name : string) =
  Printf.printf "== %s ==\n" name;
  let rec aux offset =
    if offset < Bytes.length c.code then aux @@ disassemble_instruction c offset
    else ()
  in
  aux 0
