open Camlox

let () = 
  let chunk = Chunk.make () in
  Chunk.write_op ~chunk ~line:123 ~opcode:Chunk.OpCode.Return;
  let const = Chunk.add_constant ~chunk ~value:(Value.Float 1.2) in
  Chunk.write_op ~chunk ~line:123 ~opcode:Chunk.OpCode.Constant;
  Chunk.write_int_unsafe ~chunk ~line:123 ~value:const;
  Debug.disassemble_chunk chunk "test chunk";
  ()
