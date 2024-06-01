val on : unit -> bool
val enable : unit -> unit
val disassemble_instruction : Chunk.t -> int -> int
val disassemble_chunk : Chunk.t -> string -> unit
