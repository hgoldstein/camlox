type t

val interpret_chunk : Chunk.t -> (unit, Err.t) result
val interpret : string -> (unit, Err.t) result
