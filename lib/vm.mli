type t
type error = CompileError | RuntimeError

val interpret_chunk : Chunk.t -> (unit, error) result
val interpret : string -> (unit, error) result
