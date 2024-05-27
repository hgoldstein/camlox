type t
type error = CompileError | RuntimeError

val interpret : Chunk.t -> (unit, error) result
