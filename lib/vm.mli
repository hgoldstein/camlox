type t

val make : unit -> t
val interpret : t -> string -> (unit, Err.t) result
