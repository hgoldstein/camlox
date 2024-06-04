type t [@@deriving show]

val get : t -> string
val hash : t -> int
val make_uninterned_string : string -> t
