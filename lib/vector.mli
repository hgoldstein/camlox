type !'a t

val empty : unit -> 'a t
val append : 'a t -> value:'a -> unit
val at : 'a t -> index:int -> 'a
val set : 'a t -> index:int -> value:'a -> unit
val length : 'a t -> int
val set_extend : 'a t -> index:int -> value:'a -> unit
