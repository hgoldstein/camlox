type !'a t

val empty : unit -> 'a t
val write : vec:'a t -> data:'a -> unit
val at : vec:'a t -> index:int -> 'a
val set : vec:'a t -> index:int -> value:'a -> unit
val length : vec:'a t -> int
