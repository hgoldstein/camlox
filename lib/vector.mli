type 'a t

val make : size:int -> elem:'a -> 'a t
val write : vec:'a t -> data:'a -> unit
val at : vec:'a t -> index:int -> 'a
val length : vec:'a t -> int
