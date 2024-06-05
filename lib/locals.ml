type t = { locals : local Vector.t; mutable scope_depth : int }
and local = { name : Token.t; depth : int }

let make () = { locals = Vector.empty (); scope_depth = 0 }
let begin_scope l = l.scope_depth <- l.scope_depth + 1
let end_scope l = l.scope_depth <- l.scope_depth - 1
