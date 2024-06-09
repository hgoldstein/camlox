type t = { mutable locals : local list; mutable scope_depth : int }
and local = { name : Token.t; depth : int }

let make () = { locals = []; scope_depth = 0 }
let begin_scope l = l.scope_depth <- l.scope_depth + 1
let end_scope l = l.scope_depth <- l.scope_depth - 1
