type t = { chars : string; hash : int }

let get { chars; hash = _ } = chars
let hash { chars = _; hash } = hash
let make_uninterned_string chars = { chars; hash = Hashtbl.hash chars }
