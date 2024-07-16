module StrTable = Hashtbl.Make (struct
  type t = String_val.t

  let equal (a : t) (b : t) = String.equal (String_val.get a) (String_val.get b)
  let hash (s : t) = String_val.hash s
end)

type !'a t = 'a StrTable.t

let make () = StrTable.create 0
let set = StrTable.add
let find = StrTable.find_opt
let delete = StrTable.remove
let mem = StrTable.mem

let add_all ~src ~dest =
  let f key value = set dest key value in
  StrTable.iter f src
