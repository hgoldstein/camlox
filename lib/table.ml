module InternedStrTable = Hashtbl.Make (struct
  type t = String_val.t

  let equal a b = Core.phys_equal a b
  let hash s = String_val.hash s
end)

type !'a t = 'a InternedStrTable.t

let make () = InternedStrTable.create 0
let set = InternedStrTable.add
let find = InternedStrTable.find_opt
let delete = InternedStrTable.remove
let mem = InternedStrTable.mem

let add_all ~src ~dest =
  let f key value = set dest key value in
  InternedStrTable.iter f src
