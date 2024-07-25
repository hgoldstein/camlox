module UninternedStrTable = Hashtbl.Make (struct
  type t = String_val.t

  let equal (a : t) (b : t) = String.equal (String_val.get a) (String_val.get b)
  let hash (s : t) = String_val.hash s
end)

type t = String_val.t UninternedStrTable.t

let make () = UninternedStrTable.create 0

let get (table : t) chars =
  let prospective = String_val.make_uninterned_string chars in
  match UninternedStrTable.find_opt table prospective with
  | Some v -> v
  | None ->
      UninternedStrTable.add table prospective prospective;
      prospective

let value table chars = Chunk.String (get table chars)
