type t = String_val.t Table.t

let get table chars =
  let prospective = String_val.make_uninterned_string chars in
  match Table.find table prospective with
  | Some v -> v
  | None ->
      Table.set table prospective prospective;
      prospective

let value table chars = Chunk.String (get table chars)
