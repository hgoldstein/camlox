type t = String_val.t Table.t

let get table chars =
  let prospective = String_val.make_uninterned_string chars in
  match Table.find table prospective with
  | Some v -> Chunk.Object (Chunk.String v)
  | None ->
      Table.set table prospective prospective;
      Chunk.Object (Chunk.String prospective)
