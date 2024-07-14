type !'a t = { mutable data : 'a Array.t; mutable count : int }

let empty () = { count = 0; data = [||] }
let grow_capacity (c : int) = if c < 8 then 8 else c * 2

let append vec ~value =
  let capacity = Array.length vec.data in
  if capacity < vec.count + 1 then (
    (* NOTE: does passing in `data` here slow things
     * down? Should we functorize this and require a zero value?
     *)
    let new_data = Array.make (grow_capacity capacity) value in
    Array.blit vec.data 0 new_data 0 capacity;
    vec.data <- new_data);
  vec.data.(vec.count) <- value;
  vec.count <- vec.count + 1

let at vec ~index = Array.get vec.data index
let length vec = vec.count
let set vec ~index ~value = Array.set vec.data index value

let set_extend vec ~index ~value =
  if index < vec.count then set vec ~index ~value
  else if index > vec.count then
    failwith
      (Printf.sprintf "Index, %d, out of bounds (count = %d)" index vec.count)
  else append vec ~value
