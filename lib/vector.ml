type 'a t = { mutable data : 'a Array.t; mutable count : int }

let make ~size ~elem = { count = 0; data = Array.make size elem }
let grow_capacity (c : int) = if c < 8 then 8 else c * 2

let write ~vec ~data =
  let capacity = Array.length vec.data in
  if capacity < vec.count + 1 then (
    (* NOTE(hgoldstein): does passing in `data` here slow things
     * down? Should we functorize this and require a zero value?
     *)
    let new_data = Array.make (grow_capacity capacity) data in
    Array.blit vec.data 0 new_data 0 capacity;
    vec.data <- new_data);
  vec.data.(vec.count) <- data;
  vec.count <- vec.count + 1

let at ~vec ~index = Array.get vec.data index
let length ~vec = vec.count
