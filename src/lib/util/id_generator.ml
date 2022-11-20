open Core

let counter = ref 0

let get_id =
  let x = !counter in
  counter := x + 1;
  x

let get_new_name = 
  Fmt.str "__%d" get_id