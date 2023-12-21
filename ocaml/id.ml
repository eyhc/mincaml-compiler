type t = string

let to_string x = x

let genid =
  let counter = ref (0) in
  fun () ->
    incr counter;
    Printf.sprintf "?v%d" !counter
