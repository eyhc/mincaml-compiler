type t = string
type l = string

let to_string x = x

let genid =
  let counter = ref (0) in
  fun () ->
    incr counter;
    Printf.sprintf "?v%d" !counter

let genlabel =
  let counter = ref (0) in
  fun () ->
    incr counter;
    Printf.sprintf "?l%d" !counter

let genid_list n =
  List.init n (fun _ -> genid ())