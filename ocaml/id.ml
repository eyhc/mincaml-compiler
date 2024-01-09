type t = string
type l = string

let to_string x = x

let genid =
  let counter = ref (0) in
  fun () ->
    incr counter;
    Printf.sprintf "v%d_" !counter

let make_unique =
  let counter = ref (0) in
  fun (name:t) : t ->
    if String.get name ((String.length name) - 1) = '_' then name
    else 
      let _ = incr counter in Printf.sprintf "%s%d" name !counter