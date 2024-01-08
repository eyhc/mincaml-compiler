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

let make_unique =
  let counter = ref (0) in
  fun (name:t) : t ->
    let car = String.get name 0 in
      if car = '?' || car = '$' then name
      else 
        let _ = incr counter in Printf.sprintf "$%s%d" name !counter