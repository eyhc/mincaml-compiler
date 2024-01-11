type t = string
type l = string

let to_string x = x

let make_unique =
  let counter = ref (0) in
  fun (name:t) : t ->
    incr counter;
    Printf.sprintf "%s%d" name !counter

let genid () = make_unique "temp"