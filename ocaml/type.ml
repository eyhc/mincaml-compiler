type t =
  | Unit
  | Bool
  | Int
  | Float
  | Fun of t list * t 
  | Tuple of t list
  | Array of t
  | Var of t option ref

  let gentyp () = Var(ref None) 

  let rec to_string (x:t) : string =
    match x with
    | Unit -> "unit"
    | Bool -> "bool"
    | Int -> "int"
    | Float -> "float"
    | Fun (l, t) -> Printf.sprintf "fun %s -> %s" (List.fold_left (fun s x -> s ^ (to_string x)) "" l) (to_string t)
    | Tuple l -> "tuple"
    | Array t -> "array"
    | Var v -> Printf.sprintf "Var %d" ((Obj.magic v) mod 1000)
