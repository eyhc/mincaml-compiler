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
  | Tuple l -> Printf.sprintf "tuple(%s)" (List.fold_left (fun s x -> s ^ (to_string x)) "" l)
  | Array t -> Printf.sprintf "array(%s)" (to_string t)
  | Var v -> Printf.sprintf "Var %d" ((Obj.magic v) mod 1000)

let rec to_string2 (x:t) : string =
  match x with
  | Var v -> (
    match !v with
    | None -> "var(None)";
    | Some t1 -> let s2 = to_string2 t1 in Printf.sprintf "var(%s)" s2
  )
  | Fun (l, t) -> Printf.sprintf "fun %s -> %s" (List.fold_left (fun s x -> s ^ (to_string2 x)) "" l) (to_string2 t)
  | Tuple l -> Printf.sprintf "tuple(%s)" (List.fold_left (fun s x -> s ^ (to_string2 x)) "" l)
  | Array t -> Printf.sprintf "array(%s)" (to_string2 t)
  | _ -> to_string x

(* return true iff x has the same type that y *)
let rec is_same (x:t) (y:t) : bool = 

  (* recursive call on t list *)
  let rec same_list x y = (
    if (List.length x) <> (List.length y) then false
    else match x,y with
    | [],[] -> true
    | t1::l1, t2::l2 -> (is_same t1 t2) && (same_list l1 l2)
    | _ -> failwith "impossible case"
  ) in
  
  (* matching cases *)
  match x,y with
    | Unit,Unit -> true
    | Int,Int -> true
    | Bool, Bool -> true
    | Float,Float -> true
    | Fun (l1, t1), Fun (l2, t2) -> (is_same t1 t2) && (same_list l1 l2)
    | Var v1, Var v2 -> v1 == v2 (* comparaison de références *)
    | Tuple l1, Tuple l2 -> same_list l1 l2
    | Array t1, Array t2 -> is_same t1 t2
    | _ -> false