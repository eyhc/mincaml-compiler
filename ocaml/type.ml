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

(* return true iff x has the same type that y *)
let rec is_same (x:t) (y:t) : bool = 
  (* recursive call on t list *)
  let rec same_list x y = (
    match x,y with
    | [],[] -> true
    | t1::l1, t2::l2 -> (is_same t1 t2) && (same_list l1 l2)
    | _ -> false) in
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


(* to string functions *)
let rec infix_to_string (to_s : 'a -> string) (l : 'a list) (op : string) : string = 
  match l with 
  | [] -> ""
  | [x] -> to_s x
  | hd :: tl -> (to_s hd) ^ op ^ (infix_to_string to_s tl op)

let rec to_string (x:t) : string =
  match x with
  | Unit -> "unit"
  | Bool -> "bool"
  | Int -> "int"
  | Float -> "float"
  | Fun (l, t) -> Printf.sprintf "(%s-> %s)" (infix_to_string to_string l " ") (to_string t)
  | Tuple l -> Printf.sprintf "tuple(%s)" (infix_to_string to_string l " ")
  | Array t -> Printf.sprintf "array(%s)" (to_string t)
  | Var v -> Printf.sprintf "Var %d" ((Obj.magic v) mod 1000)

let rec to_string2 (x:t) : string =
  match x with
  | Fun (l, t) -> Printf.sprintf "(%s-> %s)" (infix_to_string to_string2 l " ") (to_string2 t)
  | Tuple l -> Printf.sprintf "tuple(%s)" (infix_to_string to_string2 l " ")
  | Array t -> Printf.sprintf "array(%s)" (to_string2 t)
  | Var v -> (
    match !v with
    | None -> "v(none)";
    | Some t1 -> let s2 = to_string2 t1 in Printf.sprintf "v(%s)" s2
  )
  | _ -> to_string x