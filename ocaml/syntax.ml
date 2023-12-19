open Printf

type t = 
  | Unit
  | Bool of bool
  | Int of int
  | Float of float
  | Not of t
  | Neg of t
  | Add of t * t
  | Sub of t * t
  | FNeg of t
  | FAdd of t * t
  | FSub of t * t
  | FMul of t * t
  | FDiv of t * t
  | Eq of t * t
  | LE of t * t
  | If of t * t * t
  | Let of (Id.t * Type.t) * t * t
  | Var of Id.t
  | LetRec of fundef * t
  | App of t * t list
  | Tuple of t list
  | LetTuple of (Id.t * Type.t) list * t * t
  | Array of t * t
  | Get of t * t
  | Put of t * t * t
and fundef = { name : Id.t * Type.t; args : (Id.t * Type.t) list; body : t }





let rec find name list =
        match list with
        | [] -> 0
        | head :: l -> if name = head then 1 else find name l

let add_to_list name list =
        if find name list = 0 then list@[name] else list

let rec iterate_args args list =
        match args with
        | [] -> list
        | (id, t) :: l -> add_to_list id (iterate_args l list)

let rec find_variables exp list =
        match exp with
        | Unit -> list
        | Bool b -> list
        | Int i -> list
        | Float f -> list
        | Not e -> find_variables e list
        | Neg e -> find_variables e list
        | Add (e1, e2) -> (find_variables e1 (find_variables e2 list))
        | Sub (e1, e2) -> (find_variables e1 (find_variables e2 list))
        | FNeg e -> find_variables e list
        | FAdd (e1, e2) -> (find_variables e1 (find_variables e2 list))
        | FSub (e1, e2) -> (find_variables e1 (find_variables e2 list))
        | FMul (e1, e2) -> (find_variables e1 (find_variables e2 list))
        | FDiv (e1, e2) -> (find_variables e1 (find_variables e2 list))
        | Eq (e1, e2) -> (find_variables e1 (find_variables e2 list)) 
        | LE (e1, e2) -> (find_variables e1 (find_variables e2 list))
        | If (e1, e2, e3) -> (find_variables e1 (find_variables e2 (find_variables e3 list)))
        | Let ((id,t), e1, e2) -> (find_variables e1 (find_variables e2 (add_to_list id list)))
        | Var id -> add_to_list id list
        | App (e1, le2) -> (find_variables e1 list)
        | LetRec (fd, e) ->  find_variables e (find_variables fd.body (iterate_args fd.args list))
        | LetTuple (l, e1, e2)-> find_variables e1 (find_variables e2 list)
        | Get(e1, e2) -> (find_variables e1 (find_variables e2 list))
        | Put(e1, e2, e3) -> (find_variables e1 (find_variables e2 (find_variables e3 list)))
        | Tuple(l) -> list
        | Array(e1,e2) -> (find_variables e1 (find_variables e2 list))