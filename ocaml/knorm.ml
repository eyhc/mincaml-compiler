(*
knorm.ml

date : 08-01-2023
*)

open Printf

(* Le type knorm_t représente un ast k-normalisé
 * -> All nested expressions are replaced by new fresh variables *)
type knorm_t =
  | Unit
  | Var of Id.t
  | Int of int
  | Float of float
  | Bool of bool
  | Not of Id.t
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | Eq of Id.t * Id.t 
  | LE of Id.t * Id.t
  | If of Id.t * knorm_t * knorm_t
  | Let of (Id.t * Type.t) * knorm_t * knorm_t
  | LetRec of fundef * knorm_t
  | App of Id.t * Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * knorm_t
  | Tuple of Id.t list
  | Array of Id.t * Id.t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
and fundef = {name : Id.t * Type.t; args : (Id.t * Type.t) list; body : knorm_t }


(* insertion d'une expression let si besoin
 * pour une expression k-normalisé e de type t
 * où l'application de k sur une nouvelle variable x (Id.t) 
 * donne une expression e' (k-normalisé) dans le pattern (let x:t = e in e')
 *)
let insert_let ((e,t):knorm_t*Type.t) (k:Id.t -> knorm_t) : knorm_t =
  match e with
  | Var x -> k x
  | _ ->
    let x = Id.genid () in
      let e' = k x in
        Let((x, t), e, e')


(* CF equations de la section 4.3 de l'article *)
let k_normalization (exp:Syntax.t) : knorm_t =

  (* norm retourne l'expression k-normalisé correspondant à ast 
     ainsi que son type (nécessaire en cas d'insertion d'expression let) *)
  (* l'environnement contient le type des variables *)
  let rec norm (exp:Syntax.t) (env:(Id.t*Type.t) list) : knorm_t * Type.t =

    (* applique deux fois insert_let *)
    let insert_2x e1 e2 f = insert_let (norm e1 env) 
        (fun x -> insert_let (norm e2 env) (fun y -> f x y))
    in

    (* applique insert let sur une liste *)
    let insert_nx (le:Syntax.t list) (f:Id.t list -> knorm_t) : knorm_t =
      (* le : liste d'expression non traitées; res liste de variables (correspondantes) *)
      let rec insert_nx_rec le f res =
        (match le with
        | [] -> f (List.rev res) (* res est dans le mauvais ordre *)
        | e::l -> insert_let (norm e env) (fun xi -> insert_nx_rec l f (xi::res)))
      in insert_nx_rec le f []
    in

    (match exp with
    | Unit -> Unit, Unit
    | Int i -> Int i, Int
    | Float f -> Float f, Float
    | Bool b -> Bool b, Bool

    | Var id -> Var id,
      (try 
        (snd (List.find (fun (x,y) -> x = id) env))
      with Not_found -> failwith (Printf.sprintf "Knorm failed -> Not found : %s" id))

    | Add (e1, e2) -> (insert_2x e1 e2 (fun x y -> Add (x,y))), Int
    | Sub (e1, e2) -> (insert_2x e1 e2 (fun x y -> Sub (x,y))), Int
    | Neg e -> (insert_let (norm e env) (fun x -> Neg x)), Int

    | FNeg e -> (insert_let (norm e env) (fun x -> FNeg x)), Float
    | FAdd (e1, e2) -> (insert_2x e1 e2 (fun x y -> FAdd (x,y))), Float
    | FSub (e1, e2) -> (insert_2x e1 e2 (fun x y -> FSub (x,y))), Float
    | FMul (e1, e2) -> (insert_2x e1 e2 (fun x y -> FMul (x,y))), Float
    | FDiv (e1, e2) -> (insert_2x e1 e2 (fun x y -> FDiv (x,y))), Float

    | Not e -> (insert_let (norm e env) (fun x -> Not x)), Bool
    | Eq (e1, e2) -> (insert_2x e1 e2 (fun x y -> Eq (x,y))), Bool
    | LE (e1, e2) -> (insert_2x e1 e2 (fun x y -> LE (x,y))), Bool
    | If (e1, e2, e3) -> 
      let (k2,_) = norm e2 env and (k3,t) = norm e3 env in
        (insert_let (norm e1 env) (fun x -> If(x, k2, k3))), t

    | Let (id, e1 , e2) ->
      let (k1, t1) = norm e1 env in
        let (k2, t2) = norm e2 (id::env) in
          Let(id, k1, k2), t2

    | LetRec (fd, e) -> 
      let e1,t1 = norm fd.body (fd.name::fd.args @ env) in
        let e2,t2 = norm e (fd.name::env) in
        let _ = Printf.printf "toto : %s\n" (Syntax.infix_to_string (fun (x,y) -> Id.to_string x) (fd.name::fd.args @ env) " " ) in
          LetRec ({name = fd.name; args = fd.args; body = e1}, e2), t2
    | App (e, le) ->
      let (e',t') = norm e env in
        (match Type.simplify t' with
        | Fun(l, t) ->
          insert_let (e',t') (fun x -> (insert_nx le (fun lx -> App(x,lx)))),t
        | _ -> failwith "knormalization failed : App with non functional type")

    | LetTuple (l, e1, e2) -> 
      let k,t = norm e2 (l @ env) in
        (insert_let (norm e1 env) (fun x -> LetTuple(l, x, k))), t
    | Tuple l -> insert_nx l (fun l -> Tuple l), Tuple (List.map (fun x -> snd (norm x env)) l)

    | Array (e1, e2) ->
      let k2,t2 = norm e2 env in
        insert_let (norm e1 env) (fun x -> insert_let (k2,t2) (fun y -> Array (x, y))),Array t2
    | Get (e1, e2) -> 
      let k1,t1 = norm e1 env in
        (match Type.simplify t1 with
        | Array t -> 
          insert_let (k1,t1) (fun x -> insert_let (norm e2 env) (fun y -> Get(x, y))), t
        | _ -> failwith "knormalization failed : Get of non array type")
    | Put (e1, e2, e3) ->
      let k1,t1 = norm e1 env in
        (match Type.simplify t1 with
        | Array t -> insert_let (k1,t1) (fun x -> insert_2x e2 e3 (fun y z -> Put(x, y, z))), t
        | _ -> failwith "knormalization failed : Put of non array type")
    )
  
  in let res,_ = norm exp Typechecker.predef in res


(* fonctions to_string : dans l'idée de Syntax.to_string *)
let rec to_string_rec (with_type:bool) (k:knorm_t) : string =
  let to_string_rec = to_string_rec with_type in
  match k with
  | Var b -> Id.to_string b
  | Unit -> "()"
  | Bool b -> if b then "true" else "false"
  | Eq (x,y) -> sprintf "(%s = %s)" (Id.to_string x) (Id.to_string y) 
  | LE (x,y) -> sprintf "(%s <= %s)" (Id.to_string x) (Id.to_string y) 
  | Int i -> string_of_int i
  | Float f -> sprintf "%.2f" f
  | Not b -> sprintf "(not %s)" (Id.to_string b)
  | Neg b -> sprintf "(- %s)" (Id.to_string b)
  | Add (b1, b2) -> sprintf "(%s + %s)" (Id.to_string b1) (Id.to_string b2)
  | Sub (b1, b2) -> sprintf "(%s - %s)" (Id.to_string b1) (Id.to_string b2)
  | FNeg b -> sprintf "(-. %s)" (Id.to_string b)
  | FAdd (b1, b2) -> sprintf "(%s +. %s)" (Id.to_string b1) (Id.to_string b2)
  | FSub (b1, b2) -> sprintf "(%s -. %s)" (Id.to_string b1) (Id.to_string b2)
  | FMul (b1, b2) -> sprintf "(%s *. %s)" (Id.to_string b1) (Id.to_string b2)
  | FDiv (b1, b2) -> sprintf "(%s /. %s)" (Id.to_string b1) (Id.to_string b2)
  | If (b, k1, k2) -> sprintf "(if %s then %s else %s)" 
    (Id.to_string b) (to_string_rec k1) (to_string_rec k2)
  | Let ((id,t), k1, k2) -> sprintf "(let %s%s = %s in \n%s)"
    (Id.to_string id) (if with_type then ":"^Type.to_string2 t else "")
    (to_string_rec k1) (to_string_rec k2)
  | LetRec (fd, e) -> sprintf "(let rec %s%s %s = %s in \n%s)"
    (Id.to_string (fst fd.name))
    (if with_type then ":" ^ (Type.to_string2 (snd fd.name)) else "")
    (Syntax.infix_to_string (fun x -> Id.to_string (fst x)) fd.args " ")
    (to_string_rec fd.body)
    (to_string_rec e)
  | App (id, l) -> sprintf "(%s %s)" 
    (Id.to_string id) (Syntax.infix_to_string Id.to_string l " ")
  | LetTuple (args, var, e) -> sprintf "let (%s)%s = %s in \n%s"
    (Syntax.infix_to_string (fun x -> Id.to_string (fst x)) args ",")
    (if with_type then ":tuple(" ^ (Syntax.infix_to_string (fun (_, y) -> Type.to_string2 y) args " ") ^ ")" else "")
    (Id.to_string var)
    (to_string_rec e)
  | Tuple values -> sprintf "(%s)" (Syntax.infix_to_string Id.to_string values ",")
  | Array (b1, b2) -> sprintf "(Array.create %s %s)" (Id.to_string b1) (Id.to_string b2)
  | Get (id, b) -> sprintf "(%s.(%s))" (Id.to_string id) (Id.to_string b)
  | Put (id, b1, b2) -> sprintf "(%s.(%s) <- %s)"
    (Id.to_string id) (Id.to_string b1) (Id.to_string b2)

let to_string = to_string_rec false
let to_string_with_type = to_string_rec true