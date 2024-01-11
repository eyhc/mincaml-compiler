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
  | Neg of Id.t
  | Add of Id.t * Id.t
  | Sub of Id.t * Id.t
  | FNeg of Id.t
  | FAdd of Id.t * Id.t
  | FSub of Id.t * Id.t
  | FMul of Id.t * Id.t
  | FDiv of Id.t * Id.t
  | IfEq of (Id.t * Id.t)  * knorm_t * knorm_t
  | IfLE of (Id.t * Id.t)  * knorm_t * knorm_t
  | Let of (Id.t * Type.t) * knorm_t * knorm_t
  | LetRec of fundef * knorm_t
  | App of Id.t * Id.t list
  | LetTuple of (Id.t * Type.t) list * Id.t * knorm_t
  | Tuple of Id.t list
  | Array of Id.t * Id.t
  | Get of Id.t * Id.t
  | Put of Id.t * Id.t * Id.t
and fundef = {name : Id.t * Type.t; args : (Id.t * Type.t) list; body : knorm_t }


(* détermine le type d'une expression dans l'environnement env *)
let rec get_type (ast:knorm_t) (env:(Id.t * Type.t) list) : Type.t =
  match ast with
  | Unit -> Unit
  | Int _ | Add _ | Sub _ | Neg _ -> Int
  | Float _ | FAdd _ | FSub _ | FMul _ | FDiv _ | FNeg _ -> Float

  | Var id ->
    (try
      (snd (List.find (fun (x,y) -> x = id) env))
    with Not_found -> failwith (sprintf "Knorm failed -> Not found var : %s" id))

  | IfEq (c, e1, e2) -> get_type e2 env
  | IfLE (c, e1, e2) -> get_type e2 env
  | Let (id, e1 , e2) -> get_type e2 (id::env)

  | LetRec (fd, e) -> get_type e (fd.name::env)
  | App (f, args) -> 
    let t = get_type (Var f) env in
      (match Type.simplify t with 
      | Fun(_,t) -> t
      | _ -> failwith (sprintf "Knorm failed -> %s is not a function" f))

  | LetTuple (l, v, e) -> get_type e (l @ env)
  | Tuple l -> Tuple(List.map (fun x -> get_type (Var x) env) l)

  | Array (x, y) -> Array(get_type (Var y) env)
  | Get (x, y) -> 
    let t = get_type (Var x) env in
      (match Type.simplify t with
      | Array(t') -> t'
      | _ -> failwith (sprintf "Knorm failed -> %s is not an array" x))
  | Put (x, y, z) -> Unit


(* insertion d'une expression let si besoin
 * pour une expression k-normalisé e de type t
 * où l'application de k sur une nouvelle variable x (Id.t) 
 * donne une expression e' (k-normalisé) dans le pattern (let x:t = e in e')
 *)
let insert_let (e:knorm_t) (env:(Id.t * Type.t) list) (k:Id.t -> knorm_t) : knorm_t =
  match e with
  | Var x -> k x
  | _ ->
    let x = Id.genid () in
      let e' = k x in
        Let((x, get_type e env), e, e')


(***************************************************************)
(* Fonction principale de normalisation *)

(* CF equations de la section 4.3 de l'article *)
let normalize (exp:Syntax.t) : knorm_t =
  let rec norm (exp:Syntax.t) (env:(Id.t * Type.t) list) : knorm_t =
  (* applique deux fois insert_let *)
  let insert_2x e1 e2 f = insert_let (norm e1 env) env
    (fun x -> 
      let k = norm e2 env in insert_let k env (fun y -> f x y))
  in

  (* applique insert let sur une liste *)
  let insert_nx (le:Syntax.t list) (f:Id.t list -> knorm_t) : knorm_t =
    (* le : liste d'expression non traitées; res liste de variables (correspondantes) *)
    let rec insert_nx_rec le f res =
      (match le with
      | [] -> f (List.rev res) (* res est dans le mauvais ordre *)
      | e::l -> let k = norm e env in
        insert_let k env (fun xi -> insert_nx_rec l f (xi::res)))
    in insert_nx_rec le f []
  in

  match exp with
  | Unit -> Unit
  | Int i -> Int i
  | Float f -> Float f
  | Bool b -> Int (Bool.to_int b)
  | Var id -> Var id

  | Add (e1, e2) -> insert_2x e1 e2 (fun x y -> Add (x,y))
  | Sub (e1, e2) -> insert_2x e1 e2 (fun x y -> Sub (x,y))
  | Neg e -> 
    let k = norm e env in insert_let k env (fun x -> Neg x)

  | FNeg e -> 
    let k = norm e env in insert_let k env (fun x -> FNeg x)
  | FAdd (e1, e2) -> insert_2x e1 e2 (fun x y -> FAdd (x,y))
  | FSub (e1, e2) -> insert_2x e1 e2 (fun x y -> FSub (x,y))
  | FMul (e1, e2) -> insert_2x e1 e2 (fun x y -> FMul (x,y))
  | FDiv (e1, e2) -> insert_2x e1 e2 (fun x y -> FDiv (x,y))

  | Not e -> norm (If (Eq(e, Bool true), Bool false, Bool true)) env
  | Eq (e1, e2) -> norm (If(exp, Bool true, Bool false)) env
  | LE (e1, e2) -> norm (If(exp, Bool true, Bool false)) env
  | If (e1, e2, e3) -> 
    (match e1 with
    | Not x -> norm (If(x, e3, e2)) env
    | Eq (e11, e12) -> 
      let k2 = norm e2 env and k3 = norm e3 env in 
        insert_2x e11 e12 (fun x y -> IfEq((x,y), k2, k3))
    | LE (e11, e12) ->
      let k2 = norm e2 env and k3 = norm e3 env in 
        insert_2x e11 e12 (fun x y -> IfLE((x,y), k2, k3))
    | _ -> norm (If (Eq (e1, Bool false), e3, e2)) env)

  | Let (id, e1 , e2) -> Let(id, norm e1 env, norm e2 (id::env))

  | LetRec (fd, e) -> 
    let kbody = norm fd.body (fd.name::fd.args @ env) in
      let ke = norm e (fd.name::env) in
        LetRec ({name = fd.name; args = fd.args; body = kbody}, ke)
  | App (e, le) -> 
    let k = norm e env in
      insert_let k env (fun x -> insert_nx le (fun l -> App(x, l)))

  | LetTuple (l, e1, e2) -> 
    let k1 = norm e1 env and k2 = norm e2 (l @ env) in
      insert_let k1 env (fun x -> LetTuple (l, x, k2))
  | Tuple l -> insert_nx l (fun l -> Tuple(l))

  | Array (e1, e2) -> insert_2x e1 e2 (fun x y -> Array (x, y))
  | Get (e1, e2) -> insert_2x e1 e2 (fun x y -> Get (x, y))
  | Put (e1, e2, e3) ->
    let k1 = norm e1 env in
      insert_let k1 env (fun x -> insert_2x e2 e3 (fun y z -> Put(x, y, z)))
  in norm exp Typechecker.predef


(*********************************************************)
(* fonctions to_string : dans l'idée de Syntax.to_string *)
let rec to_string_rec (with_type:bool) ?(p: string="") (k:knorm_t) : string =
  let to_string_rec = to_string_rec with_type in
  let tab = "  " in
  let arithemic_op_to_string (p: string) (a: Id.t) (b: Id.t) (op: string): string =
    sprintf "%s%s %s %s" p (Id.to_string a) op (Id.to_string b)
  in
  let if_to_string (p: string) (a: Id.t) (b: Id.t) (op: string) (th: knorm_t) (els: knorm_t): string =
    sprintf "%sif %s %s %s then\n%s\n%selse\n%s" p a op b (to_string_rec ~p:(p^tab) th) p (to_string_rec ~p:(p^tab) els)
  in
  match k with
  | Var b -> Id.to_string b
  | Unit -> "()"
  | Int i -> string_of_int i
  | Float f -> sprintf "%.2f" f
  | Neg b -> sprintf "(- %s)" (Id.to_string b)
  | Add (b1, b2) -> arithemic_op_to_string p b1 b2 "+"
  | Sub (b1, b2) -> arithemic_op_to_string p b1 b2 "-"
  | FNeg b -> sprintf "(-. %s)" (Id.to_string b)
  | FAdd (b1, b2) -> arithemic_op_to_string p b1 b2 "+."
  | FSub (b1, b2) -> arithemic_op_to_string p b1 b2 "-."
  | FMul (b1, b2) -> arithemic_op_to_string p b1 b2 "*."
  | FDiv (b1, b2) -> arithemic_op_to_string p b1 b2 "/."
  | IfEq ((x, y), k1, k2) -> if_to_string p x y "=" k1 k2
  | IfLE ((x, y), k1, k2) -> if_to_string p x y "<=" k1 k2
  | Let ((id,t), k1, k2) -> sprintf "%slet %s%s = %s in\n%s"
    p (Id.to_string id) (if with_type then ":"^Type.to_string2 t else "")
    (to_string_rec k1) (to_string_rec ~p:p k2)
  | LetRec (fd, e) -> sprintf "let rec %s%s %s =\n%s\nin\n%s"
    (Id.to_string (fst fd.name))
    (if with_type then ":" ^ (Type.to_string2 (snd fd.name)) else "")
    (Syntax.infix_to_string (fun x -> Id.to_string (fst x)) fd.args " ")
    (to_string_rec ~p:(p^tab) fd.body)
    (to_string_rec e)
  | App (id, l) -> sprintf "%s%s %s" 
    p (Id.to_string id) (Syntax.infix_to_string Id.to_string l " ")
  | LetTuple (args, var, e) -> sprintf "%slet (%s)%s = %s in \n%s"
    p
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