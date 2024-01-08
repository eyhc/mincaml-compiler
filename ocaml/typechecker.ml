(*
typechecker.ml

date : 05-01-2024
*)


(************************
    TYPES & GLOBAL VAR
 ************************)

(* an equation is a pair of type *)
type equation = Type.t * Type.t

(* an environment is a list of pairs of types by identifiers *)
type environment = (Id.t * Type.t) list


(* default environment *)
let predef:environment = [
  ("print_int", Type.Fun ([Type.Int], Type.Unit)); 
  ("print_float", Type.Fun ([Type.Float], Type.Unit))
]


(************************
   EQUATIONS GENERATION
 ************************)

(* return the string representation of a list of equations *)
let rec to_string (x:equation list) : string =
  match x with
  | [] -> ""
  | e::l -> let (t1,t2) = e in
    Printf.sprintf "%s = %s ; %s" (Type.to_string t1) (Type.to_string t2) (to_string l)


(* returns a list of n new Var type *)
let new_list_var (n:int) : Type.t list =
  List.init n (fun i -> Type.gentyp ())

(* equations generation for type analysis *)
let rec gen_equations (expr:Syntax.t) (env:environment) (wanted:Type.t) : equation list =
  match expr with
  | Unit -> [ (Type.Unit, wanted) ]
  | Bool b -> [ (Type.Bool, wanted) ]
  | Int i -> [ (Type.Int, wanted) ]
  | Float f -> [ (Type.Float, wanted) ]

  | Not e -> let eq = gen_equations e env Bool in
    (Type.Bool, wanted) :: eq
  | Neg e -> let eq = gen_equations e env Int in
    (Type.Int, wanted) :: eq
  | Add (e1, e2) -> 
    let eq1 = gen_equations e1 env Int and eq2 = gen_equations e2 env Int in
      (Type.Int, wanted) :: eq1 @ eq2
  | Sub (e1, e2) -> 
    let eq1 = gen_equations e1 env Int and eq2 = gen_equations e2 env Int in
      (Type.Int, wanted) :: eq1 @ eq2
  | FNeg e -> 
    let eq = gen_equations e env Float in
      (Type.Float, wanted) :: eq
  | FAdd (e1, e2) ->
    let eq1 = gen_equations e1 env Float and eq2 = gen_equations e2 env Float in
      (Type.Float, wanted) :: eq1 @ eq2
  | FSub (e1, e2) ->
    let eq1 = gen_equations e1 env Float and eq2 = gen_equations e2 env Float in
      (Type.Float, wanted) :: eq1 @ eq2
  | FMul (e1, e2) ->
    let eq1 = gen_equations e1 env Float and eq2 = gen_equations e2 env Float in
      (Type.Float, wanted) :: eq1 @ eq2
  | FDiv (e1, e2) ->
    let eq1 = gen_equations e1 env Float and eq2 = gen_equations e2 env Float in
      (Type.Float, wanted) :: eq1 @ eq2

  | Eq (e1, e2) -> 
    let t = Type.gentyp () in
      let eq1 = gen_equations e1 env t and eq2 = gen_equations e2 env t
        in (wanted, Type.Bool) :: eq1 @ eq2
  | LE (e1, e2) -> 
    let t = Type.gentyp () in
      let eq1 = gen_equations e1 env t and eq2 = gen_equations e2 env t
        in (wanted, Type.Bool) :: eq1 @ eq2
  
  | Var id -> 
    (try
      let _,t1 = List.find (fun (x,y) -> x = id) env in [(t1, wanted)]
    with Not_found -> failwith (Printf.sprintf "Var %s not found" id))

  | If (e1, e2, e3) -> 
    let eq1 = gen_equations e1 env Type.Bool in
      let eq2 = gen_equations e2 env wanted and eq3 = gen_equations e3 env wanted in
        eq1 @ eq2 @ eq3

  | Let ((id,t), e1, e2) -> 
    let eq1 = gen_equations e1 env t and eq2 = gen_equations e2 ((id,t)::env) wanted in
      eq1 @ eq2

  | LetRec (fd, e) -> 
    let t = Type.gentyp () in
      let eq1 = gen_equations fd.body (fd.name::fd.args @ env) t in
        let ftype = Type.Fun(List.map snd fd.args, t) in
        let eq2 = gen_equations e ((fst fd.name, ftype)::env) wanted in
          (snd fd.name, ftype) :: eq1 @ eq2
  | App (e1, le2) -> 
    let t = Type.gentyp () and n = List.length le2 in
      let vars = new_list_var n in
        let eq1 = gen_equations e1 env t  in
          let leq2 = List.fold_left2 (fun acc x y -> (gen_equations y env x) @ acc) [] vars le2
              in  (t, Type.Fun (vars , wanted)) :: eq1 @ leq2

  | LetTuple (l, e1, e2) ->
    let eq1 = gen_equations e1 env (Type.Tuple (List.map snd l)) in
      let eq2 = gen_equations e2 (l @ env) wanted in
        eq1 @ eq2
  | Tuple(l) ->
    let n = List.length l in
      let vars = new_list_var n in
        (Type.Tuple (vars), wanted) :: (List.fold_left2 (fun acc e t -> (gen_equations e env t) @ acc) [] l vars)
  
  | Array(e1,e2) -> 
    let t = Type.gentyp () in
      let eq1 = gen_equations e1 env Type.Int and eq2 = gen_equations e2 env t in
        (Type.Array t, wanted) :: eq1 @ eq2
  | Get(e1, e2) -> 
    let eq1 = gen_equations e1 env (Type.Array wanted) and eq2 = gen_equations e2 env Type.Int in
      eq1 @ eq2
  | Put(e1, e2, e3) -> 
    let t = Type.gentyp () in
      let eq1 = gen_equations e1 env (Type.Array t) in
        let eq2 = gen_equations e2 env Type.Int in
          let eq3 = gen_equations e3 env t in
            (Type.Unit, wanted) :: eq1 @ eq2 @ eq3



(*************************
   EQUATIONS UNIFICATION
 *************************)

(* replace t or it componenets by t2 if it's the same than t1 *)
let rec replace_rec (t:Type.t)  (t1:Type.t) (t2:Type.t) : Type.t =
  match t with
  | Unit | Bool | Float | Int -> t
  | Fun (l, t3) -> Fun (List.map (fun x -> (replace_rec x t1 t2)) l, (replace_rec t3 t1 t2))
  | Tuple l -> Tuple (List.map (fun x -> (replace_rec x t1 t2)) l)
  | Array t3 -> if Type.is_same t3 t1 then Array(t2) else t
  | Var v -> if Type.is_same t t1 then t2 else t

(* replace each occurence of t1 by t2 in the equation list *)
(* bool left indicates if replacement is applied in left member of equation or not *)
let rec replace (left:bool) (l:equation list) (t1:Type.t) (t2:Type.t) : equation list =
    match l with
    | [] -> []
    | (e1,e2)::l1 ->
      if left then
        (replace_rec e1 t1 t2, replace_rec e2 t1 t2)::(replace left l1 t1 t2)
      else
        (e1, replace_rec e2 t1 t2)::(replace left l1 t1 t2)

(* checks if var is t or is contained in t *)
let rec occur (t:Type.t) (var:Type.t) : bool =
  let fold_fct acc t = (acc || (occur t var)) in
    match t with
    | Unit | Int | Bool | Float -> false
    | Fun (l, t3) -> (List.fold_left fold_fct false l) || (occur t3 var)
    | Tuple l -> List.fold_left fold_fct false l
    | Array t3 -> occur t3 var
    | Var v -> Type.is_same t var


(* equations system solver
 * uses unification algorithm *)
(* cf https://wackb.gricad-pages.univ-grenoble-alpes.fr/inf402/cours10_En.pdf slides 21 & 22 *)
let rec resolution (el:equation list) : equation list =
  match el with
  | [] -> []
  | (t1,t2)::l ->
    if Type.is_same t1 t2 then resolution l (* Remove the useless equation *)
    else
      match t1,t2 with

      (* Decompose & failure of decomposition *)
      | Fun (l3, t3), Fun (l4, t4) -> 
        (try
          let r = (t3,t4)::(List.map2 (fun x y -> (x,y)) l3 l4) 
            in resolution (r @ l)
        with Invalid_argument e -> failwith "not unifiable : failure of decomposition (function)")
      | Tuple l1, Tuple l2 ->
        (try let r = List.map2 (fun x y -> (x,y)) l1 l2
          in resolution (r @ l)
        with Invalid_argument e -> failwith "not unifiable : failure of decomposition (tuple)")
      | Array t3, Array t4 -> resolution ((t3,t4)::l)

      (* Elimination of variable & failure of elimination *)
      | Var v,t ->
        if not (occur t t1) then
          let l3 = replace true l t1 t in (t1, t)::resolution l3
        else failwith "not unifiable : failure of elimination"

      (* Orient *)
      | t,Var v -> resolution ((Var v, t)::l)

      (* Failure of decomposition *)
      | _ -> failwith "not unifiable : failure of decomposition"


(* remove Var on right member of equation by substitution *)
let rec subsitution (eq:equation list) : equation list =
  List.fold_left (fun l (t1, t2) -> replace false l t1 t2) eq eq


(*************************
  SET TYPES IN AST & MAIN
 *************************)

(* Set infered types on the ast  *)
let rec set_types (eq:equation list) : unit =
  let iter_fct eq =  (match eq with (Type.Var v, t2) -> v := Some (t2) | _ -> assert false) in
    List.iter iter_fct eq


(* Main function for type inference
 * -> generates type's equation
 * -> solve these equations
 * -> set infered types in the given AST
 *)
let type_check (ast:Syntax.t) : unit = 
  let eq = gen_equations ast predef Unit in
    (* print_endline (to_string eq); *)
    let sub = subsitution (resolution eq) in
      (* print_endline (to_string sub); *)
      set_types sub;