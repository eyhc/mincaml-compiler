(*
typechecker.ml

date : 21-12-2023
authors : Carrot Elie

TODO :
  - gen_equations : case todo
  - default type  : int in substitution
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

(* return the string representation of an equations list *)
let rec to_string (x:equation list) : string =
  match x with
  | [] -> ""
  | e::l -> let (t1,t2) = e in
    Printf.sprintf "%s = %s ; %s" (Type.to_string t1) (Type.to_string t2) (to_string l)


(* equations generation for type analysis *)
let rec gen_equations (expr:Syntax.t) (env:environment) (wanted:Type.t) : equation list =
  match expr with
  | Unit -> [ (Unit, wanted) ]
  | Bool b -> [ (Bool, wanted) ]
  | Int i -> [ (Int, wanted) ]
  | Float f -> [ (Float, wanted) ]
  | Not e -> let eq = gen_equations e env Bool in
    (Bool, wanted) :: eq
  | Neg e -> let eq = gen_equations e env Int in
    (Int, wanted) :: eq
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
        in (wanted, Type.Bool)::eq1 @ eq2
  | LE (e1, e2) -> 
    let t = Type.gentyp () in
      let eq1 = gen_equations e1 env t and eq2 = gen_equations e2 env t
        in (wanted, Type.Bool)::eq1 @ eq2
  | If (e1, e2, e3) -> 
    let eq1 = gen_equations e1 env Type.Bool in
      let eq2 = gen_equations e2 env wanted and eq3 = gen_equations e3 env wanted in
        eq1 @ eq2 @ eq3
  | Let ((id,t), e1, e2) -> 
    let eq1 = gen_equations e1 env t and eq2 = gen_equations e2 ((id,t)::env) wanted in
      eq1 @ eq2
  | Var id -> 
    (try
      let _,t1 = List.find (fun (x,y) -> x = id) env in [(t1, wanted)]
    with Not_found -> failwith (Printf.sprintf "Var %s not found" id))
  | App (e1, le2) -> 
    let t1 = Type.gentyp () and t2 = Type.gentyp () in
      let eq1 = gen_equations e1 env t1 in
        (match le2 with
        | [] -> failwith "impossible"
        | e2::l2 ->
          if (l2 <> []) then failwith "App : multi params : todo";
          let eq2 = gen_equations e2 env t2
            in  (t1, Type.Fun ([t2] , wanted)) :: eq1 @ eq2)
  | LetRec (fd, e) -> failwith "todo"
  | LetTuple (l, e1, e2)-> failwith "todo"
  | Get(e1, e2) -> failwith "todo"
  | Put(e1, e2, e3) -> failwith "todo"
  | Tuple(l) -> failwith "todo"
  | Array(e1,e2) -> failwith "todo"


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
(* bool l indicates if replacement is applied in left member of equation or not *)
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
  let fold_fct acc x = (acc && (occur x var)) in
    match t with
    | Unit | Int | Bool | Float -> false
    | Fun (l, t3) -> (List.fold_left fold_fct false l) && (occur t3 var)
    | Tuple l -> List.fold_left fold_fct false l
    | Array t3 -> occur t3 var
    | Var v -> Type.is_same t var


(* solver equation system
 * uses unification algorithm
 *)
let rec resolution (el:equation list) : equation list =
  let notunifiable = fun () -> failwith "not unifiable" in
  match el with
  | [] -> []
  | (t1,t2)::l ->
    match t1,t2 with
      (* Remove the equation *)
      | Unit,Unit | Bool,Bool | Int,Int | Float,Float -> resolution l

      (* Decompose & failure of decomposition *)
      | Fun (l1, t1), Fun (l2, t2) -> 
        (try
          let r = (t1,t2)::(List.map2 (fun x y -> (x,y)) l1 l2) 
            in resolution (r @ l)
        with Invalid_argument e -> notunifiable ())
      | Tuple l1, Tuple l2 ->
        (try let r = List.map2 (fun x y -> (x,y)) l1 l2
          in resolution (r @ l)
        with Invalid_argument e -> notunifiable ())
      | Array t1, Array t2 -> resolution ((t1,t2)::l)

      (* Orient *)
      | t,Var v -> resolution ((Var v, t)::l)

      (* Elimination of variable & failure of elimination *)
      | Var v,t ->
        if not (occur t (Var v)) then
          let l3 = replace true l (Var v) t in
            (Var v, t)::resolution l3
        else notunifiable ()

      (* Failure of decomposition *)
      | _ -> notunifiable ()


(* remove all Var on right member of equation by substitution *)
let rec subsitution (eq:equation list) : equation list =
  let temp = List.fold_left (fun l (t1, t2) -> replace false l t1 t2) eq eq in
    temp


(*************************
  SET TYPES IN AST & MAIN
 *************************)

(* Set infered types on the ast  *)
let rec set_types (eq:equation list) : unit =
  match eq with
  | [] -> ()
  | (Var v, t2)::l -> v := Some (t2); set_types l
  | _ -> failwith "non an solution list"
  

(* Main function for type inference
 * -> generates type's equation
 * -> solve these equations
 * -> set infered types in the given AST
 *)
let type_check (ast:Syntax.t) : unit = 
  let eq = gen_equations ast predef Unit in
    let sub = subsitution (resolution eq) in
      set_types sub;
      print_endline "Type inference : OK";
