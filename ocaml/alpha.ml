(*
alpha.ml

date : 21-12-2023
*)

(* epsilon is the mapping between old and new variable's name *)
type epsilon = (Id.t * Id.t) list

(* Apply epsilon in var name *)
(* If var is in env, apply returns associated name in env else returns var *)
let apply (env:epsilon) (var:Id.t) : Id.t =
  try 
    snd (List.find (fun (x,y) -> x = var) env)
  with Not_found -> var

(* replace all definition by new fresh variable *)
(* The given expression a MUST BE in k-normal form *)
let rec alpha_conversion (env:epsilon) (a:Syntax.t) : Syntax.t = 
  match a with
  (* nothing to do *)
  | Unit -> a
  | Int i -> a
  | Float f -> a
  | Bool b -> a

  (* only recursive calls *)
  | Not e -> Not (alpha_conversion env e)
  | Neg e -> Neg (alpha_conversion env e)
  | Add (e1,e2) -> 
    let e12 = alpha_conversion env e1 and e22 = alpha_conversion env e2 in Add (e12, e22)
  | Sub (e1,e2) -> 
    let e12 = alpha_conversion env e1 and e22 = alpha_conversion env e2 in Sub (e12, e22)
  | FNeg e -> FNeg (alpha_conversion env e)
  | FAdd (e1,e2) ->
    let e12 = alpha_conversion env e1 and e22 = alpha_conversion env e2 in FAdd (e12, e22)
  | FSub (e1,e2) -> 
    let e12 = alpha_conversion env e1 and e22 = alpha_conversion env e2 in FSub (e12, e22)
  | FMul (e1,e2) -> 
    let e12 = alpha_conversion env e1 and e22 = alpha_conversion env e2 in FMul (e12, e22)
  | FDiv (e1,e2) -> 
    let e12 = alpha_conversion env e1 and e22 = alpha_conversion env e2 in FDiv (e12, e22)
  | Eq (e1,e2) -> 
    let e12 = alpha_conversion env e1 and e22 = alpha_conversion env e2 in Eq (e12, e22)
  | LE (e1,e2) -> 
    let e12 = alpha_conversion env e1 and e22 = alpha_conversion env e2 in LE (e12, e22)
  | If (e1,e2,e3) -> 
    let e12 = alpha_conversion env e1 and e22 = alpha_conversion env e2 in
      let e32 = alpha_conversion env e3 in If (e12, e22, e32)
  | App (e1, le2) -> 
    let e12 = alpha_conversion env e1 in
      let le22 = List.map (alpha_conversion env) le2 in
        App(e12, le22)
  | Get(e1, e2) -> 
    let e12 = alpha_conversion env e1 and e22 = alpha_conversion env e2 in Get (e12, e22) 
  | Put(e1, e2, e3) -> 
    let e12 = alpha_conversion env e1 and e22 = alpha_conversion env e2 in 
      let e32 = alpha_conversion env e3 in Put(e12, e22, e32)
  | Tuple(l) -> Tuple (List.map (alpha_conversion env) l)
  | Array(e1,e2) -> 
    let e12 = alpha_conversion env e1 and e22 = alpha_conversion env e2 in Array (e12, e22)

  (* Creates new fresh variables and changes epsilon mapping *)
  | Let ((s,t),e2,e3) -> 
    let newvar = Id.genid () in
      let env2 = (s, newvar)::env in
        let e22 = alpha_conversion env e2 and e32 = alpha_conversion env2 e3 in
          Let ((newvar,t), e22, e32)
  | LetRec (fd, e) ->
    let newname = Id.genid () and newargs = Id.genid_list (List.length fd.args) in
      let env2 = (fst fd.name, newname)::(List.map2 (fun x y -> (fst x,y)) fd.args newargs) @ env in
        let newbody = alpha_conversion env2 fd.body in
          let e2 = alpha_conversion ((fst fd.name, newname)::env) e in
            let args = List.map2 (fun (s1,t) s2 -> (s2,t)) fd.args newargs in
              LetRec ({name = (newname, snd fd.name); args = args; body = newbody}, e2)
  | LetTuple (l, e1, e2) -> 
    let l2 = Id.genid_list (List.length l) and e12 = alpha_conversion env e1 in
      let env2 = (List.map2 (fun x y -> (fst x,y)) l l2) @ env in
        let e22 = alpha_conversion env2 e2 in
          LetTuple (List.map2 (fun (s1,t) s2 -> (s2,t)) l l2, e12, e22)

  (* find in environment *)
  | Var s -> Var (apply env s)

(* Applying of alpha-conversion on ast *)
(* The given ast MUST BE in k-normal form *)
let conversion (knorm_ast:Syntax.t) : Syntax.t =  
  alpha_conversion [] knorm_ast
