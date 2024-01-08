(*
alpha.ml

date : 21-12-2023
*)

(* epsilon is the mapping between old and new variable's name *)
type epsilon = (Id.t * Id.t) list

(* Apply epsilon in var name *)
(* If var is in env, apply returns associated name in env else returns var *)
let apply (env:epsilon) (var:Id.t) : Id.t =
  try snd (List.find (fun (x,y) -> x = var) env)
  with Not_found -> var

(* replace all definition by new fresh variable *)
(* The given expression a MUST BE in k-normal form *)
let rec alpha_conversion (env:epsilon) (a:Knorm.knorm_t) : Knorm.knorm_t =
  let eps_apply = apply env in
  match a with
  (* nothing to do *)
  | Unit -> a
  | Int i -> a
  | Float f -> a
  | Bool b -> a

  (* find in environment *)
  | Var s -> Var (eps_apply s)

  (* no recursive calls *)
  | Not x -> Not (eps_apply x)
  | Neg x -> Neg (eps_apply x)
  | Add (x, y) -> Add (eps_apply x, eps_apply y)
  | Sub (x, y) -> Sub (eps_apply x, eps_apply y)
  | FNeg x -> FNeg (eps_apply x)
  | FAdd (x, y) -> FAdd (eps_apply x, eps_apply y)
  | FSub (x, y) -> FSub (eps_apply x, eps_apply y)
  | FMul (x, y) -> FMul (eps_apply x, eps_apply y)
  | FDiv (x, y) -> FDiv (eps_apply x, eps_apply y)
  | App (f, vars) -> App (eps_apply f, List.map eps_apply vars)
  | Get (x, y) -> Get (eps_apply x, eps_apply y)
  | Put (x, y, z) -> Put (eps_apply x, eps_apply y, eps_apply z)
  | Tuple(l) -> Tuple (List.map eps_apply l)
  | Array(x,y) -> Array (eps_apply x, eps_apply y)

  (* with recursive calls *)
  | If (b,e1,e2) -> 
    let e12 = alpha_conversion env e1 and e22 = alpha_conversion env e2 in(
      match b with
      | Eq (x, y) -> If (Eq (eps_apply x, eps_apply y), e12, e22)
      | LE (x, y) -> If (LE (eps_apply x, eps_apply y), e12, e22))

  (* Creates new fresh variables and changes epsilon mapping *)
  | Let ((s,t), e1, e2) -> 
    let newvar = Id.make_unique s in
      let env2 = (s, newvar)::env in
        let e12 = alpha_conversion env e1 and e22 = alpha_conversion env2 e2 in
          Let ((newvar, t), e12, e22)
  | LetRec (fd, e) ->
    let newname = Id.make_unique (fst fd.name) in
      let newargs = List.map (fun x -> Id.make_unique (fst x)) fd.args in
        let env2 = (List.map2 (fun x y -> (fst x,y)) fd.args newargs) @ env in
          let newbody = alpha_conversion env2 fd.body in
            let e2 = alpha_conversion ((fst fd.name, newname)::env) e in
              let args = List.map2 (fun (s1,t) s2 -> (s2,t)) fd.args newargs in
                LetRec ({name = (newname, snd fd.name); args = args; body = newbody}, e2)
  | LetTuple (l1, l2, e1) -> 
    let l1' = List.map (fun x -> Id.make_unique (fst x)) l1 in
        let env2 = (List.map2 (fun x y -> (fst x, y)) l1 l1') @ env in
          let e12 = alpha_conversion env2 e1 in
            LetTuple (List.map2 (fun (s1,t) s2 -> (s2,t)) l1 l1', List.map eps_apply l2, e12)


(* Applying of alpha-conversion on ast *)
(* The given ast MUST BE in k-normal form *)
let conversion (knorm_ast:Knorm.knorm_t) : Knorm.knorm_t =  
  alpha_conversion [] knorm_ast
