(*
elim.ml
Elimination of unnecessary definitions

date : 09-01-2023
*)

(* return true iff expression contains side effect *)
(* env is the list of functions with side effects *)
let rec has_side_effects (a:Knorm.knorm_t) (env:Id.t list) : bool =
  match a with
  | App (f, vars) -> List.exists (fun x -> x = f) env
  | Put _ -> true
  | IfEq (_,e1,e2) | IfLE (_,e1,e2) | Let (_, e1, e2) ->
    (has_side_effects e1 env) || (has_side_effects e2 env) 
  | LetRec (fd, e) -> 
    if has_side_effects fd.body env then
      let env2 = (fst fd.name)::env in
        has_side_effects e env2
    else has_side_effects e env
  | LetTuple (l1, v, e) -> has_side_effects e env
  | _ -> false


type environment = (Id.t * int ref) list

(* increment of var v in env *)
let rec env_incr (env:environment) (v:Id.t) : unit =
  match env with
  | [] -> ()
  | (x,i)::l -> let _ = if x = v then incr i in env_incr l v

let rec env_get (env:environment) (v:Id.t) : int =
  match env with
  | [] -> 0
  | (x,i)::l ->
    if x = v then !i else env_get l v


let predef_with_side_effets: Id.t list = [
  "print_int";
  "print_float";
  "print_newline";
]

(* Main function *)
let elim_definition (ast:Knorm.knorm_t) : Knorm.knorm_t =

  (* env2 is the list of functions with side effects *)
  let rec elim (a:Knorm.knorm_t) (env:environment) (env2:Id.t list) : Knorm.knorm_t =
    match a with
    | Unit | Int _ | Float _ -> a

    (* no recursive calls *)
    | Var s -> let _ = env_incr env s in a
    | Neg x | FNeg x -> let _ = env_incr env x in a
    | Add (x, y) | Sub (x, y) -> 
      let _ = env_incr env x in 
        let _ = env_incr env y in a

    | FAdd (x, y) | FSub (x, y) | FMul (x, y) | FDiv (x, y) -> 
      let _ = env_incr env x in 
        let _ = env_incr env y in a

    | App (f, vars) -> 
      let _ = env_incr env f in 
        let _ = List.iter (env_incr env) vars in a

    | Get (x, y) -> 
      let _ = env_incr env x in 
        let _ = env_incr env y in a
    | Put (x, y, z) -> 
      let _ = env_incr env x in
        let _ = env_incr env y in
          let _ = env_incr env z in a
    
    | Tuple(l) -> 
      let _ = List.iter (env_incr env) l in a
    | Array(x,y) -> 
      let _ = env_incr env x in 
        let _ = env_incr env y in a

    
    (* with recursive calls *)
    | IfEq ((x,y),e1,e2) -> 
      let _ = env_incr env x in 
        let _ = env_incr env y in 
          IfEq ((x,y), elim e1 env env2, elim e2 env env2)
    | IfLE ((x,y),e1,e2) -> 
      let _ = env_incr env x in 
        let _ = env_incr env y in 
          IfLE ((x,y), elim e1 env env2, elim e2 env env2)

    (* Main cases *)
    | Let ((s,t), e1, e2) -> 
      (* here CAUTION of side effects *)
      let env' = (s, ref 0)::env in        
        let e2' = elim e2 env' env2 in
          if (env_get env' s) <> 0 || has_side_effects e1 env2 then 
            let e1' = elim e1 env env2 in Let((s,t), e1', e2')
          else e2'

    | LetRec (fd, e) -> 
      let env' = (fst fd.name, ref 0)::env in
          let env22 = 
            if has_side_effects fd.body env2 then (fst fd.name)::env2 else env2
          in 
            let e' = elim e env' env22 in
              if (env_get env' (fst fd.name)) <> 0 then
                let b = elim fd.body env env2 in
                  LetRec({name= fd.name; args=fd.args; body=b}, e')
              else e'

    | LetTuple (l1, v, e) -> 
      let _ = env_incr env v in
        let env' = (List.map (fun x -> (fst x, ref 0)) l1) @ env in
          let e' = elim e env' env2 in
            if (List.fold_left (fun x y -> x + (env_get env' (fst y))) 0 l1) <> 0 then
              LetTuple(l1, v, e')
            else e'
  in elim ast [] predef_with_side_effets