(*
beta.ml

date : 09-01-2023
*)

let rec reduction (ast:Knorm.knorm_t) : Knorm.knorm_t =
  let rec beta (env:Alpha.epsilon) (a:Knorm.knorm_t) : Knorm.knorm_t = 
    let eps_apply = Alpha.apply env in
      (match a with
      (* nothing to do *)
      | Unit -> a
      | Int i -> a
      | Float f -> a
    
      (* find in environment *)
      | Var s -> Var (eps_apply s)
    
      (* no recursive calls *)
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
      | IfEq ((x,y),e1,e2) -> 
        let e12 = beta env e1 and e22 = beta env e2 in
          IfEq ((eps_apply x, eps_apply y), e12, e22)
      | IfLE ((x,y),e1,e2) -> 
        let e12 = beta env e1 and e22 = beta env e2 in
          IfLE ((eps_apply x, eps_apply y), e12, e22)
      | LetRec (fd, e) -> 
        LetRec ({name = fd.name; args = fd.args; body = beta env fd.body}, beta env e)
      | LetTuple (l1, v, e) -> LetTuple (l1, eps_apply v, beta env e)
    
      (* Main case *)
      | Let ((s,t), e1, e2) -> 
        let e12 = beta env e1 in
          match e12 with
          | Var x -> beta ((s,x)::env) e2
          | _ -> Let((s,t), e12, beta env e2)
      )
  in beta [] ast