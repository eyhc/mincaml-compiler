open Knorm

type const = Ent of int | Floatt of float | Tuplet of Id.t list | None 

type exp_map = (Id.t * const) list

let get_val(env:exp_map) (var:Id.t) : const =
  try snd (List.find (fun (x, y) -> x = var) env)
  with Not_found -> None

let rec constant_folding (env:exp_map) (expr: Knorm.knorm_t) : Knorm.knorm_t =
  match expr with
  | Int i -> Int(i)
  | Float f -> Float(f)
  | Var(x) -> (match get_val env x with
    | Ent i -> Int(i)
    | Floatt f -> Float(f)
    | Tuplet t -> Tuple(t)
    | None -> expr)
  | Neg x -> (match get_val env x with
    | Ent i -> Int(-(i))
    | _ -> expr)
  | Add(x, y) -> (match get_val env x, get_val env y with
    | Ent i1, Ent i2 -> Int (i1+i2)
    | _ -> Add(x, y))
  | Sub(x, y) -> (match get_val env x, get_val env y with
    | Ent i1, Ent i2 -> Int (i1-i2)
    | _ -> Sub(x, y))
  | FNeg x -> (match get_val env x with
    | Floatt i -> Float(-.(i))
    | _ -> expr)
  | FAdd(x, y) -> (match get_val env x, get_val env y with
    | Floatt i1, Floatt i2 -> Float (i1+.i2)
    | _ -> FAdd(x, y))
  | FSub(x, y) -> (match get_val env x, get_val env y with
    | Floatt i1, Floatt i2 -> Float (i1-.i2)
    | _ -> FSub(x, y))
  | FMul(x, y) -> (match get_val env x, get_val env y with
    | Floatt i1, Floatt i2 -> Float (i1*.i2)
    | _ -> FMul(x, y))
  | FDiv(x, y) -> (match get_val env x, get_val env y with
    | Floatt i1, Floatt i2 -> Float (i1/.i2)
    | _ -> FDiv(x, y))
  | IfEq((x, y), e1, e2) -> (match get_val env x, get_val env y with
    | Ent i1, Ent i2 -> if i1 = i2 then constant_folding env e1 else constant_folding env e2
    | Floatt i1, Floatt i2 -> if i1 = i2 then constant_folding env e1 else constant_folding env e2
    | _ -> IfEq((x, y), constant_folding env e1, constant_folding env e2))
  | IfLE((x, y), e1, e2) -> (match get_val env x, get_val env y with
    | Ent i1, Ent i2 -> if i1 <= i2 then constant_folding env e1 else constant_folding env e2
    | Floatt i1, Floatt i2 -> if i1 <= i2 then constant_folding env e1 else constant_folding env e2
    | _ -> IfLE((x, y), constant_folding env e1, constant_folding env e2))
  | Let((x, t), e1, e2) ->
    let e11 = constant_folding env e1 in
    (match e11 with 
    | Int i -> Let((x,t), e11, constant_folding ((x, Ent(i))::env) e2)
    | Float f -> Let((x,t), e11, constant_folding ((x, Floatt(f))::env) e2)
    | Tuple t2 -> Let((x,t), e11, constant_folding ((x, Tuplet(t2))::env) e2)
    | _ -> Let((x,t), e11, constant_folding env e2))
  | LetRec(fd,e) -> LetRec({name = fd.name; args = fd.args; body = constant_folding env fd.body}, constant_folding env e)
  | LetTuple(l1, v, e1) -> (match get_val env v with
    | Tuplet t -> List.fold_left2 (fun e2 l2 x -> constant_folding env (Let(l2, (Var x), e2))) 
      (constant_folding env e1) l1 t
    | _ -> LetTuple(l1, v, constant_folding env e1))
  | _ -> expr

let constant (knorm_ast:Knorm.knorm_t) : Knorm.knorm_t =  
  constant_folding [] knorm_ast