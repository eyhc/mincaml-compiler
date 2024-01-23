(*
reduction.ml

date : 21-12-2023
*)

open Knorm

let rec insert (x:Id.t*Type.t) (e1:knorm_t) (e2:knorm_t) : knorm_t =
  match e1 with
  | Let (y, e3, e4) -> Let (y, e3, insert x e4 e2)
  | LetRec (fundef, e) -> LetRec (fundef, insert x e e2)
  | LetTuple (l1, v, e) -> LetTuple (l1, v, insert x e e2)
  | _ -> Let (x, e1, e2)


(* reduction of nested let-expression *)
(* this algorithm is the same one in the article  *)
let rec reduction (ast_norm:knorm_t) : knorm_t =
  match ast_norm with
  | IfEq (b, e2, e3) -> IfEq (b, reduction e2, reduction e3)
  | IfLE (b, e2, e3) -> IfLE (b, reduction e2, reduction e3)
  | Let (x, e1, e2) ->  insert x (reduction e1) (reduction e2)
  | LetRec (fd, e) -> 
    LetRec ({name = fd.name; args = fd.args; body = reduction fd.body}, reduction e)
  | LetTuple(l1, l2, e) -> LetTuple(l1, l2, reduction e)
  | _ -> ast_norm
