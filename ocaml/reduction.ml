(*
reduction.ml

date : 21-12-2023
*)

open Knorm

(* reduction of nested let-expression *)
(* this algorithm is the same one in the article  *)
(* AST MUST BE IN K-NORMAL FORM ! *)
let rec reduction (ast_norm:knorm_t) : knorm_t =
  match ast_norm with
  | IfEq (b, e2, e3) -> IfEq (b, reduction e2, reduction e3)
  | IfLE (b, e2, e3) -> IfLE (b, reduction e2, reduction e3)
  | Let (x, e1, e2) -> 
    let rec insert =
      (function
      | Let (y, e3, e4) -> Let (y, e3, insert e4)
      | LetRec (fundef, e) -> LetRec (fundef, insert e)
      | LetTuple (l1, l2, e) -> LetTuple (l1, l2, insert e)
      | e -> Let (x, e, reduction e2))
    in insert (reduction e1)
  | LetRec (fd, e) -> 
    LetRec ({name = fd.name; args = fd.args; body = reduction fd.body}, reduction e)
  | LetTuple(l1, l2, e) -> LetTuple(l1, l2, reduction e)
  | _ -> ast_norm
