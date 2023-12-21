open Syntax

(*
reduction.ml

date : 21-12-2023
authors : Carrot Elie
*)

(* reduction of nested let-expression *)
(* this algorithm is the same one in the article  *)
(* AST MUST BE IN K-NORMAL FORM ! *)
let rec reduction (ast:t) : t =
  match ast with
  | If (e1, e2, e3) -> If (e1, reduction e2, reduction e3)
  | Let (xt, e1, e2) -> 
    let rec insert =
      (function
      | Let (yt, e3, e4) -> Let (yt, e3, insert e4)
      | LetRec (fundefs, e) -> LetRec (fundefs, insert e)
      | LetTuple (yts, z, e) -> LetTuple (yts, z, insert e)
      | e -> Let (xt, e, reduction e2))
    in insert (reduction e1)
  | LetRec (fd, e) -> 
    LetRec ({ name = fd.name; args = fd.args; body = reduction fd.body }, reduction e)
  | LetTuple (l, e1, e2) -> LetTuple (l, e1, reduction e2)
  | _ -> ast
