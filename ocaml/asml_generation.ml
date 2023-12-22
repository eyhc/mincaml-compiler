open Syntax
open Printf

type asml =
  | NIL
  | Int of int
  | Ident of string
  | Add of asml * asml
  | Sub of asml * asml
  | Var of string
  | Let of string * asml * asml 
  | LetRec of string * asml list * asml
  | Main of asml list * asml

let rec find (exp: Syntax.t) =
  match exp with
  | Let((id,t), e1, e2) -> find e2
  | LetRec(fd, e) -> [exp]@(find e)
  | _ -> []


let rec asml_convertion (exp: Syntax.t): unit =
  (* let rec convert_args args =
    match args with
    | [] -> []
    | (id, t) :: l -> Var(id) :: (convert_args l) in
  match exp with
  | Int i -> Int(i)
  | Var id -> Var(id)
  | Add(e1, e2) -> Add(asml_convertion e1, asml_convertion e2)
  | Sub(e1, e2) -> Sub(asml_convertion e1, asml_convertion e2)
  | Let((id, t), e1, e2) -> Let(id, asml_convertion e1, asml_convertion e2)
  | LetRec(fd, e) -> LetRec("_"^(fst fd.name), convert_args fd.args, asml_convertion fd.body)
  in NIL  *)
  let l = find exp in
  match l with
  | [x] -> printf("%s\n") (to_string x)
  | head :: l -> printf("%s\n") (to_string head);