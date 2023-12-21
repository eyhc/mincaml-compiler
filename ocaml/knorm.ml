open Syntax
open Printf

let ide x = x

(* Insert a new let in the code and replace e with the new generated let *)
let insert_let e k =
  let name = Id.genid () in
  let e' = k (Var(name)) in
  Let((name, Int), e, e')


(* let rec norm_args args k =
  match args with
  | [x] -> (match x with
          | App(e1, le2) -> norm_args le2 (fun z -> k [App(e1, z)])
          | _ -> insert_let x (fun y -> k [y]))
  | head :: l -> insert_let head (fun x -> norm_args l (fun y -> k ([x]@y)) ) *)

let k_normalization exp =
  let rec norm e k =
    match e with
    | Var id -> insert_let e k
    | Int i ->  insert_let e k
    | Add(e1, e2) -> norm e1 (fun x -> norm e2 (fun y -> k (Add(x, y))))
    | Sub(e1, e2) -> norm e1 (fun x -> norm e2 (fun y -> k (Sub(x, y))))
    | Let((id, t), e1, e2) -> let k_e1 = norm e1 ide in Let((id, t), k_e1, 
        match e2 with
        | Var id -> e2
        | _ -> norm e2 ide)
    | LetRec(fd, e) -> let k_body = norm fd.body ide in LetRec({name = fd.name; args = fd.args; body = k_body}, 
        match e with
        | Var id -> e
        | _ -> norm e ide)
    (* | App (e1, le2) -> norm_args le2 (fun x -> App(e1, x)) *)
    | _ -> e
  in norm exp ide