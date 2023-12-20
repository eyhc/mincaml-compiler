(* `Code` est un ensemble de fonctions *)
(* type asml =
  | Code of asml_expr list *)

type asml_expr =
  | Var of string
  | Int of int
  | RegLet of string * asml_expr
  | Let of string * asml_expr
  (* | StackLet of  *)
  | Fun of asm_function
  | Add of asml_expr * asml_expr
  | Sub of asml_expr * asml_expr
  | Mul of asml_expr * asml_expr
  | Assign of string * asml_expr

and asm_function = {
  name : string;
  params : asml_expr list;
  body : asml_expr list;
}

let asml = 
  [
    Fun {
      name = "_";
      params = [];
      body = [Let ("x", Int 1); Let ("y", Int 2); Let ("a", Int 8);
              Let ("b", Int 14); Assign ("a", Sub (Var "x", Var "y"));
              Assign ("z", Add (Var "x", Var "y")); Assign ("b", Mul (Var "a", Var "z"))];
    }
  ]
;;


let rec asml_show asml = 
  match asml with
  | Var v -> Printf.printf "Variable: %s\n" v
  | Int i -> Printf.printf "Integer: %d\n" i
  | RegLet (r, e) -> 
      Printf.printf "Register Let: %s\n" r;
      asml_show e
  | Let (l, e) -> 
      Printf.printf "Let: %s\n" l;
      asml_show e
  | Fun f ->
      Printf.printf "Function: %s\n" f.name;
      Printf.printf "Params:\n";
      List.iter (fun param -> asml_show param; Printf.printf "---\n") f.params;
      Printf.printf "Body:\n";
      List.iter (fun expr -> asml_show expr; Printf.printf "---\n") f.body
  | _ -> () (* Handle other cases as needed *)
  
and 
  
  asml_list_show l = 
  match l with 
  | e :: ll -> asml_show e; asml_list_show ll;
  | [] -> ()
;;

(* let rec asml_list_apply func l = 
  match l with 
  | e :: ll -> func e; asml_list_apply func ll
  | [] -> ()
;; *)

let get_vars asml =
  let rec g_v asml l = 
    match asml with 
    | Var v -> l @ [v]
    | Int i -> l
    | Let (s, e) -> g_v e (l @ [s])
    | Assign (s, e) -> g_v e (l @ [s])
    | Fun f ->  iter_list f.body l
        (* List.iter (fun param -> g_v param ) f.params; *)
        (* List.iter (fun expr -> g_v expr ) f.body *)
    | _ -> l (* Handle other cases as needed *)
    
  and
    iter_list asml_list l =
    match asml_list with 
    | e :: ll -> iter_list ll (g_v e l); 
    | [] -> l
  in
  iter_list asml [] 
;;

List.iter (fun expr -> Printf.printf "\"%s\", " expr) (get_vars asml)

(* asml_list_apply asml_show asml;; *)
  