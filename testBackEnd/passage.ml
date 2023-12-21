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
              Let ("b", Int 14); Assign ("a", Sub (Var "x", Var "y"))
             ;Assign ("z", Add (Var "x", Var "y"));Assign ("b", Mul (Var "a", Var "z"))];
    }
  ]
;;


let asml_show asml = 
  let rec asml_show_unit asml = 
    match asml with
    | Var v -> Printf.printf "%s" v
    | Int i -> Printf.printf "%d" i
    | RegLet (r, e) -> 
        Printf.printf "Register Let: %s\n" r;
        asml_show_unit e
    | Let (l, e) -> 
        Printf.printf "Let %s = " l;
        asml_show_unit e;
        Printf.printf "\n"
    | Assign (s, e) -> 
        Printf.printf "%s = " s;
        asml_show_unit e;
        Printf.printf "\n"
    | Add (e1, e2) ->  
        asml_show_unit e1;
        Printf.printf " + ";
        asml_show_unit e2;
        Printf.printf "\n";    
    | Mul (e1, e2) ->  
        asml_show_unit e1;
        Printf.printf " * ";
        asml_show_unit e2;
        Printf.printf "\n";
    | Sub (e1, e2) ->  
        asml_show_unit e1;
        Printf.printf " - ";
        asml_show_unit e2;
        Printf.printf "\n";
    | Fun f ->
        Printf.printf "Function: %s\n" f.name;
        Printf.printf "Params:\n";
        List.iter (fun param -> asml_show_unit param; Printf.printf "---\n") f.params;
        Printf.printf "Body:\n";
        List.iter (fun expr -> asml_show_unit expr; Printf.printf "---\n") f.body
    | _ -> () (* Handle other cases as needed *)
  
  and 
  
    asml_list_show l = 
    match l with 
    | e :: ll -> asml_show_unit e; asml_list_show ll;
    | [] -> () 
  in asml_list_show asml
;;

(* let rec asml_list_apply func l = 
  match l with 
  | e :: ll -> func e; asml_list_apply func ll
  | [] -> ()
;; *)

let key_in_hash hash key =
  try 
    Hashtbl.find hash key;
    true
  with e -> (false)
;;

let add_hash hash key value =
  if not (key_in_hash hash key) then 
    Hashtbl.add hash key value;
  hash
;;

let modify_value hashmap key value =
  match (Hashtbl.find_opt hashmap key) with
  | Some lst -> Hashtbl.replace hashmap key (List.hd lst :: value :: []); hashmap
  | None -> Hashtbl.add hashmap key [value; value]; hashmap
;;

let get_vars asml =
  let rec g_v asml l hash ind =
    match asml with 
    | Var v ->  
        if not (key_in_hash hash v) then 
          (l @ [v, ind], modify_value hash v ind)
        else
          (l, modify_value hash v ind)
    | Let (s, e) ->
        g_v e l hash (ind + 1)
    | Assign (s, e) -> 
        if not (key_in_hash hash s) then 
          g_v e (l @ [(s, ind)]) (modify_value hash s (ind) ) (ind) 
        else 
          g_v e l (modify_value hash s (ind) ) (ind)
    | Add (e1, e2) | Mul (e1, e2) | Sub (e1, e2) -> 
        g_v e2 (l @ (fst (g_v e1 [] hash (ind)))) hash (ind)
    | Fun f ->  iter_list f.body l hash ind
    | _ -> (l, hash) (* Handle other cases as needed *)
    
  and 
    iter_list asml_list l hash ind =
    match asml_list with 
    | e :: ll -> 
        let hash_ = (g_v e l hash ind) in
        iter_list ll (fst hash_) (snd hash_) (ind + 1)
    | [] -> (l, hash)
  in
  iter_list asml [] (Hashtbl.create 0) 1
;;

let rec add_to_list_in_order list value cmp = 
  match list with 
  | [] -> value :: []
  | e :: ll -> 
      (* snd value <= snd e *)
      if cmp (snd value) (snd e) then value :: e :: ll else e :: add_to_list_in_order ll value cmp 
;;

(* 1 takes the name of the variable form a (string * int) list 
   2 takes its `end` value from the hashtable
   3 *)
let start_to_end list_start list_end hash =
  let var_name = fst (List.hd list_start) in 
  let var = (var_name, (List.nth (Hashtbl.find hash var_name) 1)) in
  Printf.printf "(\"%s\", %d); " (fst var) (snd var);
  add_to_list_in_order list_end var ( <= )
;;

let create_and_initialize_register_hashtable () =
  let my_hashtable = Hashtbl.create 9 in
  for i = 4 to 12 do
    Hashtbl.add my_hashtable ("r" ^ string_of_int i) 1
  done;
  my_hashtable
;;

let find_free_register hashtable =
  let found_key = ref None in
  Hashtbl.iter (fun key value ->
      if value = 1 && !found_key = None then begin
        found_key := Some key;
        Hashtbl.replace hashtable key 0
      end
    ) hashtable;
  !found_key
;;



let registers = create_and_initialize_register_hashtable () ;;
let (var_start_list, hashtable) = get_vars asml ;;
Hashtbl.iter (fun a b ->Printf.printf "\"%s\", %d %d \n" a (List.hd b) (List.nth b 1)) hashtable;;
List.iter (fun a -> Printf.printf "(\"%s\", %d); " (fst a) (snd a)) var_start_list;;

let var_end_list = [("x", 6); ("y", 6);  ("a", 7); ("z", 7); ("b", 7)];;

start_to_end var_start_list  var_end_list hashtable
 


(* asml_list_apply asml_show asml;; *)
(*asml_show asml;;*)


(* let linearScanAlloc hashmap var_list =  *)
  
  
