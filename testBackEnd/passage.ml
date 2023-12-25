(* Ceci est un éditeur pour OCaml
   Entrez votre programme ici, et envoyez-le au toplevel en utilisant le
   bouton "Évaluer le code" ci-dessous ou [Ctrl-e]. *)

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
 
 
 (* let get_intervals asml =
   let rec g_v asml l  ind =
     match asml with 
     | Var v -> (l @ [v], modify_value hash v ind)
     | Int i -> (l, hash)
     | Let (s, e) ->
         g_v e (l @ [s])  (ind + 1)
     | Assign (s, e) -> 
         g_v e (l @ [s]) (modify_value hash s ind ) (ind + 1)
     | Add (e1, e2) -> g_v e2 (l @ (fst (g_v e1 [] (ind))) ) (ind)
     | Mul (e1, e2) -> g_v e2 (l @ (fst (g_v e1 [] (ind))) ) (ind)
     | Sub (e1, e2) -> g_v e2 (l @ (fst (g_v e1 [] (ind))) ) (ind)
     | Fun f ->  iter_list f.body l
         (* List.iter (fun param -> g_v param ) f.params; *)
         (* List.iter (fun expr -> g_v expr ) f.body *)
     | _ -> (l, hash) (* Handle other cases as needed *)
     
   and 
     iter_list asml_list l =
     match asml_list with 
     | e :: ll -> 
         let hash_ = (g_v e l hash 0) in
         iter_list ll 
     | [] -> l
   in
   iter_list asml [] (Hashtbl.create 0)
 ;; *)
 
 let asml = 
   [
     Fun {
       name = "_";
       params = [];
       body = [Let ("x", Int 1); Let ("y", Int 2); Let ("a", Int 8);
               Let ("b", Int 14); Assign ("a", Sub (Var "x", Var "y"));
               Assign ("z", Add (Var "x", Var "y"));Assign ("b", Mul (Var "a", Var "z"))];
     }
   ]
 ;;
 
 let rec find_var_and_replace lst var use =
   match lst with
   | (v, (first_use, last_use)) :: tl ->
       if v = var then
         (v, (first_use, use)) :: tl
       else
         (v, (first_use, last_use)) :: find_var_and_replace tl var use
   | [] -> (var, (use, use)) :: []
 ;;
 
 let get_intervals asml = 
   let rec gv asml l ind =
     match asml with 
       (* if variable exists in the list `l`, will update it's `last use` value*)
     | Var v -> find_var_and_replace l v ind 
     | Let (s, e) -> 
         let k = gv e l (ind + 1) in
         find_var_and_replace k s (ind + 1)  
     | Assign (s, e) -> 
         let k = gv e l (ind + 1) in
         find_var_and_replace k s (ind + 1)  
     | Add (e1, e2) | Mul (e1, e2) | Sub (e1, e2) -> 
         gv e1 (gv e2 l ind) ind
     | Fun f -> iter_list f.body l ind
     | _ -> l
   and 
     iter_list asml_list l ind = 
     match asml_list with
     | e :: ll -> iter_list ll (gv e l ind)  (ind + 1)
     | [] -> l
   in iter_list asml [] 0
 ;;
 
 let intervals = get_intervals asml;;
 (* number of free registers *)
 let num_free = 9;;
 (* list of free registers *)
 let free = ["r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "r10"; "r11"; "r12"];;
 (* 
   hashtable binding variable to register,
   a : r1 -> variable `a` is stored in register `r1` 
 *)
 let var_to_register = Hashtbl.create num_free;;
 
 let var_name (a, (_, _)) = a;;
 let startpoint (_, (b, _)) = b;;
 let endpoint (_, (_, c)) = c;;
 
 (* ---------------Functions with lists--------------- *)
 exception EOL;;
 
 let rec remove_el el = function
   | e :: ll -> 
       if e == el then 
         ll
       else 
         e :: remove_el el ll
   | [] -> []
 ;;
 
 let rec get_last_el = function
   | [] -> raise EOL 
   | [e] -> e
   | e :: ll -> get_last_el ll
 ;;
 
 (* Returns a list containing all the variable declarations on the given interval *)
 let rec get_new_var_on_interval interval = function
   | e :: ll ->
       if (endpoint e) < interval then
         get_new_var_on_interval interval ll
       else if interval = (endpoint e) then
         e :: get_new_var_on_interval interval ll
       else 
         []
   | [] -> raise EOL
 ;;
 
 (* Adds element to the list sorted by its start/end point *)
 let rec add_by_increasing_func_point func el = function
   | e :: ll ->
       if (func el) <= (func e) then
         el :: e :: ll
       else 
         e :: add_by_increasing_func_point func el ll
   | [] -> el :: []
 ;;
 
 (* Sorts list by start/end point *)
 let sort_func_point liste func =
   let comparer a b f =
     compare (f a) (f b) 
   in
   List.sort (fun x y -> comparer x y func) liste
 ;;
 
 (* 
 Takes `var` -> variable's name
   Returns `loc` : FP - loc, the location in the memory 
                                             *)
 let var_stack_location var = 
   let rec vsl loc = function
     | e :: ll -> 
         if (var_name e) = var then
           loc
         else vsl (loc + 4) ll
     | [] -> raise EOL
   in vsl 4 intervals
 ;;
 (* ------------Fin Functions with lists------------ *)
 
 
 let spillAtInterval active i = 
   let spill = get_last_el active in
   let register = Hashtbl.find var_to_register (var_name i) in
   (* remove spill from active *)
   let act = remove_el spill active in
   if (endpoint spill) > (endpoint i) then begin
     (* register[i] ← register[spill] *)
     Hashtbl.remove var_to_register (var_name i);
     Hashtbl.add var_to_register (var_name spill) register;
     
     (* add i to active, sorted by increasing end point *)
     add_by_increasing_func_point endpoint i act
   end
   else
     active 
 ;;
 