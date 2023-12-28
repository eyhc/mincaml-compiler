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
 let num_registers = 9;;
    (* list of free registers *)
 let free_registers = ref ["r4"; "r5"; "r6"; "r7"; "r8"; "r9"; "r10"; "r11"; "r12"];;
    (* 
  hashtable binding variable to register,
  a : r1 -> variable `a` is stored in register `r1` 
  *)
 let var_to_register = Hashtbl.create num_registers;;
  
 let active = ref [];; 

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
       if (startpoint e) < interval then
         get_new_var_on_interval interval ll
       else if interval = (startpoint e) then
         e :: get_new_var_on_interval interval ll
       else 
         []
   | [] -> []
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

 let var_in_hash hash var =
   try 
     Hashtbl.find hash var;
     true
   with e -> false
 ;;
   
 let rec active_vars registers = function
   | e :: ll ->
       if (var_in_hash registers (startpoint e)) then
         e :: active_vars registers ll 
       else
         active_vars registers ll 
   | [] -> []
 ;;
   
 let add_element_to_list lst element =
   lst := element :: !lst
 ;;
  
 let remove_element_from_list lst element =
   lst := List.filter (fun x -> x <> element) !lst
 ;; 
 let sort_active_by_endpoint () =
   active := List.sort (fun (_, (_, endpoint1)) (_, (_, endpoint2)) -> compare endpoint1 endpoint2) !active
 ;;
    
 let spillAtInterval i = 
   let spill = get_last_el !active in
   let register = Hashtbl.find var_to_register (var_name spill) in
      (* remove spill from active *)
   remove_element_from_list active spill;
   if (endpoint spill) >= (endpoint i) then begin
        (* register[i] ← register[spill] *)
     Hashtbl.remove var_to_register (var_name spill);
     Hashtbl.add var_to_register (var_name i) register; 
        (* add i to active, sorted by increasing end point *)
     add_element_to_list active i;
     sort_active_by_endpoint ()
   end 
 ;; 
  
  (* retourne la liste des variables actives en fonction de la ligne i *) 
 let expire_old_interval i =
   let rec active_register i active elem_suppr =
     match active with 
     | e :: ll -> 
         if i > (endpoint e) then 
           active_register i ll (elem_suppr @ [e]) 
         else
           active_register i ll elem_suppr 
     | [] -> elem_suppr
   in
   let elem_suppr = (active_register i !active []) in
   active := List.filter (fun x -> not (List.mem x elem_suppr)) !active;
   let rec remove_in_registers elem_suppr = 
     match elem_suppr with
     | hd :: tail ->
         (try
            let r = Hashtbl.find var_to_register (fst hd) in
            Hashtbl.remove var_to_register (fst hd);
            add_element_to_list free_registers r;
            remove_in_registers tail
          with
          | _ -> remove_in_registers tail)
     | [] -> ()
   in 
   remove_in_registers elem_suppr;; 
  
 let print_hashtable my_hashtable =
   Hashtbl.iter (fun key value ->
       Printf.printf "%s -> %s\n" key value  (* Remplacez 'string_of_int' par une conversion appropriée pour votre type 'a' *)
     ) my_hashtable;;
 let print_free my_list =
   List.iter (fun elem ->
       Printf.printf "%s\n" elem  (* Remplacez '%s' par le format approprié pour le type réel de vos éléments *)
     ) my_list;;
 let print_intervals intervals =
   List.iter (fun (key, (start, end_)) ->
       Printf.printf "%s -> (%d, %d)\n" key start end_
     ) intervals;;
   
  let linear_scan_register_allocation =
   let rec live_interval live =
    match live with (* foreach live interval i, in order of increasing start point *)
     | hd :: tail ->
         expire_old_interval (startpoint hd) ; 
         if (List.length !active) = num_registers then begin
           spillAtInterval (hd);
           Printf.printf "Ligne %d, %s\n" (startpoint hd) (fst hd);
           Printf.printf "liste actives \n";
           print_intervals !active ;
           Printf.printf "hashtbl var registre \n";
           print_hashtable var_to_register;
           live_interval tail;
         end
         else begin 
           let r = List.hd !free_registers in 
           remove_element_from_list free_registers r;
           Hashtbl.add var_to_register (fst hd) r;
           add_element_to_list active hd;
           sort_active_by_endpoint ();
           Printf.printf "Ligne %d, %s\n" (startpoint hd) (fst hd);
           Printf.printf "liste actives \n";
           print_intervals !active ;
           Printf.printf "hashtbl var registre \n";
           print_hashtable var_to_register;
           live_interval tail
         end;
     | []-> () 
   in
   let live = sort_func_point (get_intervals asml) startpoint in
   live_interval live
  
   (* TEST expireoldinterval 
   
   (* Listes au point 6 :*)
   free_registers := [ "r9"; "r10"; "r11"; "r12"];; 
   active := [("x", (1, 6)); ("y", (2, 6)); ("a", (3, 7)); ("b", (4, 7)); ("z", (6, 7))];; 
   Hashtbl.add var_to_register "x" "r4";;
    Hashtbl.add var_to_register "y" "r5";;
    Hashtbl.add var_to_register "a" "r6";;
    Hashtbl.add var_to_register "b" "r7";;
    Hashtbl.add var_to_register "z" "r8";;
   (* Liste au point 7*)
    expire_old_interval 7 ;
    Printf.printf "liste actives \n";
    print_intervals !active ;
    Printf.printf "hashtbl var registre \n";
    print_hashtable var_to_register;
    Printf.printf "liste registres libre \n";
    print_free !free_registers;;
       
 *)