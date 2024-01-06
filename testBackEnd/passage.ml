 type asml_expr =
   | Var of string
   | Int of int
   | Let of string * asml_expr 
   | Fun of asm_function
   | Add of asml_expr * asml_expr
   | Sub of asml_expr * asml_expr
   | Mul of asml_expr * asml_expr
   | Assign of string * asml_expr
   | Tuple of (string * string * int)
   | Call of string * string list
   | Store of asml_expr * string 
   | Load of string * asml_expr 
   | Reg of string
   | Unit
          
 and asm_function = {
   name : string;
   params : asml_expr list;
   body : asml_expr list;
 }
 ;;
 let asml = 
   [ 
     Fun { 
       name = "_"; 
       params = []; 
       body = [Let ("x", Int 1); Let ("y", Int 2); Let ("a", Int 8);
               Let ("b", Int 14); Assign ("a", Sub (Var "x", Var "y")); 
               Assign ("z", Add (Var "x", Var "y")); 
               Assign ("b", Mul (Var "a", Var "z")); 
               Assign ("i", Mul (Var "a", Var "z"))]; 
     } 
   ] ;;
 
 let num_registers = 9;;
 (* hashmap de toutes les variables sur la pile avec leurs adresses
 exemple : "x" "FP - 4" : *)
 let var_in_stack = Hashtbl.create 0;;
     (* 
 hashtable binding variable to register,
 a : r1 -> variable `a` is in register `r1` 
 *)
 let var_to_register = Hashtbl.create num_registers;;
 
 (* Retourne pour chaque ligne i les prochaines variables qui vont etre utilisées *)
 let get_intervals_i asml i =
   let list = ref [] in
   let rec i_intervals asml index  =
     match asml with
     | Fun f :: tail -> i_intervals f.body index;
         i_intervals tail (index + (List.length f.body));
     | hd :: tail ->
         ( match hd with
           | _ when (List.length !list) = num_registers -> ()
           | Let (var, _) ->
               list := !list @ [(var,index)];
               i_intervals tail (index + 1)
           | Assign (var, e1) ->
               if not (List.exists (fun (v, _) -> v = var) !list) then begin
                 list := !list @ [(var,index)];
                 i_intervals [e1] index;
                 i_intervals tail (index + 1)
               end
               else begin
                 i_intervals [e1] index;
                 i_intervals tail (index + 1)
               end
           | Add (e1,e2) | Sub (e1,e2) | Mul (e1,e2) ->
               i_intervals [e1] index;
               i_intervals [e2] index
           | Var var ->
               if not (List.exists (fun (v, _) -> v = var) !list) then
                 list := !list @ [(var,index)];
           | _ -> i_intervals tail (index + 1))
     | [] -> ()
   in i_intervals asml i;
   let l = list
   in l
 ;;

 (* retourne une list de var qui doivent etre store car presente dans la hashmap mais pas dans la liste active *) 
 let var_not_in_list hashmap active =
   let strings_in_list = List.map (fun (str, _) -> str) active in
   Hashtbl.fold (fun key value acc -> if not (List.mem key strings_in_list) then (key,value) :: acc else acc) hashmap []
 ;;
 
 (* retourne une liste des var a mettre dans les registres (load ou let)*)
 let var_not_in_hash hashmap active = 
   List.fold_left (fun acc (key, _) -> 
       match Hashtbl.find_opt hashmap key with 
       | Some _ -> acc  (* si la cle est deja  dans la hashmap, ne rien faire *) 
       | None -> key :: acc  (* sinon, on ajoute la cle a la liste *) 
     ) [] active 
 ;;
 
 let store_load intervals body =
   (* a la premiere ligne hashtable est vide, on y met les 9 variables *)
   if (Hashtbl.length var_to_register) = 0 then begin 
     let rec add_hashmap n inter =
       match inter with
       | [] -> ()
       | _ when n >= num_registers -> ()
       | (var, v) :: tail -> 
           Hashtbl.add var_to_register var ("r" ^ string_of_int (n + 4)); (* + 4  car commence a r4 *)
           add_hashmap (n + 1) tail
     in
     add_hashmap 0 !intervals;
   end
   else
     (* on recupere la liste des var a store *)
     let rec parcours_store list_store active_a_ajouter =
       match list_store with
       | (v,r) ::  tl ->
           Hashtbl.remove var_to_register v;
           (try
              let adr = Hashtbl.find var_in_stack v in
              body := !body @ [Store (Reg r, adr)];
            with Not_found ->
              let adr = "FP - " ^ string_of_int (((Hashtbl.length var_in_stack) + 1) * 4) in
              Hashtbl.add var_in_stack v adr;
              body := !body @ [Store (Reg r, adr)]);
           (try
              let var = List.hd active_a_ajouter in
              Hashtbl.add var_to_register var r;
              (try
                  (*  si la var est presente dans la pile, on load *)
                 let value = Hashtbl.find var_in_stack var in
                 body := !body @ [Load (value, Reg r)]; 
               with e -> ());
            with e -> ());
           parcours_store tl (try (List.tl active_a_ajouter) with Failure tl -> []);
       | [] -> ()
     in 
     let store = var_not_in_list var_to_register !intervals in 
     let active_a_ajouter = var_not_in_hash var_to_register !intervals in 
     parcours_store store active_a_ajouter; 
     ()
 ;;
 
 (* Retourne asml_expr en remplacant par registres pour Int, Var, Add, Sub, Mul *)
 let rec expr_asml expr =
   match expr with
   | Int n -> Int n
   | Var r -> Reg (Hashtbl.find var_to_register r)
   | Add (e1, e2) -> Add ((expr_asml e1), (expr_asml e2))
   | Sub (e1, e2) -> Sub ((expr_asml e1), (expr_asml e2))
   | Mul (e1, e2) -> Mul ((expr_asml e1), (expr_asml e2))
   | _ -> Unit;;
 
 let parcours asml =
   let rec parcours list_asml i new_body =
     match list_asml with
     | hd :: tail ->
         (match hd with 
          | Fun f -> 
              let new_func : asm_function = {
                name = f.name;
                params = f.params;
                body = parcours f.body i (ref []);
              } in 
              new_body := !new_body @ [Fun new_func];
              parcours tail (i + (List.length f.body)) new_body;
          | Let (v, e1) -> 
              let active = get_intervals_i list_asml i in
              store_load active new_body;
              new_body:= !new_body @ [Let ((Hashtbl.find var_to_register v), (expr_asml e1))]; 
              parcours tail (i + 1) new_body 
          | Assign (s,e1) ->
              let active = get_intervals_i list_asml i in
              store_load active new_body;
              new_body:= !new_body @ [Assign ((Hashtbl.find var_to_register s), (expr_asml e1))]; 
              parcours tail (i + 1) new_body 
          | _ -> 
              parcours tail (i + 1) new_body ); 
     | [] -> 
         store_load (ref []) new_body;
         !new_body;
 
   in
   let new_body = ref [] in 
   parcours asml 1 new_body;;
 


   (* Fonction print 
  let print_hashtable my_hashtable =
    Hashtbl.iter (fun key value ->
        Printf.printf "%s -> %s\n" key value  (* Remplacez 'string_of_int' par une conversion appropriée pour votre type 'a' *)
      ) my_hashtable;;
  let print_free my_list =
    List.iter (fun elem ->
        Printf.printf "%s\n" elem  (* Remplacez '%s' par le format approprié pour le type réel de vos éléments *)
      ) my_list;;
  let print_intervals intervals =
    List.iter (fun (key,start) ->
        Printf.printf "(%s,(%s))\n" key start
      ) intervals;; 
  *)