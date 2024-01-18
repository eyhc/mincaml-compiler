open Asml;;

(* type pour la partie back-end *)
type reg_expr =
  | Int of int 
  | Neg of Id.t
  | Add of Id.t * reg_expr
  | Sub of Id.t * reg_expr
  | Call of Id.l 
  | If of Id.l * (Id.t*reg_expr) * regt list * regt list
  | Reg of Id.t
  | Unit

and regt = 
  | Let of Id.t * reg_expr 
  | Exp of reg_expr
  | Store of reg_expr * Id.t 
  | Load of Id.t * reg_expr 
  | Push of Id.t
      
and letregdef = 
  | Fun of reg_function
  
and reg_function = {
  name : Id.l;
  body : regt list
}

let num_params : int list ref = ref []

let print_hashtable my_hashtable =
  Hashtbl.iter (fun key value ->
      Printf.printf "%s -> %s\n" key value  (* Remplacez 'string_of_int' par une conversion appropriée pour votre type 'a' *)
    ) my_hashtable

let print_free my_list =
  List.iter (fun elem ->
      Printf.printf "%s\n" elem  (* Remplacez '%s' par le format approprié pour le type réel de vos éléments *)
    ) my_list

let print_intervals intervals =
  List.iter (fun (key,start) ->
      Printf.printf "(%s,(%s))\n" key start
    ) intervals

  (* Nombres de registres pour les variables locales *)
let num_registers = 7
  
  

  (* parcours_asmt
  * hashtable binding variable to register,
  * a : r1 -> variable `a` is in register `r1` 
*)
let registers = ["r4";"r5";"r6";"r7";"r8";"r9";"r10"];;
let reg_available = ref registers;;

let intersection_keys hashtable list = 
  let result = ref [] in 
  List.iter (fun key -> 
      if Hashtbl.mem hashtable key then 
        result := key :: !result 
    ) list; 
  !result 
;;

let remove_e_list_ref e = 
  let rec r_lr list =
    match list with
    | hd :: ll -> 
        if e = hd then 
          r_lr ll 
        else 
          hd :: r_lr ll 
    | [] -> []
  in reg_available := (r_lr !reg_available)
;;

let update_reg_available hashmap =
  reg_available := registers;
  Hashtbl.iter (fun key value ->
      remove_e_list_ref value;
    ) hashmap;
;;

let get_intervals_i asml var_to_register list_param = 
  let list = ref [] in
  let rec i_intervals_asmt asmt =
    match asmt with 
    | LET (var1, var2, exp) -> 
        if not ((List.length !list) = num_registers) then begin
          i_intervals_string var1;
          i_intervals_expr var2;
          i_intervals_asmt exp; 
        end
    | EXP expr -> i_intervals_expr expr;
          
  and i_intervals_string var = 
    if not ((List.length !list) = num_registers) then begin
      if not (List.exists (fun v -> v = var) !list) && not (List.exists (fun v -> v = var) list_param)then 
        list := !list @ [var]; 
    end                    
  and i_intervals_id_or_imm id_or_im =
    match id_or_im with
    | Var v -> i_intervals_string v
    | Const _ -> ()
                  
  and i_intervals_expr expr =
    match expr with 
    | VAL v ->
        i_intervals_id_or_imm v
    | NEG s ->
        i_intervals_string s
    | ADD (s, id_or_im) | SUB (s, id_or_im)  ->
        i_intervals_string s;
        i_intervals_id_or_imm id_or_im 
    | NOP -> () 
    | CALL (s,hd::tl) ->
        i_intervals_string hd;
        i_intervals_expr (CALL (s,tl))
    | CALL (_, []) -> ()
    | IFEQ ((s,i_o_s) , asmt1, asmt2) | IFLE ((s,i_o_s) , asmt1, asmt2) | IFGE ((s,i_o_s) , asmt1, asmt2) -> 
        i_intervals_string s;
        i_intervals_id_or_imm i_o_s;
        get_keys var_to_register
    | _ when (List.length !list) = num_registers -> () 
    | _ -> ()
              (* and i_intervals asml =
                  match asml with
                  | _ when (List.length !list) = num_registers -> () 
                  | (Main hd) :: tail -> 
                      i_intervals_asmt hd;
                      i_intervals tail
                  | LetFloat _ :: tl -> () (* A definir *)
                  | LetLabel (_,_,_) :: tl -> () (* A definir *) *)
  and get_keys hashtable = 
    Hashtbl.iter (fun key _ ->
        i_intervals_string key
      ) hashtable; 
      
  in
  i_intervals_asmt asml ;
  let l = list in l
;;

  (* retourne une list de var qui doivent etre store car presente dans la hashmap mais pas dans la liste active *) 
let var_not_in_list hashmap active =
  let strings_in_list = List.map (fun str -> str) active in
  Hashtbl.fold (fun key value acc -> if not (List.mem key strings_in_list) then (key,value) :: acc else acc) hashmap []
;;
  
  (* retourne une liste des var a mettre dans les registres (load ou let)*)
let var_not_in_hash hashmap active = 
  List.fold_left (fun acc key -> 
      match Hashtbl.find_opt hashmap key with 
      | Some _ -> acc  (* si la cle est deja  dans la hashmap, ne rien faire *) 
      | None -> key :: acc  (* sinon, on ajoute la cle a la liste *) 
    ) [] active 
;; 
  
let filter_hashtable_by_keys hashtable = 
  let result_hashtable = Hashtbl.create (Hashtbl.length hashtable) in 
  Hashtbl.iter (fun key value -> 
      Hashtbl.add result_hashtable key value
    ) hashtable; 
  result_hashtable 
;;

let fill_var_to_register intervals var_to_register =
  let rec add_hashmap n inter =
    match inter with
    | [] -> ()
    | _ when n >= num_registers -> ()
    | var :: tail -> 
        Hashtbl.add var_to_register var ("r" ^ string_of_int (n + 4));
        remove_e_list_ref ("r" ^ string_of_int (n + 4));
        add_hashmap (n + 1) tail
  in
  add_hashmap 0 !intervals;;

let rec active_add act_add bd var_to_register var_in_stack =
  match act_add with
  | hd :: tl -> 
      (try Hashtbl.find var_to_register hd; () with e ->
         update_reg_available var_to_register;
         let r = List.hd !reg_available in
         (try 
            (*  si la var est presente dans la pile, on load *)
            let value = Hashtbl.find var_in_stack hd in
            bd:= !bd @ [Load (value, Reg r)]; 
          with e -> ()); 
         remove_e_list_ref r;
         Hashtbl.add var_to_register hd r; 
         active_add tl bd var_to_register var_in_stack)
  | [] -> ();;
            
              (*let store_load_end_if active var_to_register =
                if (Hashtbl.length var_to_register = 0) then 
                  []
                else begin*) 

let register_param = ["r0";"r1";"r2";"r3"];;

let is_pos_adr chaine = 
  if String.length chaine >= 7 then 
    chaine.[6] = '-' 
  else 
    false 
;;
 
let store_load intervals body var_to_register var_in_stack =
    (* a la premiere ligne hashtable est vide, on y met les 9 variables *)
      (* sinon, on recupere la liste des var a store *)
  let rec parcours_store list_store active_a_ajouter =
    match list_store with
    | (v,r) ::  tl -> 
        if List.exists (fun x -> x = r) register_param then begin 
          parcours_store tl (try (active_a_ajouter) with Failure tl -> [])
        end 
        else 
          (Hashtbl.remove var_to_register v; 
           reg_available := !reg_available @ [r];
           (try
              let adr = Hashtbl.find var_in_stack v in
              if not (is_pos_adr adr) then 
                Hashtbl.add var_in_stack v adr 
              else begin
                body := !body @ [Store ((Reg r), adr)];
              end
            with Not_found ->
              let first_element = match !num_params with
                  | hd :: _ -> hd
                  | [] -> 0
              in
              let adr = "-" ^ string_of_int ((((Hashtbl.length var_in_stack) - first_element) + 1) * 4) in
              Hashtbl.add var_in_stack v adr;
              body := !body @ [Store ((Reg r), adr)];
              (try
                 let var = List.hd active_a_ajouter in
                 Hashtbl.add var_to_register var r;
                 remove_e_list_ref r;
                 (try
                    (*  si la var est presente dans la pile, on load *)
                    let value = Hashtbl.find var_in_stack var in 
                    body := !body @ [Load (value, Reg r)]; 
                  with e -> ());
               with e -> ());
              parcours_store tl (try (List.tl active_a_ajouter) with Failure tl -> [])));
    | [] -> active_add active_a_ajouter body var_to_register var_in_stack
  in 
  let store = var_not_in_list var_to_register !intervals in 
  let active_a_ajouter = var_not_in_hash var_to_register !intervals in 
  parcours_store store active_a_ajouter; 
  ()
;; 

let load_if var_to_register var_to_register_asmt bd var_in_stack = 
  Hashtbl.iter (fun key value -> 
      try 
        let val1 = Hashtbl.find var_to_register_asmt key in 
        if val1 != value then begin
          let adr = Hashtbl.find var_in_stack key in
          bd := !bd @ [Load (adr, Reg value)]
        end
      with e -> 
        let adr = Hashtbl.find var_in_stack key in
        bd := !bd @ [Load (adr, Reg value)] 
    ) var_to_register; 
;; 

let register_store_param = "r12";;

let store_to_regs_params lst bd var_to_register var_in_stack =
  let parameters_register = ["r0"; "r1"; "r2"; "r3"] in 
  let rec store lst count = 
    match lst with 
    | hd :: tl -> 
        if count < 4 then begin 
          (try
            let r = Hashtbl.find var_to_register hd in 
            let new_r = "r" ^ string_of_int count in
            if String.compare r new_r < 0 then
              let adr = Hashtbl.find var_in_stack hd in
              bd := !bd @ [Load (adr, Reg new_r)];
            else if String.compare r new_r <> 0 then
              bd := !bd @ [Let (new_r, Reg r)];
            store tl (count + 1)            
           with Not_found -> 
             let adr = Hashtbl.find var_in_stack hd in
             let new_r = "r" ^ string_of_int count in
             bd := !bd @ [Load (adr, Reg new_r)];
             store tl (count + 1)
          )
        end 
        else 
          begin
            (try 
               let r = Hashtbl.find var_to_register hd in 
               bd:= !bd @ [Push  (r)];
               store tl (count + 1)
             with e -> 
                let adr_in_stack = Hashtbl.find var_in_stack hd in 
                bd:= !bd @ [Load (adr_in_stack, (Reg register_store_param))];
                bd:= !bd @ [Push  (register_store_param)];
                store tl (count + 1) 
            ) 
          end 
    | [] -> ()
  in store lst 0 
;; 

let premiers_elements l =
  let final = ref [] in
  let rec npe l count = 
    if count == num_registers 
    then ()
    else 
      match l with 
      | [] -> ()
      | e :: ll -> final := !final @ [e];
          (npe ll (count + 1))
  in npe l 0; final
;; 

let remove_var_to_register var_to_register  =
  Hashtbl.iter (fun key _ ->
      Hashtbl.remove var_to_register key 
    )var_to_register ;;

let create_copy_hash var_to_register =
  let newh = Hashtbl.create 0 in
  Hashtbl.iter (fun key value ->
      Hashtbl.add newh key value; 
    ) var_to_register; newh

let getKeys hashmap = 
  let active = ref [] in
  Hashtbl.iter (fun key _ ->
      active := !active @ [key]
    ) hashmap ; active 
;;

let init_var_to_register_func var_to_register var_in_stack list_param =
  let rec parcours_param list count =
    match list with
    | hd :: tl -> 
        if count < 4 then begin
          let new_r = "r" ^ string_of_int count in
          Hashtbl.add var_to_register hd new_r;
          parcours_param tl (count + 1)
        end
        else
          let adr = string_of_int ((8 + (11 * 4)) + (((List.length list_param - 4) - (count - 3)) * 4)) in
          Hashtbl.add var_in_stack hd adr; 
          parcours_param tl (count + 1)
    | [] -> ()
  in
  parcours_param list_param 0
;;

let parcours asml =
  let new_body = ref [] in 
  let rec parcours_asmt asmt bd var_to_register var_in_stack =
    match asmt with
    | LET (var1, var2, exp) -> 
        let active = get_intervals_i asmt var_to_register [] in
        store_load active bd var_to_register var_in_stack;
        let r = Hashtbl.find var_to_register var1 in
        Hashtbl.remove var_to_register var1;
        reg_available := !reg_available @ [r];
        let expr = parcours_expr var2 bd var_to_register active var_in_stack in
        Hashtbl.add var_to_register var1 r;
        remove_e_list_ref r;
        bd := !bd @ [Let (r, expr)]; 
        parcours_asmt exp bd var_to_register var_in_stack
    | EXP expr -> 
        let active = get_intervals_i asmt var_to_register [] in
        store_load active bd var_to_register var_in_stack;
        bd := !bd @ [Exp (parcours_expr expr bd var_to_register active var_in_stack)];
        bd 
  and parcours_id_or_im id_or_im var_to_register =
    match id_or_im with
    | Const n -> Int n
    | Var r ->  Reg (Hashtbl.find var_to_register r) 
                    
  and parcours_expr expr bd var_to_register active var_in_stack =
    match expr with
    | VAL v -> 
        parcours_id_or_im v var_to_register
    | NEG s ->
        Neg (Hashtbl.find var_to_register s)
    | ADD (s, id_or_im) -> 
        Add (Hashtbl.find var_to_register s, parcours_id_or_im id_or_im var_to_register)
    | SUB (s, id_or_im) -> 
        Sub (Hashtbl.find var_to_register s, parcours_id_or_im id_or_im var_to_register)
    | NOP -> Unit
    | CALL (s, ls) ->
        (*  update_reg_available var_to_register; A reflechir *)
        (* store_load active bd var_to_register; *) 
        store_to_regs_params ls bd var_to_register var_in_stack ; 
        Call (s) 
    | IFEQ ((s,i_o_s) , asmt1, asmt2) -> 
        parcours_if "eq" s i_o_s asmt1 asmt2 var_to_register var_in_stack
    | IFLE ((s,i_o_s) , asmt1, asmt2) -> 
        parcours_if "le" s i_o_s asmt1 asmt2 var_to_register var_in_stack
    | IFGE ((s,i_o_s) , asmt1, asmt2) -> 
        parcours_if "ge" s i_o_s asmt1 asmt2 var_to_register var_in_stack
    | _ -> Unit

  and parcours_if ifexpr s i_o_s asmt1 asmt2 var_to_register var_in_stack = 
    let new_s = Hashtbl.find var_to_register s in
    let new_ios = parcours_id_or_im i_o_s var_to_register in
    let bd_asmt2 = !(parcours_asmt_if asmt2 var_to_register var_in_stack) in 
    let bd_asmt1 = !(parcours_asmt_if asmt1 var_to_register var_in_stack) in 
    If (ifexpr, (new_s,new_ios),bd_asmt1,bd_asmt2)
    
    
  and parcours_asmt_if asmt var_to_register var_in_stack = 
    let var_to_register_asmt = create_copy_hash var_to_register in
    let active = get_intervals_i asmt var_to_register_asmt [] in
    let body_asmt = ref [] in
    store_load active body_asmt var_to_register_asmt var_in_stack;
    let regt = parcours_asmt asmt body_asmt var_to_register_asmt var_in_stack in
    load_if var_to_register var_to_register_asmt body_asmt var_in_stack;
    update_reg_available var_to_register;
    body_asmt
    
      
  and parcours_asml_list asml_list =
    match asml_list with
    | Main hd :: tl -> 
        let var_to_register = Hashtbl.create 0 in
        let var_in_stack = Hashtbl.create 0 in
        let active = get_intervals_i hd var_to_register [] in
        reg_available := registers;
        fill_var_to_register active var_to_register;
        let new_func : reg_function = {
          name = "_start"; 
          body = !(parcours_asmt hd (ref []) var_to_register var_in_stack) @ [Exp (Call ("_min_caml_exit"))];
        } in 
        new_body:= !new_body @ [Fun new_func];
        parcours_asml_list tl 
    | LetFloat _ :: tl -> !new_body (* À définir *)
    | LetLabel (fun_name, list_params, hd) :: tl ->
      let var_to_register = Hashtbl.create 0 in
      let var_in_stack = Hashtbl.create 0 in
      let param_length = List.length list_params in
      num_params := (min 5 param_length) :: !num_params;
      let active = get_intervals_i hd var_to_register list_params in
      reg_available := registers;
      let body_func = ref [] in
      let temp_loads = ref [] in
      init_var_to_register_func var_to_register var_in_stack list_params;
    
      for i = 0 to min 3 (List.length list_params - 1) do
        let param_reg = "r" ^ string_of_int i in
        let param_offset = (i + 1) * 4 in
        let param_address = "-" ^ string_of_int param_offset in
        body_func := [Store (Reg param_reg, param_address)] @ !body_func;
        Hashtbl.add var_in_stack (List.nth list_params i) param_address;
      done;
    
      for i = 0 to min 3 (List.length list_params - 1) do
        let param_reg = "r" ^ string_of_int i in
        let param_offset = (i + 1) * 4 in
        let param_address = "-" ^ string_of_int param_offset in
        temp_loads := [Load (param_address, Reg param_reg)] @ !temp_loads;
      done;
    
      let new_func : reg_function = {
        name = fun_name; 
        body = !(parcours_asmt hd body_func var_to_register var_in_stack) @ !temp_loads;
      } in 
      new_body := !new_body @ [Fun new_func];
    
      let temp =
        match !num_params with
        | _ :: rest ->
          num_params := rest;
        | [] -> assert false in  
      parcours_asml_list tl  
    | [] -> !new_body
  in 
  parcours_asml_list asml;
;; 

let rec print_reg_expr reg_expr =
  match reg_expr with
  | Int i -> Printf.printf "(Int : %d)" i
  | Neg s -> Printf.printf "(Neg :%s)" s
  | Add (s, expr) -> Printf.printf "(Add (%s," s ;
      print_reg_expr expr;
      Printf.printf "))";
  | Sub (s, expr) -> Printf.printf "(Sub (%s," s ;
      print_reg_expr expr;
      Printf.printf "))";
  | Call (name) ->
      Printf.printf "Call(%s)" name 
  | If (s,(s1,ios),regt1,regt2) -> 
      Printf.printf "If%s (%s," s s1;
      print_reg_expr ios;
      Printf.printf ") then \n";
      print_list regt1;
      Printf.printf "else \n";
      print_list regt2;
  | Reg s -> Printf.printf  "Reg %s" s
  | Unit -> Printf.printf  "Unit"

and print_regt  regt =
  match regt with
  | Let (s, expr) -> Printf.printf "(Let (%s," s ;
      print_reg_expr expr;
      Printf.printf "))\n";
  | Exp expr -> Printf.printf "(Exp (" ;
      print_reg_expr expr;
      Printf.printf "))\n";
  | Store (expr, s) -> Printf.printf " (Store (" ;
      print_reg_expr expr;
      Printf.printf ", %s)) " s;
  | Load (s, expr) -> Printf.printf " (Load (%s," s;
      print_reg_expr expr;
      Printf.printf ")) "
  | Push (s) -> Printf.printf " (Push {%s}))" s;
  
and print_reg_function reg_function =
  match reg_function with 
  | Fun f :: tl ->  Printf.printf "Function name: %s\n" f.name;
      Printf.printf "Body:\n";
      List.iter (fun regt -> print_regt regt) f.body;
      print_reg_function tl
  | [] -> ()

and print_list l = 
  match l with 
  | hd :: tl -> 
      print_regt hd;
      print_list tl
  | [] -> ()
;;
