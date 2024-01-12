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

and regt= 
  | Let of Id.t * reg_expr 
  | Exp of reg_expr
  | Store of reg_expr * Id.t 
  | Load of Id.t * reg_expr 
      
and letregdef = 
  | Fun of reg_function
  
and reg_function = {
  name : Id.l;
  body : regt list
}
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
 
(* Hashmap des variables sur la pile avec leurs adresses, exemple : "x" -> "[fp, #4]" *)
let var_in_stack = Hashtbl.create 0

(* parcours_asmt
 * hashtable binding variable to register,
 * a : r1 -> variable `a` is in register `r1` 
*)
let reg_available = ref ["r4";"r5";"r6";"r7";"r8";"r9";"r10"];;

let get_intervals_i asml = 
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
      if not (List.exists (fun v -> v = var) !list) then 
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
  in
  i_intervals_asmt asml ;
  let l = list in l
;;

             (* and i_intervals asml =
                 match asml with
                 | _ when (List.length !list) = num_registers -> () 
                 | (Main hd) :: tail -> 
                     i_intervals_asmt hd;
                     i_intervals tail
                 | LetFloat _ :: tl -> () (* A definir *)
                 | LetLabel (_,_,_) :: tl -> () (* A definir *) *)

;;
(* on va renvoyer asml avce le nom de la fonction quand on appelle i_get_interval *)

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
 
let filter_hashtable_by_keys hashtable keys = 
  let result_hashtable = Hashtbl.create (Hashtbl.length hashtable) in 
  List.iter (fun key -> 
      if Hashtbl.mem hashtable key then 
        Hashtbl.add result_hashtable key (Hashtbl.find hashtable key) 
    ) keys; 
  result_hashtable 
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

let rec active_add act_add var_to_register =
  match act_add with
  | hd :: tl -> 
      let r = List.hd !reg_available in
      Hashtbl.add var_to_register hd r;
      remove_e_list_ref r;
      active_add tl var_to_register
  | [] -> ()

let store_load intervals body var_to_register =
   (* a la premiere ligne hashtable est vide, on y met les 9 variables *)
  if (Hashtbl.length var_to_register) = 0 then begin 
    let rec add_hashmap n inter =
      match inter with
      | [] -> ()
      | _ when n >= num_registers -> ()
      | var :: tail -> 
          Hashtbl.add var_to_register var ("r" ^ string_of_int (n + 4)); (* + 4  car commence a r4 *)
          remove_e_list_ref ("r" ^ string_of_int (n + 4));                                                              
          add_hashmap (n + 1) tail
    in
    add_hashmap 0 !intervals; 
  end
  else
     (* sinon, on recupere la liste des var a store *)
    let rec parcours_store list_store active_a_ajouter =
      match list_store with
      | (v,r) ::  tl ->
          Hashtbl.remove var_to_register v; 
          reg_available := !reg_available @ [r];
          (try
             let adr = Hashtbl.find var_in_stack v in
             body := !body @ [Store ((Reg r), adr)];
           with Not_found ->
             let adr = "[fp, #-" ^ string_of_int (((Hashtbl.length var_in_stack) + 1) * 4) ^ "]" in
             Hashtbl.add var_in_stack v adr;
             body := !body @ [Store ((Reg r), adr)]);
          (try
             let var = List.hd active_a_ajouter in
             Hashtbl.add var_to_register var r;
             remove_e_list_ref r ;
             (try
                  (*  si la var est presente dans la pile, on load *)
                let value = Hashtbl.find var_in_stack var in
                body := !body @ [Load (value, Reg r)]; 
              with e -> ());
           with e -> ());
          parcours_store tl (try (List.tl active_a_ajouter) with Failure tl -> []);
      | [] -> active_add active_a_ajouter var_to_register
    in 
    let store = var_not_in_list var_to_register !intervals in 
    let active_a_ajouter = var_not_in_hash var_to_register !intervals in 
    parcours_store store active_a_ajouter; 
    ()
;; 

let store_to_regs_params lst bd var_to_register=
  (*let params = Hashtbl.create 4 in*)
  let rec store lst count = 
    match lst with 
    | hd :: tl -> 
        (try
           let r = Hashtbl.find var_to_register hd in 
           let new_r = "r" ^ string_of_int count in
           bd := !bd @ [Let (new_r, Reg r)];
           store tl (count + 1)
         with Not_found -> 
           let adr = Hashtbl.find var_in_stack hd in
           let new_r = "r" ^ string_of_int count in
           bd := !bd @ [Load (adr, Reg new_r)];
           store tl (count + 1)
        ); 
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

let parcours asml =
  let new_body = ref [] in 
  let rec parcours_asmt asmt bd var_to_register =
    match asmt with
    | LET (var1, var2, exp) -> 
        let active = get_intervals_i asmt in
        store_load active bd var_to_register;
        let r = Hashtbl.find var_to_register var1 in
        let expr = parcours_expr var2 bd var_to_register active in
        bd := !bd @ [Let (r, expr)]; 
        parcours_asmt exp bd var_to_register;
    | EXP expr -> 
        let active = get_intervals_i asmt in
        store_load active bd var_to_register;
        bd := !bd @ [Exp (parcours_expr expr bd var_to_register active)];
        bd 
  and parcours_id_or_im id_or_im var_to_register =
    match id_or_im with
    | Const n -> Int n
    | Var r ->  Reg (Hashtbl.find var_to_register r) 
    
                  
  and parcours_expr expr bd var_to_register active =
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
        store_load active bd var_to_register; 
        store_to_regs_params ls bd var_to_register;
        Call (s) 
    | IFEQ ((s,i_o_s) , asmt1, asmt2) -> 
        
        let s1 = Hashtbl.find var_to_register s in
        let i = parcours_id_or_im i_o_s var_to_register in
        let active1 = get_intervals_i asmt1 in
        let new_hash = filter_hashtable_by_keys var_to_register !active1 in
        let new_bd = ref [] in
        store_load active1 new_bd var_to_register;
        let a1 = !(parcours_asmt asmt1 new_bd new_hash) in
        let active1 = get_intervals_i asmt2 in
        let new_hash1 = filter_hashtable_by_keys var_to_register !active1 in
        let a2 = !(parcours_asmt asmt2 (ref []) new_hash1) in
        If ("eq",(s1, i), a1, a2)
    | IFLE ((s,i_o_s) , asmt1, asmt2) -> 
        let s1 = Hashtbl.find var_to_register s in
        let i = parcours_id_or_im i_o_s var_to_register in
        let active1 = get_intervals_i asmt1 in
        let new_hash = filter_hashtable_by_keys var_to_register !active1 in
        let new_bd = ref [] in
        store_load active1 new_bd var_to_register;
        let a1 = !(parcours_asmt asmt1 new_bd new_hash) in
        let active1 = get_intervals_i asmt2 in
        let new_hash1 = filter_hashtable_by_keys var_to_register !active1 in
        let a2 = !(parcours_asmt asmt2 (ref []) new_hash1) in
        If ("le",(s1, i), a1, a2)
    | IFGE ((s,i_o_s) , asmt1, asmt2) -> 
        let s1 = Hashtbl.find var_to_register s in
        let i = parcours_id_or_im i_o_s var_to_register in
        let active1 = get_intervals_i asmt1 in
        let new_hash = filter_hashtable_by_keys var_to_register !active1 in
        let new_bd = ref [] in
        store_load active1 new_bd var_to_register;
        let a1 = !(parcours_asmt asmt1 new_bd new_hash) in
        let active1 = get_intervals_i asmt2 in
        let new_hash1 = filter_hashtable_by_keys var_to_register !active1 in
        let a2 = !(parcours_asmt asmt2 (ref []) new_hash1) in
        If ("ge",(s1, i), a1, a2)
    | _ -> Unit

  and parcours_asml_list asml_list =
    match asml_list with
    | Main hd :: tl -> 
        let var_to_register = Hashtbl.create 0 in
        let new_func : reg_function = {
          name = "main"; 
          body = !(parcours_asmt hd (ref []) var_to_register) @ [Exp (Call ("_min_caml_exit"))];
        } in 
        new_body:= !new_body @ [Fun new_func];
        parcours_asml_list tl 
    | LetFloat _ :: tl -> !new_body (* À définir *)
    | LetLabel (_,_,_) :: tl-> !new_body (* À définir *) 
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