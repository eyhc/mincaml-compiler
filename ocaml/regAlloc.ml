open Asml;;

(* type pour la partie back-end *)
type reg_expr =
  | Int of int 
  | Neg of Id.t
  | Add of Id.t * reg_expr
  | Sub of Id.t * reg_expr
  | Call of Id.l * int
  | CallClo of Id.l * int
  | If of Id.l * (Id.t*reg_expr) * regt list * regt list
  | Reg of Id.t
  | MemAssign of Id.t 
  | Adresse of Id.t
  | Label of Id.t
  | Unit

and regt= 
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
  

(* Fonction pour print les hashmap et listes de string *)
let print_hashtable my_hashtable =
  Hashtbl.iter (fun key value ->
      Printf.printf "%s -> %s\n" key value
    ) my_hashtable

let print_free my_list =
  List.iter (fun elem ->
      Printf.printf "%s\n" elem
    ) my_list


(* Nombres de registres pour les variables locales *)
let num_registers = 7
(* Registres *)
let registers = ["r4";"r5";"r6";"r7";"r8";"r9";"r10"];;
(* Registres disponible *)
let reg_available = ref registers;;
(* Registres pour les parametres des fonctions *)
let register_param = ["r0";"r1";"r2";"r3"];;
(* Registres utilisés pour store les parametres des fonctions *)
let register_store_param = "r12";;

(*------------------------------ FONCTION AUXILIAIRE -----------------------------*)
let quatre_premiers_elem list_params = 
  let rec fonc lst count = 
    match lst with 
    | e :: ll -> 
        if count <= 3 then 
          e :: fonc ll (count + 1)
        else
          []
    | [] -> []
  in 
  fonc list_params 0
;;


let calcul_adr var_in_stack list_params =
(* Retourne len(param) - 4 ou 0 si negatif *)
  let nb_param lst =
    let len = List.length lst in
    if len <= 4 then 0
    else len - 4
  in
  "-" ^ string_of_int (((Hashtbl.length var_in_stack) - nb_param list_params  + 1) * 4)
;;

(* Supprime un element d'une liste ref *)
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

(* Retourne une liste de var qui doivent etre store car presente dans la hashmap mais pas dans la liste active *)
let var_not_in_list hashmap active =
  let strings_in_list = List.map (fun str -> str) active in
  Hashtbl.fold (fun key value acc -> if not (List.mem key strings_in_list) then (key,value) :: acc else acc) hashmap []
;;

(* Retourne une liste des var a mettre dans les registres car presente dans la hashmap mais pas dans active (load ou let)*)
let var_not_in_hash hashmap active =
  List.fold_left (fun acc key ->
      match Hashtbl.find_opt hashmap key with
      | Some _ -> acc
      | None -> key :: acc
    ) [] active
;;

(* Remplis les registres au début du main avec les 7 premieres variables utilisées *)
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

(* Retourne true si ladresse est "negatif" : fp - 4
false sinon : fp + 4 *)
let is_pos_adr chaine =
  if String.length chaine >= 7 then
    chaine.[6] = '-'
  else
    false
;;

(* Creer une copie d'une hashmap *)
let create_copy_hash hashmap =
  let newh = Hashtbl.create 0 in
  Hashtbl.iter (fun key value ->
      Hashtbl.add newh key value;
    ) hashmap; newh

(* Met a jour les registres disponible en fonction de la hashmap des variables associées aux registres *)
let update_reg_available var_to_register =
  reg_available := registers;
  Hashtbl.iter (fun key value ->
      remove_e_list_ref value;
    ) var_to_register;
;;

(* Parcours la liste de variable a ajouter dans les registres et load si la var est deja presente dans la pile *)
let rec active_add act_add bd var_to_register var_in_stack =
  match act_add with
  | hd :: tl ->
      (try Hashtbl.find var_to_register hd; () with e ->
         update_reg_available var_to_register;
         let r = List.hd !reg_available in
         (try
            let value = Hashtbl.find var_in_stack hd in
            bd:= !bd @ [Load (value, Reg r)];
          with e -> ());
         remove_e_list_ref r;
         Hashtbl.add var_to_register hd r;
         active_add tl bd var_to_register var_in_stack)
  | [] -> ()

(* Parcours un asml et donne les 7 prochaines variable utilisées 
   Cas particulier : 
    - IFEQ ((s,i_o_s) , asmt1, asmt2) : ne rentre pas dans les asmt 
- Functions : les variables qui sont dans r0, r1, r2 et r3 ne seront pas ajoutées a active *)
let get_intervals_i asml var_to_register list_param=
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
      if not (List.exists (fun v -> v = var) !list) && not (List.exists (fun v -> v = var) (quatre_premiers_elem list_param))then
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
    | MEMASSIGN (_, _,s2) ->
        i_intervals_string s2;
    | MEMGET (s, i_o_s ) ->
        i_intervals_string s;
        i_intervals_id_or_imm i_o_s;
    | NEW i_o_s ->
        i_intervals_id_or_imm i_o_s;
    | _ when (List.length !list) = num_registers -> ()
    | _ -> ()

  and get_keys hashtable =
    Hashtbl.iter (fun key _ ->
        i_intervals_string key
      ) hashtable;

  in
  i_intervals_asmt asml ;
  let l = list in l
;;

(* Recupere une liste de var a ajouter dans les registres et a store, puis parcours la liste de variable a store,
a chaque store on ajoute une autre variable a se registres. *)
let store_load intervals body var_to_register var_in_stack list_params =
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
              if not (is_pos_adr adr) then begin
                Hashtbl.add var_in_stack v adr end
              else begin
                body := !body @ [Store ((Reg r), adr)];
              end
            with Not_found ->
              let adr = calcul_adr var_in_stack list_params in
              Hashtbl.add var_in_stack v adr;
              body := !body @ [Store ((Reg r), adr)]);
           (try
              let var = List.hd active_a_ajouter in
              Hashtbl.add var_to_register var r;
              remove_e_list_ref r;
              (try
                 let value = Hashtbl.find var_in_stack var in
                 body := !body @ [Load (value, Reg r)];
               with e -> ());
            with e -> ());
           parcours_store tl (try (List.tl active_a_ajouter) with Failure tl -> []));
    | [] -> active_add active_a_ajouter body var_to_register var_in_stack
  in
  let store = var_not_in_list var_to_register !intervals in
  let active_a_ajouter = var_not_in_hash var_to_register !intervals in
  parcours_store store active_a_ajouter;
  ()
;;

(* Prend en parametre la var_to_registres avant le if et la var_to_registres d'un then ou else,
   puis on load toutes les variables qui ne sont pas dans la premiere hashmap ou qui ont un registres different afin de 
revenir a l'environnement de depart *)
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

(* Gere les parametres d'une fonction juste au moment d'un call, les trois premiers sont dans r0, r1, r2 et r3 et les autres sont sur la pile *)
let store_to_regs_params lst bd var_to_register var_in_stack  =
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

(* Initialise var_to_register dans la fonction avec les 4 premiers registres qui contienne les parametres et ajoute dans var_in_stack les autres 
parametres avec des adresses positives : fp + 4*)
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
          let adr = string_of_int (8 + (7 * 4) + (((List.length list_param - 4) - (count - 3)) * 4)) in
          Hashtbl.add var_in_stack hd adr;
          parcours_param tl (count + 1)
    | [] -> ()
  in
  parcours_param list_param 0
;;

(* Parcours l'asml et renvoie une liste de letregdef *)
let parcours asml =
  let new_body = ref [] in
  let rec parcours_asmt asmt bd var_to_register var_in_stack list_params =
    match asmt with
    | LET (var1, (MEMASSIGN (tuple, _, var)), exp) ->
        let active = get_intervals_i asmt var_to_register list_params in
        store_load active bd var_to_register var_in_stack list_params;
        let r = Hashtbl.find var_to_register var1 in
        Hashtbl.remove var_to_register var1;
        reg_available := !reg_available @ [r];
        
        parcours_asmt exp bd var_to_register var_in_stack list_params;
    | LET (var1, var2, exp) ->
        let active = get_intervals_i asmt var_to_register list_params in
        store_load active bd var_to_register var_in_stack list_params;
        let r = Hashtbl.find var_to_register var1 in

        (match var2 with
           (*| MEMGET (var, Const n) ->
             let adr = (Hashtbl.find var_in_stack var) + 4 + n in
             let reg = Hashtbl.find var_to_register
         
         | MEMASSIGN (tuple, _, var) ->
         *)    
             (*MemAssign (r) *)
         | NEW i_o_s ->
             let adr = calcul_adr var_in_stack list_params in
             let next_adr = (int_of_string adr) - 4 in
             Hashtbl.add var_in_stack var1 adr;
             bd := !bd @ [Let (r, Adresse (string_of_int next_adr)); Store (Reg r, adr)];
             Hashtbl.remove var_to_register var1;
             reg_available := !reg_available @ [r];
         | LABEL s ->
             let adr = calcul_adr var_in_stack list_params in
             bd := !bd @ [Let (r, Label (s))];
         | _ ->
             Hashtbl.remove var_to_register var1;
             reg_available := !reg_available @ [r];
             let expr = parcours_expr var2 bd var_to_register active var_in_stack list_params in
             Hashtbl.add var_to_register var1 r;
             remove_e_list_ref r;
             bd := !bd @ [Let (r, expr)];
        );
        parcours_asmt exp bd var_to_register var_in_stack list_params;
    | EXP expr ->
        let active = get_intervals_i asmt var_to_register list_params in
        store_load active bd var_to_register var_in_stack list_params;
        bd := !bd @ [Exp (parcours_expr expr bd var_to_register active var_in_stack list_params )];
        bd
  and parcours_id_or_im id_or_im var_to_register =
    match id_or_im with
    | Const n -> Int n
    | Var r ->  Reg (Hashtbl.find var_to_register r)

  and parcours_expr expr bd var_to_register active var_in_stack list_params  =
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
        store_to_regs_params ls bd var_to_register var_in_stack;
        let first_element = match !num_params with
          | hd :: _ -> hd
          | [] -> 0
        in
        Call (s, first_element)
    | IFEQ ((s,i_o_s) , asmt1, asmt2) ->
        parcours_if "eq" s i_o_s asmt1 asmt2 var_to_register var_in_stack list_params
    | IFLE ((s,i_o_s) , asmt1, asmt2) ->
        parcours_if "le" s i_o_s asmt1 asmt2 var_to_register var_in_stack list_params
    | IFGE ((s,i_o_s) , asmt1, asmt2) ->
        parcours_if "ge" s i_o_s asmt1 asmt2 var_to_register var_in_stack list_params
    
(* get *)
    | _ -> Unit

  and parcours_if ifexpr s i_o_s asmt1 asmt2 var_to_register var_in_stack list_params  =
    let new_s = Hashtbl.find var_to_register s in
    let new_ios = parcours_id_or_im i_o_s var_to_register in
    let bd_asmt1 = !(parcours_asmt_if asmt1 var_to_register var_in_stack list_params ) in
    let bd_asmt2 = !(parcours_asmt_if asmt2 var_to_register var_in_stack list_params ) in
    If (ifexpr, (new_s,new_ios),bd_asmt1,bd_asmt2)


  and parcours_asmt_if asmt var_to_register var_in_stack list_params =
    let var_to_register_asmt = create_copy_hash var_to_register in
    let active = get_intervals_i asmt var_to_register_asmt list_params in
    let body_asmt = ref [] in
    store_load active body_asmt var_to_register_asmt var_in_stack list_params;
    let regt = parcours_asmt asmt body_asmt var_to_register_asmt var_in_stack list_params  in
    load_if var_to_register var_to_register_asmt body_asmt var_in_stack;
    update_reg_available var_to_register;
    body_asmt


  and parcours_asml_list asml_list =
    match asml_list with
    | Main hd :: tl ->
        let var_to_register = Hashtbl.create 0 in
        let var_in_stack = Hashtbl.create 0 in
        let active = get_intervals_i hd var_to_register [] in
        reg_available := registers ;
        fill_var_to_register active var_to_register;
        let new_func : reg_function = {
          name = "_start";
          body = !(parcours_asmt hd (ref []) var_to_register var_in_stack []) @ [Exp (Call ("_min_caml_exit", 0))];
        } in
        new_body:= !new_body @ [Fun new_func];
        parcours_asml_list tl
    | LetFloat _ :: tl -> !new_body (* À définir *)
    | LetLabel (fun_name, list_params, hd) :: tl ->
        let var_to_register = Hashtbl.create 0 in
        let var_in_stack = Hashtbl.create 0 in
        num_params := (List.length list_params) :: !num_params;
        reg_available := registers ;
        let body_func = ref [] in
        init_var_to_register_func var_to_register var_in_stack list_params;

        for i = 0 to min 3 (List.length list_params - 1) do
          let param_reg = "r" ^ string_of_int i in
          let param_offset = (i + 1) * 4 in
          let param_address = "-" ^ string_of_int param_offset in
          body_func := [Store (Reg param_reg, param_address)] @ !body_func;
          Hashtbl.add var_in_stack (List.nth list_params i) param_address;
        done;

        let new_func : reg_function = {
          name = fun_name;
          body = !(parcours_asmt hd body_func var_to_register var_in_stack list_params);
        } in
        new_body := !new_body @ [Fun new_func];
        (match !num_params with
         | _ :: rest ->
             num_params := rest;
         | [] -> assert false);
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
  | Call (name, params) ->
      Printf.printf "Call(%s, %d)" name params
  | If (s,(s1,ios),regt1,regt2) ->
      Printf.printf "If%s (%s," s s1;
      print_reg_expr ios;
      Printf.printf ") then \n";
      print_lists regt1;
      Printf.printf "else \n";
      print_lists regt2;
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
  | Push (s) -> Printf.printf " (Push (%s))" s;

and print_reg_function reg_function =
  match reg_function with
  | Fun f :: tl ->  Printf.printf "Function name: %s\n" f.name;
      Printf.printf "Body:\n";
      List.iter (fun regt -> print_regt regt) f.body;
      print_reg_function tl
  | [] -> ()

and print_lists l =
  match l with
  | hd :: tl ->
      print_regt hd;
      print_lists tl
  | [] -> ()
;;