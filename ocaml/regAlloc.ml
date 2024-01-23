open Asml;;

(* type pour la partie back-end *)
type reg_expr =
  | Int of int 
  | Neg of Id.t
  | Add of Id.t * reg_expr
  | Sub of Id.t * reg_expr
  | Fneg of Id.t
  | Fadd of Id.t * Id.t 
  | Fsub of Id.t * Id.t 
  | Fmul of Id.t * Id.t 
  | Fdiv of Id.t * Id.t 
  | Iffequal of (Id.t*Id.t) * regt list * regt list
  | Iffle of (Id.t*Id.t) * regt list * regt list
  | Call of Id.l * int
  | CallClo of Id.l * int
  | If of Id.l * (Id.t*reg_expr) * regt list * regt list
  | Reg of Id.t
  | MemGet of Id.t * Id.t * int
  | MemGetTab of Id.t * Id.t
  | Adresse of Id.t
  | Label of Id.t
  | Unit

and regt= 
  | Let of Id.t * reg_expr 
  | Exp of reg_expr
  | Store of reg_expr * Id.t 
  | Load of Id.t * reg_expr 
  | LoadReg of Id.t * reg_expr 
  | Push of Id.t
  | MemAssign of Id.t * Id.t * Id.t
  
  and letregdef = 
    | Fun of reg_function
    | LetFloatReg of Id.l * Id.t
    
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
let num_registers_float = 30
(* Registres *)
let registers = ["r4";"r5";"r6";"r7";"r8";"r9";"r10"];;
(* Registres disponible *)
let reg_available = ref registers;;
(* Registres pour les parametres des fonctions *)
let register_param = ["r0";"r1";"r2";"r3"];;
(* Registres utilisés pour store les parametres des fonctions *)
let register_store_param = "r12";;
(* hashmap de `nom de float` -> registre *)
let float_to_reg = Hashtbl.create 0;;
let float = Hashtbl.create 0;;
(* Registres pour float *)
let float_reg_available = ref [ "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"; "s12"; "s13"; "s14"; "s15"; "s16"; "s17"; "s18"; "s19"; "s20"; "s21"; "s22"; "s23"; "s24"; "s25"; "s26"; "s27"; "s28"; "s29"; "s30"; "s31"];;
let register_float = [ "s1"; "s2"; "s3"; "s4"; "s5"; "s6"; "s7"; "s8"; "s9"; "s10"; "s11"; "s12"; "s13"; "s14"; "s15"; "s16"; "s17"; "s18"; "s19"; "s20"; "s21"; "s22"; "s23"; "s24"; "s25"; "s26"; "s27"; "s28"; "s29"; "s30"; "s31"];;

let new_sizes = Hashtbl.create 0;;


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
let remove_e_list_ref e available_reg =
  let rec r_lr list =
    match list with
    | hd :: ll ->
        if e = hd then
          r_lr ll
        else
          hd :: r_lr ll
    | [] -> []
  in available_reg := (r_lr !available_reg)
;;
(* Supprime un element d'une liste ref *)
let remove_e_list_ref2 e list_ref =
  let rec r_lr list =
    match list with
    | hd :: ll ->
        if e = hd then
          r_lr ll
        else
          hd :: r_lr ll
    | [] -> []
  in list_ref := (r_lr !list_ref)
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
        remove_e_list_ref ("r" ^ string_of_int (n + 4)) reg_available;
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

let vider_hash hashmap = 
  Hashtbl.iter (fun key _ ->
      Hashtbl.remove hashmap key;
    ) hashmap;;

  
(* Met a jour les registres disponible en fonction de la hashmap des variables associées aux registres *)
let update_reg_available var_to_register available_reg is_float=
  if is_float = 0 then
    available_reg:= registers
  else
    available_reg:= register_float ;
  Hashtbl.iter (fun key value ->
      remove_e_list_ref value available_reg;
    ) var_to_register;
;;

(* Parcours la liste de variable a ajouter dans les registres et load si la var est deja presente dans la pile *)
let rec active_add act_add bd var_to_register var_in_stack available_reg is_float=
  match act_add with
  | hd :: tl ->
      (try Hashtbl.find var_to_register hd; () with e ->
         update_reg_available var_to_register available_reg is_float;
         let r = List.hd !available_reg in
         (try
            let value = Hashtbl.find var_in_stack hd in
            bd:= !bd @ [Load (value, Reg r)];
          with e -> ());
         remove_e_list_ref r available_reg;
         Hashtbl.add var_to_register hd r;
         active_add tl bd var_to_register var_in_stack available_reg is_float)
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
    | CALLCLO (s,hd::tl) ->
        i_intervals_string s;
        i_intervals_string hd;
        i_intervals_expr (CALL (s,tl))
    | CALLCLO (_, []) -> ()
    | IFEQ ((s,i_o_s) , asmt1, asmt2) | IFLE ((s,i_o_s) , asmt1, asmt2) | IFGE ((s,i_o_s) , asmt1, asmt2) ->
        i_intervals_string s;
        i_intervals_id_or_imm i_o_s;
        get_keys var_to_register
    | MEMASSIGN (t, i_o_s,s2) ->
        i_intervals_string t;
        i_intervals_id_or_imm i_o_s;
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

let get_intervals_i_float asml float_to_reg list_param=
  let list = ref [] in
  let rec i_intervals_asmt asmt =
    match asmt with
    | LET (var1, var2, exp) ->
        if not ((List.length !list) = num_registers_float) then begin
          i_intervals_string var1;
          i_intervals_expr var2;
          i_intervals_asmt exp;
        end
        
    | EXP expr -> i_intervals_expr expr;

  and i_intervals_string var =
    if not ((List.length !list) = num_registers_float) then begin
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
    | FNEG s ->
        i_intervals_string s
    | FADD (s1, s2) | FSUB (s1, s2) | FMUL (s1, s2) | FDIV (s1, s2) ->
        i_intervals_string s1;
        i_intervals_string s2;
    | NOP -> ()
    | CALL (s,hd::tl) ->
        i_intervals_string hd;
        i_intervals_expr (CALL (s,tl))
    | CALL (_, []) -> ()
    | CALLCLO (s,hd::tl) ->
        i_intervals_string s;
        i_intervals_string hd;
        i_intervals_expr (CALL (s,tl))
    | CALLCLO (_, []) -> ()
    | IFFEQUAL ((s1, s2) , asmt1, asmt2) | IFFLE ((s1, s2) , asmt1, asmt2) ->
        i_intervals_string s1;
        i_intervals_string s2;
        get_keys float_to_reg
    | MEMASSIGN (_, _,s2) ->
        i_intervals_string s2;
    | MEMGET (s,i_o_s) ->
        i_intervals_string s;
        i_intervals_id_or_imm i_o_s;
    | NEW i_o_s ->
        i_intervals_id_or_imm i_o_s;
    | _ when (List.length !list) = num_registers_float -> ()
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
let store_load intervals body var_to_register var_in_stack list_params available_reg is_float =
  if (Hashtbl.length var_to_register) = 0 then begin 
    if is_float = 0 then begin
      let rec add_hashmap n inter =
        match inter with
        | [] -> ()
        | _ when n >= num_registers -> ()
        | var :: tail -> 
            Hashtbl.add var_to_register var ("r" ^ string_of_int (n + 4)); (* + 4  car commence a r4 *)
            remove_e_list_ref ("r" ^ string_of_int (n + 4)) available_reg;                                                              
            add_hashmap (n + 1) tail
      in
      add_hashmap 0 !intervals; 
    end else begin
      let rec add_hashmap n inter =
        match inter with
        | [] -> ()
        | _ when n >= num_registers_float -> ()
        | var :: tail -> 
            Hashtbl.add var_to_register var ("s" ^ string_of_int (n+2));
            remove_e_list_ref ("s" ^ string_of_int (n+2)) available_reg;                                                              
            add_hashmap (n + 1) tail
      in
      add_hashmap 0 !intervals; 
    end 
  end
  else 
    let rec parcours_store list_store active_a_ajouter =
      match list_store with
      | (v,r) ::  tl ->
          if List.exists (fun x -> x = r) register_param then begin
            parcours_store tl (try (active_a_ajouter) with Failure tl -> [])
          end
          else
            (Hashtbl.remove var_to_register v;
             available_reg:= !available_reg @ [r];
             (try
                let adr = Hashtbl.find var_in_stack v in
                if (is_pos_adr adr) then begin
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
                remove_e_list_ref r available_reg;
                (try
                   let value = Hashtbl.find var_in_stack var in
                   body := !body @ [Load (value, Reg r)];
                 with e -> ());
              with e -> ());
             parcours_store tl (try (List.tl active_a_ajouter) with Failure tl -> []));
      | [] -> active_add active_a_ajouter body var_to_register var_in_stack available_reg is_float
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

(* Fonction qui calcul la liste des prochaines variables utilisées, et store load
   et renvoie un registre pour la variable var1 *)
let actualisation_registres asmt hash list_params bd var_in_stack var1 is_float=
  if is_float = 0 then begin 
    let active = get_intervals_i asmt hash list_params in
    store_load active bd hash var_in_stack list_params reg_available is_float;
    let r = Hashtbl.find hash var1 in
    r
  end 
  else begin 
    let active = get_intervals_i_float asmt hash list_params in
    store_load active bd hash var_in_stack list_params float_reg_available is_float; 
    let r = Hashtbl.find hash var1 in
    r
  end;;

(* Fonction qui supprime une variable de la hashmap des registres et rend son registre disponible *)
let remove_hash_register hash var r =
  Hashtbl.remove hash var;
  reg_available := !reg_available @ [r];;

(* Fonction qui calcul une adresse et l'ajoute dans la pile *)
let add_in_stack var_in_stack list_params var =
  let adr = calcul_adr var_in_stack list_params in
  Hashtbl.add var_in_stack var adr ;
  adr ;;
  
(* Parcours l'asml et renvoie une liste de letregdef *)
let parcours asml =
  let new_body = ref [] in
  let rec parcours_asmt asmt bd var_to_register var_in_stack list_params is_float =
    match asmt with
    | LET (var1, (MEMASSIGN (tuple, Const n, var)), exp) ->
        let r = actualisation_registres asmt var_to_register list_params bd var_in_stack var1 0 in
        remove_hash_register var_to_register var1 r;
        let adr = add_in_stack var_in_stack list_params var in
        let r_var = Hashtbl.find var_to_register var in 
        bd := !bd @ [Store (Reg r_var, adr)];
        remove_hash_register var_to_register var r_var;
        parcours_asmt exp bd var_to_register var_in_stack list_params is_float;
    | LET (var1, (MEMASSIGN (tab, Var idx, var)), exp) ->
        let r = actualisation_registres asmt var_to_register list_params bd var_in_stack var1 0 in
        remove_hash_register var_to_register var1 r;
        let adr = add_in_stack var_in_stack list_params var in
        let r_var = Hashtbl.find var_to_register var in 
        let r_idx = Hashtbl.find var_to_register idx in 
        let r_tab = Hashtbl.find var_to_register tab in
        bd := !bd @ [MemAssign (r_tab,r_var, r_idx)];
        remove_hash_register var_to_register var r_var;
        parcours_asmt exp bd var_to_register var_in_stack list_params is_float;
    | LET (var1, MEMGET (var, Const n), exp) ->
        (try 
           let r_var = Hashtbl.find float var in
           let r = actualisation_registres asmt float_to_reg list_params bd var_in_stack var1 1 in
           let offset = "-" ^ string_of_int (n)  in
           bd := !bd @ [Let (r, MemGet (r_var, offset,List.length list_params ))];
           parcours_asmt exp bd float_to_reg var_in_stack list_params is_float; 
         with Not_found -> 
           let r = actualisation_registres asmt var_to_register list_params bd var_in_stack var1 0 in
           let r_var = ref "" in
           if var = "%self" then begin
             r_var := "%self" ;
             let r_self = Hashtbl.find var_to_register var in
             remove_hash_register var_to_register var r_self;
           end
           else 
             r_var := Hashtbl.find var_to_register var;
           let offset = "-" ^ string_of_int (n)  in
           bd := !bd @ [Let (r, MemGet (!r_var, offset, List.length list_params))];
           parcours_asmt exp bd var_to_register var_in_stack list_params is_float); 
    | LET (var1, MEMGET (var, Var idx), exp) -> 
        let r = actualisation_registres asmt var_to_register list_params bd var_in_stack var1 0 in
        let r_var = ref "" in
        if var = "%self" then begin
          r_var := "%self" ;
          let r_self = Hashtbl.find var_to_register var in
          remove_hash_register var_to_register var r_self;
        end
        else 
          r_var := Hashtbl.find var_to_register var;
        let r_idx = Hashtbl.find var_to_register idx in
        bd := !bd @ [Let (r, MemGetTab (!r_var, r_idx))];
        parcours_asmt exp bd var_to_register var_in_stack list_params is_float; 
    | LET (var1, LABEL s, exp) -> 
        (try 
           Hashtbl.find float s;
           let r = actualisation_registres asmt var_to_register list_params bd var_in_stack var1 0 in 
           Hashtbl.add float var1 r;
           bd := !bd @ [Let (r, Label (s))];
           parcours_asmt exp bd float_to_reg var_in_stack list_params 1
         with e -> 
           let r = actualisation_registres asmt var_to_register list_params bd var_in_stack var1 0 in
           bd := !bd @ [Let (r, Label (s))];
           remove_hash_register var_to_register var1 r;
           parcours_asmt exp bd var_to_register var_in_stack list_params is_float);
    | LET (var1, var2, exp) ->
        if is_float = 0 then begin
          let r = actualisation_registres asmt var_to_register list_params bd var_in_stack var1 0 in 
          (match var2 with 
           | NEW (Const n) ->
               let adr = add_in_stack var_in_stack list_params var1 in
               let next_adr = (int_of_string adr) - 4 in
               bd := !bd @ [Let (r, Adresse (string_of_int next_adr)); Store (Reg r, adr)];
               bd := !bd @ [Push (r)];
               remove_hash_register var_to_register var1 r;
               Hashtbl.add new_sizes var1 (string_of_int n)
           | _ ->
               remove_hash_register var_to_register var1 r;
               let expr = parcours_expr var2 bd var_to_register  var_in_stack list_params is_float in
               Hashtbl.add var_to_register var1 r;
               remove_e_list_ref r reg_available;
               bd := !bd @ [Let (r, expr)];
          );
          parcours_asmt exp bd var_to_register var_in_stack list_params is_float ;
         
        end
        else begin
          let r = actualisation_registres asmt float_to_reg list_params bd var_in_stack var1 1 in 
          let expr = parcours_expr var2 bd var_to_register  var_in_stack list_params is_float in
          Hashtbl.add var_to_register var1 r;
          remove_e_list_ref r reg_available;
          bd := !bd @ [Let (r, expr)];
          parcours_asmt exp bd var_to_register var_in_stack list_params is_float ;
        end
    | EXP expr ->
        let active = get_intervals_i asmt var_to_register list_params in
        store_load active bd var_to_register var_in_stack list_params reg_available 0;
        bd := !bd @ [Exp (parcours_expr expr bd var_to_register  var_in_stack list_params is_float)];
        bd
  and parcours_id_or_im id_or_im var_to_register =
    match id_or_im with
    | Const n -> Int n
    | Var r ->  Reg (Hashtbl.find var_to_register r)

  and parcours_expr expr bd var_to_register var_in_stack list_params is_float =
    match expr with
    | VAL v ->
        parcours_id_or_im v var_to_register
    | NEG s ->
        Neg (Hashtbl.find var_to_register s)
    | ADD (s, id_or_im) ->
        Add (Hashtbl.find var_to_register s, parcours_id_or_im id_or_im var_to_register)
    | SUB (s, id_or_im) ->
        Sub (Hashtbl.find var_to_register s, parcours_id_or_im id_or_im var_to_register)
    | FNEG s ->
        Fneg (Hashtbl.find float_to_reg s)
    | FADD (s1, s2) ->
        Fadd (Hashtbl.find float_to_reg s1, Hashtbl.find float_to_reg s2)
    | FSUB (s1, s2) ->
        Fsub (Hashtbl.find float_to_reg s1, Hashtbl.find float_to_reg s2)
    | FMUL (s1, s2) ->
        Fmul (Hashtbl.find float_to_reg s1, Hashtbl.find float_to_reg s2)
    | FDIV (s1, s2) ->
        Fdiv (Hashtbl.find float_to_reg s1, Hashtbl.find float_to_reg s2)
    | NOP -> Unit
    | CALL (s, ls) ->
        store_to_regs_params ls bd var_to_register var_in_stack;
        let first_element = match !num_params with
          | hd :: _ -> hd
          | [] -> 0
        in
        Call (s, first_element)
    | CALLCLO (s, ls) ->
        let r = Hashtbl.find var_to_register s in
        bd := !bd @ [LoadReg ("r12", Reg r)];
        store_to_regs_params ls bd var_to_register var_in_stack;
        let first_element = match !num_params with
          | hd :: _ -> hd
          | [] -> 0
        in
        
        CallClo (r, first_element)
    | IFEQ ((s,i_o_s) , asmt1, asmt2) ->
        parcours_if "eq" s i_o_s asmt1 asmt2 var_to_register var_in_stack list_params is_float
    | IFLE ((s,i_o_s) , asmt1, asmt2) ->
        parcours_if "le" s i_o_s asmt1 asmt2 var_to_register var_in_stack list_params is_float
    | IFGE ((s,i_o_s) , asmt1, asmt2) ->
        parcours_if "ge" s i_o_s asmt1 asmt2 var_to_register var_in_stack list_params is_float
    
(* get *)
    | _ -> Unit

  and parcours_if ifexpr s i_o_s asmt1 asmt2 var_to_register var_in_stack list_params is_float =
    let new_s = Hashtbl.find var_to_register s in
    let new_ios = parcours_id_or_im i_o_s var_to_register in
    let bd_asmt1 = !(parcours_asmt_if asmt1 var_to_register var_in_stack list_params is_float ) in
    let bd_asmt2 = !(parcours_asmt_if asmt2 var_to_register var_in_stack list_params is_float) in
    If (ifexpr, (new_s,new_ios),bd_asmt1,bd_asmt2)


  and parcours_asmt_if asmt var_to_register var_in_stack list_params is_float =
    let var_to_register_asmt = create_copy_hash var_to_register in
    let active = get_intervals_i asmt var_to_register_asmt list_params in
    let body_asmt = ref [] in
    store_load active body_asmt var_to_register_asmt var_in_stack list_params reg_available 0;
    let regt = parcours_asmt asmt body_asmt var_to_register_asmt var_in_stack list_params is_float  in
    load_if var_to_register var_to_register_asmt body_asmt var_in_stack;
    update_reg_available var_to_register reg_available 0;
    body_asmt


  and parcours_asml_list asml_list =
    match asml_list with
    | Main hd :: tl ->
        let var_to_register = Hashtbl.create 0 in
        let var_in_stack = Hashtbl.create 0 in
        let active = get_intervals_i hd var_to_register [] in
        reg_available := registers ;
        float_reg_available := register_float;
        let new_func : reg_function = {
          name = "main";
          body = !(parcours_asmt hd (ref []) var_to_register var_in_stack [] 0) @ [Exp (Call ("_min_caml_exit", 0))];
        } in
        new_body:= !new_body @ [Fun new_func];
        parcours_asml_list tl
    | LetFloat (label,fl) :: tl -> 
        new_body := !new_body @ [LetFloatReg (label, (string_of_float fl))];
        Hashtbl.add float label "";
        parcours_asml_list tl
    | LetLabel (fun_name, list_params, hd) :: tl ->
        let var_to_register = Hashtbl.create 0 in
        let var_in_stack = Hashtbl.create 0 in
        num_params := (List.length list_params) :: !num_params;
        reg_available := registers ;
        float_reg_available := register_float;
        vider_hash float_to_reg;
        print_hashtable float_to_reg;
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
          body = !(parcours_asmt hd body_func var_to_register var_in_stack list_params 0);
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