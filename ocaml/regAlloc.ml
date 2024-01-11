open Asml;;

(* type id_or_imm = Var of Id.t | Const of int

type expr =
| NOP
| VAL of id_or_imm
| LABEL of Id.l
| NEG of Id.t
| ADD of Id.t * id_or_imm
| SUB of Id.t * id_or_imm
| FNEG of Id.t
| FADD of Id.t * Id.t
| FSUB of Id.t * Id.t
| FMUL of Id.t * Id.t
| FDIV of Id.t * Id.t
| NEW of id_or_imm
| MEMGET of Id.t * id_or_imm
| MEMASSIGN of Id.t * id_or_imm * Id.t
| IFEQ of (Id.t*id_or_imm) * asmt * asmt
| IFLE of (Id.t*id_or_imm) * asmt * asmt
| IFFEQUAL of (Id.t*Id.t) * asmt * asmt
| IFFLE of (Id.t*Id.t) * asmt * asmt
| CALL of Id.l * Id.t list
| CALLCLO of Id.t * Id.t list
and asmt =
| LET of Id.t * expr * asmt (* let t = exp in asmt *)
| EXP of expr
and letdef =
| Main of asmt
| LetFloat of float
| LetLabel of Id.l * Id.t list * asmt
and asml = letdef list *)

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
  body : regt list ;
}
;;

let asml = [(Main (LET ("x", VAL (Const 1), (LET ("y", VAL (Const 2), (LET ("a", (VAL (Const 8)),
                                                                            (LET ("b", VAL (Const 14), (LET ("i", VAL (Const 14),
                                                                                                             EXP (CALL ("bonjour",["a";"z";"x";"b";"i"])))))))))))))];;
 
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

let num_registers = 9;;
 (* hashmap de toutes les variables sur la pile avec leurs adresses
exemple : "x" "FP - 4" : *)
let var_in_stack = Hashtbl.create 0;;
     (* parcours_asmt
hashtable binding variable to register,
a : r1 -> variable `a` is in register `r1` 
*)
let var_to_register = Hashtbl.create num_registers;;

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
      i_intervals_asmt asmt1;
      i_intervals_asmt asmt2;
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
 


let store_load intervals body =
   (* a la premiere ligne hashtable est vide, on y met les 9 variables *)
  if (Hashtbl.length var_to_register) = 0 then begin 
    let rec add_hashmap n inter =
      match inter with
      | [] -> ()
      | _ when n >= num_registers -> ()
      | var :: tail -> 
          Hashtbl.add var_to_register var ("r" ^ string_of_int (n + 4)); (* + 4  car commence a r4 *)
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

let store_to_regs_params lst bd =
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
  let rec parcours_asmt asmt bd =
    match asmt with
    | LET (var1, var2, exp) -> 
        let active = get_intervals_i asmt in
        store_load active bd;
        bd := !bd @ [Let ((Hashtbl.find var_to_register var1), (parcours_expr var2 bd))];
        parcours_asmt exp bd; 
    | EXP expr -> 
        let active = get_intervals_i asmt in
        store_load active bd;
        bd := !bd @ [Exp (parcours_expr expr bd)];
        bd
  and parcours_id_or_im id_or_im =
    match id_or_im with
    | Const n -> Int n
    | Var r ->  Reg (Hashtbl.find var_to_register r)
                  
  and parcours_expr expr bd =
    match expr with
    | VAL v -> 
        parcours_id_or_im v
    | NEG s ->
        Neg (Hashtbl.find var_to_register s)
    | ADD (s, id_or_im) -> 
        Add (Hashtbl.find var_to_register s, parcours_id_or_im id_or_im)
    | SUB (s, id_or_im) -> 
        Sub (Hashtbl.find var_to_register s, parcours_id_or_im id_or_im)
    | NOP -> Unit
    | CALL (s, ls) ->
        let active = premiers_elements ls in
        store_load active bd; 
        store_to_regs_params ls bd;
        Call (s) 
    | IFEQ ((s,i_o_s) , asmt1, asmt2) -> 
        let i = parcours_id_or_im i_o_s in
        let a1 = !(parcours_asmt asmt1 (ref [])) in
        let a2 = !(parcours_asmt asmt2 (ref [])) in
        If ("eq",(s, i), a1, a2)
    | IFLE ((s,i_o_s) , asmt1, asmt2) -> 
        let i = parcours_id_or_im i_o_s in
        let a1 = !(parcours_asmt asmt1 (ref [])) in
        let a2 = !(parcours_asmt asmt2 (ref [])) in
        If ("le",(s, i), a1, a2)
    | IFGE ((s,i_o_s) , asmt1, asmt2) -> 
        let i = parcours_id_or_im i_o_s in
        let a1 = !(parcours_asmt asmt1 (ref [])) in
        let a2 = !(parcours_asmt asmt2 (ref [])) in
        If ("ge",(s, i), a1, a2)
    | _ -> Unit

  and parcours_asml_list asml_list =
    match asml_list with
    | Main hd :: tl -> 
        let new_func : reg_function = {
          name = "main"; 
          body = !(parcours_asmt hd (ref []));
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
  | If (_,_,_,_) -> Printf.printf  "if()"
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
      Printf.printf ")) ";

;;

let rec print_reg_function reg_function =
  match reg_function with 
  | Fun f :: tl ->  Printf.printf "Function name: %s\n" f.name;
      Printf.printf "Body:\n";
      List.iter (fun regt -> print_regt regt) f.body;
      print_reg_function tl
  | [] -> ()
;;