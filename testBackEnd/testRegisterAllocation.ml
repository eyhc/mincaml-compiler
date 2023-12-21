open Hashtbl;;

let create_and_initialize_register_hashtable () =
  let my_hashtable = Hashtbl.create 9 in
  for i = 4 to 12 do
    Hashtbl.add my_hashtable ("r" ^ string_of_int i) 1
  done;
  my_hashtable

(* let create_active_variables_hashtable () =
  let my_hashtable = Hashtbl.create 9 in
  my_hashtable *)

let create_empty_hashtbl size =
  Hashtbl.create size
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

(* Adds in hashtable key and value *)
let add_variable_value hashtable key value =
  Hashtbl.add hashtable key value
;;

(* Returns true if variable `var` is in stack *)
let key_in_hash hash key =
  try 
    Hashtbl.find hash key;
    true
  with e -> (false)
;;

(* 
  If the variable `var` is not in the stakc,
  calculates it's position and adds it into the stack
*)
let add_hash hash key =
  if not (key_in_hash hash key) then 
    Hashtbl.add hash key ((Hashtbl.length hash + 1) * 4)
;;

(* Useless function *)
let find_var_pos_stack stack var = 
  Hashtbl.find stack var
;;
  
let process_variable_list register_hashtable active_variables_hashtable stack variables =
  List.iter (fun variable ->
      match find_free_register register_hashtable with
      | Some register_key ->
          add_variable_value active_variables_hashtable variable register_key;
          Printf.printf "Added variable '%s' with register: %s\n" variable register_key
      | None ->
          add_hash stack variable;
          Printf.printf "The variable '%s' is on the stack, it's position is FP - %d.\n" variable (find_var_pos_stack stack variable);
    ) variables

let () =
  let register_hashtable = create_and_initialize_register_hashtable () in
  let active_variables_hashtable = create_empty_hashtbl 9 in
  let stack = create_empty_hashtbl 0 in

  let variables = ["a"; "b"; "c" ; "d"; "e"; "f"; "j"; "h"; "i"; "g"; "g"; "k"] in

  process_variable_list register_hashtable active_variables_hashtable stack variables;

  Printf.printf "\nRegister Hashtable:\n";
  Hashtbl.iter (fun key value -> Printf.printf "%s: %d\n" key value) register_hashtable;

  Printf.printf "\nActive Variables Hashtable:\n";
  Hashtbl.iter (fun key value -> Printf.printf "%s: %s\n" key value) active_variables_hashtable
