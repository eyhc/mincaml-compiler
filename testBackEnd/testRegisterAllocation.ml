open Hashtbl;;

let create_and_initialize_register_hashtable () =
  let my_hashtable = Hashtbl.create 9 in
  for i = 4 to 12 do
    Hashtbl.add my_hashtable ("r" ^ string_of_int i) 1
  done;
  my_hashtable

let create_active_variables_hashtable () =
  let my_hashtable = Hashtbl.create 9 in
  my_hashtable

let find_free_register hashtable =
  let found_key = ref None in
  Hashtbl.iter (fun key value ->
    if value = 1 && !found_key = None then begin
      found_key := Some key;
      Hashtbl.replace hashtable key 0
    end
  ) hashtable;
  !found_key

let add_variable_value hashtable key value =
  Hashtbl.add hashtable key value

let process_variable_list register_hashtable active_variables_hashtable variables =
  List.iter (fun variable ->
    match find_free_register register_hashtable with
    | Some register_key ->
      add_variable_value active_variables_hashtable variable register_key;
      Printf.printf "Added variable '%s' with register: %s\n" variable register_key
    | None ->
      Printf.printf "No register with value 1 found for variable '%s'.\n" variable
  ) variables

let () =
  let register_hashtable = create_and_initialize_register_hashtable () in
  let active_variables_hashtable = create_active_variables_hashtable () in

  let variables = ["a"; "b"; "c" ; "d"; "e"; "f"; "j"; "h"; "i"; "g"] in

  process_variable_list register_hashtable active_variables_hashtable variables;

  Printf.printf "\nRegister Hashtable:\n";
  Hashtbl.iter (fun key value -> Printf.printf "%s: %d\n" key value) register_hashtable;

  Printf.printf "\nActive Variables Hashtable:\n";
  Hashtbl.iter (fun key value -> Printf.printf "%s: %s\n" key value) active_variables_hashtable
