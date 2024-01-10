type reg_expr =
  | Int of int 
  | Neg of string
  | Add of string * reg_expr
  | Sub of string * reg_expr
  | Call of string * string list
  | Reg of string
  | Unit

and regt = 
  | Let of string * reg_expr 
  | Exp of reg_expr
  | Store of reg_expr * string 
  | Load of string * reg_expr 

and letregdef = 
  | Fun of reg_function

and reg_function = {
  name : string;
  params : reg_expr list;
  body : regt list;
}

let rec count_lets_in_regt : regt -> int = function
  | Let (_, _) -> 1
  | Exp _ | Store (_, _) | Load (_, _) -> 0

let rec count_lets_in_reg_function : reg_function -> int =
  fun { body; _ } -> List.fold_left (fun acc stmt -> acc + count_lets_in_regt stmt) 0 body

let rec generate_asm_regt : regt -> string list = function
  | Let (s, expr) ->
    (match expr with
     | Int n -> [Printf.sprintf "MOV %s, #%d" s n]
     | Reg reg -> [Printf.sprintf "MOV %s, %s" s reg]
     | Add (s1, expr) ->
       (match expr with
        | Int n -> [Printf.sprintf "ADD %s, %s, #%d" s s1 n]
        | Reg reg -> [Printf.sprintf "ADD %s, %s, %s" s s1 reg]
        | _ -> ["Error"])
     | Sub (s1, expr) ->
       (match expr with
        | Int n -> [Printf.sprintf "SUB %s, %s, #%d" s s1 n]
        | Reg reg -> [Printf.sprintf "SUB %s, %s, %s" s s1 reg]
        | _ -> ["Error"])
     | _ -> ["Error"])
  | Exp _ -> [] (* Do nothing for Exp *)
  | Store (Reg reg, mem) -> [Printf.sprintf "STR %s, %s" reg mem]
  | Load (s, Reg reg) -> [Printf.sprintf "LDR %s, %s" reg s]

and generate_asm_fun_internal : string list -> regt list -> string list = fun acc body ->
  match body with
  | [] -> acc @ ["ADD SP, FP, #0"; "LDR FP, [SP]"; "ADD SP, SP, #4"]
  | hd :: tl -> generate_asm_fun_internal (acc @ (generate_asm_regt hd)) tl

and generate_asm_reg_function : reg_function -> string list = fun { name; params; body } ->
  let size = count_lets_in_reg_function { name; params; body } in
  generate_prologue size @ generate_asm_fun_internal [] body

and generate_prologue size =
  ["ADD SP, SP, #-4"; "STR FP, [SP]"; "ADD FP, SP, #0"; "ADD SP, SP, #-" ^ string_of_int (size * 4)]

let generate_asm_reg (defs: letregdef list) : string list =
  match List.filter (function Fun { name; _ } when name = "_" -> true | _ -> false) defs with
  | [] -> failwith "No matching function with name '_'"
  | [funDef] ->
    let rec generate_asm_internal acc = function
      | [] -> acc
      | hd :: tl -> 
        let asm_hd = match hd with Fun f -> generate_asm_reg_function f in
        generate_asm_internal (acc @ asm_hd) tl
    in
    generate_asm_internal [] [funDef]
  | _ :: _ :: _ -> failwith "Multiple functions with name '_' found"

let () =
  let result_asm_reg =
    generate_asm_reg
      [Fun
        { name = "_"; params = [];
          body =
            [Let ("r4", Int 1); Store (Reg "r4", "[FP - 4]"); Let ("r5", Int 2);
             Store (Reg "r5", "[FP - 8]"); Load ("[FP - 4]", Reg "r5");
             Let ("r6", Int 8); Let ("r4", Int 14); Store (Reg "r4", "[FP - 12]");
             Load ("[FP - 8]", Reg "r4"); Let ("r6", Sub ("r5", Reg "r4"));
             Store (Reg "r6", "[FP - 16]"); Let ("r6", Add ("r5", Reg "r4"));
             Store (Reg "r5", "[FP - 4]"); Load ("[FP - 16]", Reg "r5");
             Store (Reg "r4", "[FP - 8]"); Load ("[FP - 12]", Reg "r4");
             Let ("r4", Sub ("r5", Reg "r6")); Store (Reg "r4", "[FP - 12]");
             Let ("r4", Add ("r5", Reg "r6")); Store (Reg "r4", "[FP - 20]");
             Store (Reg "r6", "[FP - 24]"); Store (Reg "r5", "[FP - 16]")]}]
  in
  let output_file_reg = "output.asm" in
  let oc_reg = open_out output_file_reg in
  List.iter (fun instruction -> output_string oc_reg (instruction ^ "\n")) result_asm_reg;
  close_out oc_reg;
  print_endline ("Results written to " ^ output_file_reg)
