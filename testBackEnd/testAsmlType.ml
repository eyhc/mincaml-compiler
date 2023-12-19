type asml =
  | Fun of asm_function * asml list

and asml_expr =
  | Var of string
  | Int of int
  | Let of string * asml_expr
  | Add of asml_expr * asml_expr
  | Sub of asml_expr * asml_expr
  | Mul of asml_expr * asml_expr
  | Assign of string * asml_expr
  | End of unit

and asm_function = {
  name : string;
  params : asml_expr list;
  body : asml_expr list;
}

let rec generate_asm_expr : asml_expr -> string list = function
  | Var _ | Int _ -> []
  | Let (var, expr) ->
    let rec extract_int_value = function
      | Int n -> n
      | _ -> failwith "Expected an integer value in the Let expression"
    in
    let int_value = extract_int_value expr in
    let asm_expr = generate_asm_expr expr in
    asm_expr @ ["MOV " ^ var ^ ", " ^ string_of_int int_value]
  | Assign (var, value) ->
    let rec generate_assign_value = function
      | Var _ | Int _ -> []
      | Add (Var src1, Var src2) ->
        ["ADD " ^ var ^ ", " ^ src1 ^ ", " ^ src2]
      | Add (Var src1, Int n) ->
        ["ADD " ^ var ^ ", " ^ src1 ^ ", " ^ string_of_int n]
      | Sub (Var src1, Var src2) ->
        ["SUB " ^ var ^ ", " ^ src1 ^ ", " ^ src2]
      | Sub (Var src1, Int n) ->
        ["SUB " ^ var ^ ", " ^ src1 ^ ", " ^ string_of_int n]
      | Mul (Var src1, Var src2) ->
        ["MUL " ^ var ^ ", " ^ src1 ^ ", " ^ src2]
      | Mul (Var src1, Int n) ->
        ["MUL " ^ var ^ ", " ^ src1 ^ ", " ^ string_of_int n]
    in
    let asm_value = generate_assign_value value in
    asm_value
  | End () -> ["!EPILOGUE BEGIN"; "ADD SP, FP, 0"; "LDR FP, [SP]"; "ADD SP, SP, 4"; "!EPILOGUE END"]
  | _ -> failwith "Unsupported operation outside Assign"

let generate_prologue size =
  ["!PROLOGUE BEGIN"; "ADD SP, SP, -4"; "STR FP, [SP]"; "ADD FP, SP, 0"; "ADD SP, SP, -" ^ string_of_int (size * 4) ^ "\n!PROLOGUE END"]

let generate_asm_fun : asm_function -> string list =
  fun { name; params; body } ->
    let rec generate_asm_fun_internal acc = function
      | [] -> acc
      | hd :: tl ->
        let asm_hd = generate_asm_expr hd in
        generate_asm_fun_internal (acc @ asm_hd) tl
    in
    let size = List.length (List.filter (function Let _ | Assign _ -> true | _ -> false) body) in
    generate_prologue size @ generate_asm_fun_internal [] body

let rec generate_asm : asml -> string list = function
  | Fun (main, functions) ->
    let main_asm = generate_asm_fun main in
    let functions_asm = List.flatten (List.map generate_asm functions) in
    main_asm @ functions_asm

let () =
  let result_asm =
    generate_asm
      (Fun
         ({ name = "_"; params = []; body = [Let ("x", Int 1); Let ("y", Int 2); Let ("a", Int 8);
         Let ("b", Int 14); Assign ("a", Sub (Var "x", Var "y")); Assign ("z", Add (Var "x", Var "y"));
          Assign ("b", Mul(Var "a", Var "z")); End()]},
          [Fun ({name = "_f"; params = []; body = [Let ("h", Int 5); 
          Let ("v", Int 9); Assign ("s", Add (Var "h", Var "v")); End()]}, [])]))
  in
  List.iter print_endline result_asm
