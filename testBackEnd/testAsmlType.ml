type asml =
  | Code of asml_expr list

and asml_expr =
  | Var of string
  | Int of int
  | Let of string * asml_expr
  | Fun of asm_function
  | Add of asml_expr * asml_expr
  | Sub of asml_expr * asml_expr
  | Mul of asml_expr * asml_expr
  | Assign of string * asml_expr
  | Tuple of string * string * asml_expr

and asm_function = {
  name : string;
  params : asml_expr list;
  body : asml_expr list;
}

let rec generate_asm_expr : asml_expr -> string list = function
  | Var _ | Int _ -> []
  | Let (var, expr) ->
    (match expr with
     | Int n ->
       let asm_expr = generate_asm_expr expr in
       asm_expr @ ["MOV " ^ var ^ ", " ^ string_of_int n]
     | Tuple (reg, mem_pos, inner_expr) ->
       let int_value =
         match inner_expr with
         | Int n -> n
         | _ -> failwith "Expected an integer value in the Let expression"
       in
       let asm_expr = generate_asm_expr inner_expr in
       asm_expr @ ["LD " ^ reg ^ ", " ^ mem_pos; "MOV " ^ reg ^ ", " ^ string_of_int int_value]
     | _ -> failwith "Invalid expression in Let")  
      
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
      | _ -> failwith "Unsupported operation outside Assign"
    in
    let asm_value = generate_assign_value value in
    asm_value
  | Fun { name; params; body } ->
    let rec generate_asm_fun_internal acc = function
      | [] -> acc @ ["!EPILOGUE BEGIN"; "ADD SP, FP, 0"; "LDR FP, [SP]"; "ADD SP, SP, 4"; "!EPILOGUE END"]
      | hd :: tl ->
        let asm_hd = generate_asm_expr hd in
        generate_asm_fun_internal (acc @ asm_hd) tl
    in
    let size = List.length (List.filter (function Let _ | Assign _ -> true | _ -> false) body) in
    generate_prologue size @ generate_asm_fun_internal [] body
  | _ -> failwith "Unsupported operation outside Assign"

and generate_prologue size =
  ["!PROLOGUE BEGIN"; "ADD SP, SP, -4"; "STR FP, [SP]"; "ADD FP, SP, 0"; "ADD SP, SP, -" ^ string_of_int (size * 4) ^ "\n!PROLOGUE END"]

let generate_asm_fun : asm_function -> string list =
  fun { name; params; body } ->
    generate_prologue (List.length (List.filter (function Let _ | Assign _ -> true | _ -> false) body))
    @ generate_asm_expr (Fun { name; params; body })

let generate_asm : asml -> string list = function
  | Code exprs ->
    let rec generate_asm_internal acc = function
      | [] -> acc
      | hd :: tl ->
        let asm_hd = generate_asm_expr hd in
        generate_asm_internal (acc @ asm_hd) tl
    in
    generate_asm_internal [] exprs

let () =
  let result_asm =
    generate_asm
      (Code
          [ Fun {
              name = "_";
              params = [];
              body = [Let ("x", Tuple("r0", "[FP - 4]", Int 5)); Let ("y", Int 2); Let ("a", Int 8);
                      Let ("b", Int 14); Assign ("a", Sub (Var "x", Var "y"));
                      Assign ("z", Add (Var "x", Var "y")); 
                      Assign ("b", Mul (Var "a", Var "z"))];
            };
            Fun {
              name = "_f1";
              params = [];
              body = [Let ("m", Int 5); Let ("n", Int 3); Assign ("p", Add (Var "m", Int 4))];
            };
            Fun {
              name = "_f2";
              params = [];
              body = [Let ("q", Int 10); Let ("r", Int 4); Assign ("s", Sub (Var "q", Int 3));
                      Assign ("t", Mul (Var "q", Int 5))];
            }
          ])
  in
  List.iter print_endline result_asm
