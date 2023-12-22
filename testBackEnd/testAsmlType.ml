type asml_expr =
  | Var of string
  | Int of int
  | Let of asml_expr * asml_expr
  | Fun of asm_function
  | Add of asml_expr * asml_expr
  | Sub of asml_expr * asml_expr
  | Mul of asml_expr * asml_expr
  | Assign of string * asml_expr
  | Tuple of (string * string)

and asm_function = {
  name : string;
  params : asml_expr list;
  body : asml_expr list;
}

let rec generate_asm_expr : asml_expr -> string list = function
  | Var _ | Int _ -> []
  | Let (expr1, expr2) ->

    let get_var_result = function
      | Tuple (inner_var, _) -> inner_var
      | Int n -> "#" ^ string_of_int n
      | _ -> failwith "Invalid expression in Let"
    in

    let ld_expr1 =
      match expr1 with
      | Tuple (inner_var1, inner_mem_pos1) ->
          if inner_mem_pos1 <> "" then
            ["LDR " ^ inner_var1 ^ ", " ^ inner_mem_pos1]
          else
            []
      | _ -> failwith "Invalid expression in Let - ld_expr_1"
    in

    let ld_expr2 =
      match expr2 with
      | Tuple (inner_var2, inner_mem_pos2) ->
          if inner_mem_pos2 <> "" then
            ["LDR " ^ inner_var2 ^ ", " ^ inner_mem_pos2]
          else
            []
      | _ -> []
    in

    ld_expr1 @ ld_expr2 @ ["MOV " ^ get_var_result expr1 ^ ", " ^ get_var_result expr2]

  | Fun { name; params; body } ->
    let rec generate_asm_fun_internal acc = function
      | [] -> acc @ ["!EPILOGUE BEGIN"; "ADD SP, FP, #0"; "LDR FP, [SP]"; "ADD SP, SP, #4"; "!EPILOGUE END"]
      | hd :: tl ->
        let asm_hd = generate_asm_expr hd in
        generate_asm_fun_internal (acc @ asm_hd) tl
    in
    let size = List.length (List.filter (function Let _ | Assign _ -> true | _ -> false) body) in
    generate_prologue size @ generate_asm_fun_internal [] body

  | _ -> failwith "Unsupported operation outside Assign"


and generate_prologue size =
  ["!PROLOGUE BEGIN"; "ADD SP, SP, #-4"; "STR FP, [SP]"; "ADD FP, SP, #0"; "ADD SP, SP, #-" ^ string_of_int (size * 4) ^ "\n!PROLOGUE END"]

let generate_asm (exprs: asml_expr list) : string list =
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
      [
        Fun {
          name = "_";
          params = [];
          body = [
            Let (Tuple("x", ""), Int 5);
            Let (Tuple("y", "[FP - 4]"), Int 10);
            Let (Tuple("z", "[FP - 4]"), Tuple("w", ""));
            Let (Tuple("a", "[FP - 4]"), Tuple("b", "[FP - 8]"));
            Let (Tuple("c", ""), Tuple("d", "[FP - 8]"));
          ];
        }
      ]
  in
  List.iter print_endline result_asm


