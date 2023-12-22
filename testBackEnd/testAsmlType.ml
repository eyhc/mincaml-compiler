type asml_expr =
  | Var of string
  | Int of int
  | Let of asml_expr * asml_expr
  | Fun of asm_function
  | Add of asml_expr * asml_expr
  | Sub of asml_expr * asml_expr
  | Mul of asml_expr * asml_expr
  | Assign of asml_expr * asml_expr
  | Tuple of (string * string * int)

and asm_function = {
  name : string;
  params : asml_expr list;
  body : asml_expr list;
}

let rec generate_asm_expr : asml_expr -> string list = function
  | Var _ | Int _ -> []
  | Let (expr1, expr2) ->

    let get_var_result = function
      | Tuple (inner_var, _, _) -> inner_var
      | Int n -> "#" ^ string_of_int n
      | _ -> failwith "Invalid expression in Let"
    in

    let get_mem_pos_result = function
      | Tuple (_, mem_pos, _) -> mem_pos
      | _ -> failwith "Invalid expression in Let"
    in

    let ld_expr2 =
      match expr2 with
      | Tuple (inner_var2, inner_mem_pos2, in_register) ->
          if in_register == 0 then
            ["LDR " ^ inner_var2 ^ ", " ^ inner_mem_pos2]
          else
            []
      | _ -> []
    in

    ld_expr2 @ ["MOV " ^ get_var_result expr1 ^ ", " ^ get_var_result expr2;
     "STR " ^ get_var_result expr1 ^ ", " ^ get_mem_pos_result expr1]
  
  | Assign (expr1, expr2) ->

    let get_var_result = function
      | Tuple (inner_var, _, _) -> inner_var
      | Int n -> "#" ^ string_of_int n
      | _ -> failwith "Invalid expression in Let"
    in

    let get_mem_pos_result = function
      | Tuple (_, mem_pos, _) -> mem_pos
      | _ -> failwith "Invalid expression in Let"
    in

    let expr2_ins =
      match expr2 with
      | Add (inner_expr1, inner_expr2) ->
        let ld_inner_expr1 =
          match inner_expr1 with
          | Tuple (inner_var1, inner_mem_pos1, in_register) ->
            if in_register == 0 then
              ["LDR " ^ inner_var1 ^ ", " ^ inner_mem_pos1]
            else
              []
          | _ -> []
        in

        let ld_inner_expr2 =
          match inner_expr2 with
          | Tuple (inner_var1, inner_mem_pos1, in_register) ->
            if in_register == 0 then
              ["LDR " ^ inner_var1 ^ ", " ^ inner_mem_pos1]
            else
              []
          | _ -> []
        in

        let add_expr =
          match (get_var_result inner_expr1, get_var_result inner_expr2) with
          | (s1, s2) when String.length s1 > 0 && s1.[0] = '#' && String.length s2 > 0 && s2.[0] = '#' ->
            ["MOV " ^ get_var_result expr1 ^ ", " ^ s1;
              "ADD " ^ get_var_result expr1 ^ ", " ^ get_var_result expr1 ^ ", " ^ s2]
          | (s1, _) when String.length s1 > 0 && s1.[0] = '#' ->
            ["ADD " ^ get_var_result expr1 ^ ", " ^ get_var_result inner_expr2 ^ ", " ^ get_var_result inner_expr1]
          | (_, _) ->
            ["ADD " ^ get_var_result expr1 ^ ", " ^ get_var_result inner_expr1 ^ ", " ^ get_var_result inner_expr2]
        in
      
        ld_inner_expr1 @ ld_inner_expr2 @ add_expr @
        ["STR " ^ get_var_result expr1 ^ ", " ^ get_mem_pos_result expr1]
    
      | Sub (inner_expr1, inner_expr2) ->
        let ld_inner_expr1 =
          match inner_expr1 with
          | Tuple (inner_var1, inner_mem_pos1, in_register) ->
            if in_register == 0 then
              ["LDR " ^ inner_var1 ^ ", " ^ inner_mem_pos1]
            else
              []
          | _ -> []
        in

        let ld_inner_expr2 =
          match inner_expr2 with
          | Tuple (inner_var1, inner_mem_pos1, in_register) ->
            if in_register == 0 then
              ["LDR " ^ inner_var1 ^ ", " ^ inner_mem_pos1]
            else
              []
          | _ -> []
        in
      
        let sub_expr =
          match (get_var_result inner_expr1, get_var_result inner_expr2) with
          | (s1, s2) when String.length s1 > 0 && s1.[0] = '#' && String.length s2 > 0 && s2.[0] = '#' ->
            ["MOV " ^ get_var_result expr1 ^ ", " ^ s1;
              "SUB " ^ get_var_result expr1 ^ ", " ^ get_var_result expr1 ^ ", " ^ s2]
          | (s1, _) when String.length s1 > 0 && s1.[0] = '#' ->
            ["RSB " ^ get_var_result expr1 ^ ", " ^ get_var_result inner_expr1 ^ ", " ^ get_var_result inner_expr2]
          | (_, _) ->
            ["SUB " ^ get_var_result expr1 ^ ", " ^ get_var_result inner_expr1 ^ ", " ^ get_var_result inner_expr2]
        in
      
        ld_inner_expr1 @ ld_inner_expr2 @ sub_expr @
        ["STR " ^ get_var_result expr1 ^ ", " ^ get_mem_pos_result expr1]

      | Mul (inner_expr1, inner_expr2) ->
        let ld_inner_expr1 =
          match inner_expr1 with
          | Tuple (inner_var1, inner_mem_pos1, in_register) ->
            if in_register == 0 then
              ["LDR " ^ inner_var1 ^ ", " ^ inner_mem_pos1]
            else
              []
          | _ -> []
        in

        let ld_inner_expr2 =
          match inner_expr2 with
          | Tuple (inner_var1, inner_mem_pos1, in_register) ->
            if in_register == 0 then
              ["LDR " ^ inner_var1 ^ ", " ^ inner_mem_pos1]
            else
              []
          | _ -> []
        in
      
        let mul_expr =
          match (get_var_result inner_expr1, get_var_result inner_expr2) with
          | (s1, s2) when String.length s1 > 0 && s1.[0] = '#' && String.length s2 > 0 && s2.[0] = '#' ->
            ["MOV " ^ get_var_result expr1 ^ ", " ^ s1;
              "MUL " ^ get_var_result expr1 ^ ", " ^ get_var_result expr1 ^ ", " ^ s2]
          | (s1, _) when String.length s1 > 0 && s1.[0] = '#' ->
            ["MUL " ^ get_var_result expr1 ^ ", " ^ get_var_result inner_expr2 ^ ", " ^ get_var_result inner_expr1]
          | (_, _) ->
            ["MUL " ^ get_var_result expr1 ^ ", " ^ get_var_result inner_expr1 ^ ", " ^ get_var_result inner_expr2]
        in
      
        ld_inner_expr1 @ ld_inner_expr2 @ mul_expr @
        ["STR " ^ get_var_result expr1 ^ ", " ^ get_mem_pos_result expr1]

    | _ -> failwith "Unknown assign instruction"

    in
    
    expr2_ins

  | Fun { name; params; body } ->
    let rec generate_asm_fun_internal acc = function
      | [] -> acc @ ["!EPILOGUE BEGIN"; "ADD SP, FP, #0"; "LDR FP, [SP]"; "ADD SP, SP, #4"; "!EPILOGUE END"]
      | hd :: tl ->
        let asm_hd = generate_asm_expr hd in
        generate_asm_fun_internal (acc @ asm_hd) tl
    in
    let size = List.length (List.filter (function Let _ | Assign _ -> true | _ -> false) body) in
    generate_prologue size @ generate_asm_fun_internal [] body

  | _ -> failwith "Unsupported operation"

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

            Let (Tuple("r0", "[FP - 4]", 1), Int 10);
            Let (Tuple("r0", "[FP - 4]", 1), Tuple("r1", "[FP - 8]", 1));
            Let (Tuple("r0", "[FP - 4]", 1), Tuple("r1", "[FP - 8]", 0));

            Assign(Tuple("r0", "[FP - 4]", 1), Add (Int 10, Int 20));
            Assign(Tuple("r0", "[FP - 4]", 1), Add (Tuple("r1", "[FP - 8]", 1), Int 20));
            Assign(Tuple("r0", "[FP - 4]", 1), Add (Tuple("r1", "[FP - 8]", 0), Int 20));
            Assign(Tuple("r0", "[FP - 4]", 1), Add (Int 10, Tuple("r1", "[FP - 8]", 1)));
            Assign(Tuple("r0", "[FP - 4]", 1), Add (Int 10, Tuple("r1", "[FP - 8]", 0)));

            Assign(Tuple("r0", "[FP - 4]", 1), Add (Tuple("r1", "[FP - 8]", 0), Tuple("r2", "[FP - 12]", 0)));
            Assign(Tuple("r0", "[FP - 4]", 1), Add (Tuple("r1", "[FP - 8]", 1), Tuple("r2", "[FP - 12]", 0)));
            Assign(Tuple("r0", "[FP - 4]", 1), Add (Tuple("r1", "[FP - 8]", 0), Tuple("r2", "[FP - 12]", 1)));
            Assign(Tuple("r0", "[FP - 4]", 1), Add (Tuple("r1", "[FP - 8]", 1), Tuple("r2", "[FP - 12]", 1)));

            
            Assign(Tuple("r0", "[FP - 4]", 1), Sub (Int 10, Int 20));
            Assign(Tuple("r0", "[FP - 4]", 1), Sub (Tuple("r1", "[FP - 8]", 1), Int 20));
            Assign(Tuple("r0", "[FP - 4]", 1), Sub (Tuple("r1", "[FP - 8]", 0), Int 20));
            Assign(Tuple("r0", "[FP - 4]", 1), Sub (Int 10, Tuple("r1", "[FP - 8]", 1)));
            Assign(Tuple("r0", "[FP - 4]", 1), Sub (Int 10, Tuple("r1", "[FP - 8]", 0)));

            Assign(Tuple("r0", "[FP - 4]", 1), Sub (Tuple("r1", "[FP - 8]", 0), Tuple("r2", "[FP - 12]", 0)));
            Assign(Tuple("r0", "[FP - 4]", 1), Sub (Tuple("r1", "[FP - 8]", 1), Tuple("r2", "[FP - 12]", 0)));
            Assign(Tuple("r0", "[FP - 4]", 1), Sub (Tuple("r1", "[FP - 8]", 0), Tuple("r2", "[FP - 12]", 1)));
            Assign(Tuple("r0", "[FP - 4]", 1), Sub (Tuple("r1", "[FP - 8]", 1), Tuple("r2", "[FP - 12]", 1)));


            Assign(Tuple("r0", "[FP - 4]", 1), Mul (Int 10, Int 20));
            Assign(Tuple("r0", "[FP - 4]", 1), Mul (Tuple("r1", "[FP - 8]", 1), Int 20));
            Assign(Tuple("r0", "[FP - 4]", 1), Mul (Tuple("r1", "[FP - 8]", 0), Int 20));
            Assign(Tuple("r0", "[FP - 4]", 1), Mul (Int 10, Tuple("r1", "[FP - 8]", 1)));
            Assign(Tuple("r0", "[FP - 4]", 1), Mul (Int 10, Tuple("r1", "[FP - 8]", 0)));

            Assign(Tuple("r0", "[FP - 4]", 1), Mul (Tuple("r1", "[FP - 8]", 0), Tuple("r2", "[FP - 12]", 0)));
            Assign(Tuple("r0", "[FP - 4]", 1), Mul (Tuple("r1", "[FP - 8]", 1), Tuple("r2", "[FP - 12]", 0)));
            Assign(Tuple("r0", "[FP - 4]", 1), Mul (Tuple("r1", "[FP - 8]", 0), Tuple("r2", "[FP - 12]", 1)));
            Assign(Tuple("r0", "[FP - 4]", 1), Mul (Tuple("r1", "[FP - 8]", 1), Tuple("r2", "[FP - 12]", 1)));

          ];
        }
      ]
  in
  List.iter print_endline result_asm


