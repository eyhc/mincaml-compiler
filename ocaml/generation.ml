open RegAlloc

let header : string list ref = ref [".text"; ".global main"; ""]
let consts : string list ref = ref []
let floats : string list ref = ref []

let if_label_counter = ref 0
let num_label_counter = ref 0

let generate_if_label () =
  let label = "l" ^ string_of_int !if_label_counter in
  if_label_counter := !if_label_counter + 1;
  label

let generate_num_label () =
  let label = "const_" ^ string_of_int !num_label_counter in
  num_label_counter := !num_label_counter + 1;
  label

let rec count_lets_in_regt : regt -> int = function
  | Let (s, _) when List.mem s ["r0"; "r1"; "r2"; "r3"] -> 0
  | Let (_, _) -> 1
  | Exp _ | Store (_, _) | Load (_, _) -> 0

let rec count_lets_in_reg_function : reg_function -> int =
  fun { body; _ } -> List.fold_left (fun acc stmt -> acc + count_lets_in_regt stmt) 0 body
   
  and generate_asm_regt : regt -> string list = function
  | Let (s, expr) ->
    (match expr with
      | Int n ->
        if n <= 255 then
          [Printf.sprintf "\tmov %s, #%d" s n]
        else
          [Printf.sprintf "\tldr %s, =#%d" s n]
      | Reg reg -> [Printf.sprintf "\tmov %s, %s" s reg]
      | Add (s1, expr) ->
        (match expr with
        | Int n -> 
          if n <= 255 then
            [Printf.sprintf "\tadd %s, %s, #%d" s s1 n]
          else
            [Printf.sprintf "\tldr %s, =#%d" s n; Printf.sprintf "\tadd %s, %s, %s" s s1 s]
        | Reg reg -> [Printf.sprintf "\tadd %s, %s, %s" s s1 reg]
        | _ -> assert false)
      | Sub (s1, expr) ->
        (match expr with
        | Int n -> 
          if n <= 255 then
            [Printf.sprintf "\tsub %s, %s, #%d" s s1 n]
          else
            [Printf.sprintf "\tldr %s, =#%d" s n; Printf.sprintf "\tsub %s, %s, %s" s s1 s]
        | Reg reg -> [Printf.sprintf "\tsub %s, %s, %s" s s1 reg]
        | _ -> assert false)
      | If (cmp_type, (r1, Reg r2), true_branch, false_branch) ->
        let true_label = generate_if_label () in
        let end_label = generate_if_label () in
        Printf.sprintf "\tcmp %s, %s" r1 r2 ::
        Printf.sprintf "\tb%s %s" cmp_type true_label ::
        List.concat (List.map generate_asm_regt false_branch) @
        Printf.sprintf "\tb %s" end_label ::
        Printf.sprintf "\t%s:" true_label ::
        List.concat (List.map generate_asm_regt true_branch) @
        Printf.sprintf "\t%s:" end_label :: []
      | Call (func_name) -> [Printf.sprintf "\tbl %s" func_name; Printf.sprintf "\tmov %s, r0" s]
      | Neg s1 -> [Printf.sprintf "\tneg %s, %s" s s1]
      | Unit -> [Printf.sprintf "\tmov %s, #0" s]
      | _ -> assert false)
  | Exp exp ->
  (match exp with
    | If (cmp_type, (r1, Reg r2), true_branch, false_branch) ->
      let true_label = generate_if_label () in
      let end_label = generate_if_label () in
      Printf.sprintf "\tcmp %s, %s" r1 r2 ::
      Printf.sprintf "\tb%s %s" cmp_type true_label ::
      List.concat (List.map generate_asm_regt false_branch) @
      Printf.sprintf "\tb %s" end_label ::
      Printf.sprintf "\t%s:" true_label ::
      List.concat (List.map generate_asm_regt true_branch) @
      Printf.sprintf "\t%s:" end_label :: []
    | Int n ->
      if n <= 255 then
        [Printf.sprintf "\tmov r0, #%d" n]
      else
        [Printf.sprintf "\tldr r0, =#%d" n]      
    | Reg reg -> [Printf.sprintf "\tmov r0, %s" reg]
    | Call (func_name) -> [Printf.sprintf "\tbl %s" func_name;]
    | Unit -> []
    | _ -> assert false)
  | Store (Reg reg, mem) -> [Printf.sprintf "\tstr %s, %s" reg mem]
  | Load (s, Reg reg) -> [Printf.sprintf "\tldr %s, %s" reg s]
  | _ -> assert false

and generate_asm_fun_internal : reg_function -> string list = fun { name; body } ->
  let size = count_lets_in_reg_function { name; body } in
  [Printf.sprintf "%s:" name] @ generate_prologue size @ List.concat (List.map generate_asm_regt body) @ generate_epilogue

and generate_prologue size =
  if size * 4 <= 255 then
    ["\tpush {fp, lr}"; "\tadd fp, sp, #0"; "\tsub sp, sp, #" ^ string_of_int (size * 4)]
  else
    let label = generate_num_label () in
    consts := !consts @ [label ^ ": .word " ^ string_of_int (size * 4) ^ "\n"];
    ["\tpush {fp, lr}"; "\tadd fp, sp, #0"; "\tldr sp, " ^ label;
                "\tsub sp, fp, sp"]
  
and generate_epilogue =
  ["\tadd sp, fp, #0"; "\tpop {fp, lr}"; "\tbx lr\n"]

let generate_asm_reg (defs: letregdef list) : string list =
  match defs with
  | [] -> []
  | _ ->
    let rec generate_asm_internal acc = function
      | [] -> acc
      | hd :: tl ->
        let asm_hd = match hd with Fun f -> generate_asm_fun_internal f in
        generate_asm_internal (acc @ asm_hd) tl
    in
    let asm_code = generate_asm_internal [] defs in !header @ !consts @ !floats @ asm_code
