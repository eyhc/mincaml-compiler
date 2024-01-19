open RegAlloc

let header : string list ref = ref [".text"; ".global _start"; ""]
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
| Let (s, _) when List.mem s ["r0"; "r1"; "r2"; "r3"; "sp"] -> 0
| Let (s, If (_, _, rts1, rts2)) ->
  count_lets_in_regt_list rts1 + count_lets_in_regt_list rts2
| Exp (If (_, _, rts1, rts2)) ->
  count_lets_in_regt_list rts1 + count_lets_in_regt_list rts2
| Let (s, _) -> 1
| _ -> 0

and count_lets_in_regt_list : regt list -> int = fun rts ->
  List.fold_left (fun acc rt -> acc + count_lets_in_regt rt) 0 rts

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
            [Printf.sprintf "\tldr r12, =#%d" n; Printf.sprintf "\tadd %s, %s, r12" s s1]
        | Reg reg -> [Printf.sprintf "\tadd %s, %s, %s" s s1 reg]
        | _ -> assert false)
      | Sub (s1, expr) ->
        (match expr with
        | Int n -> 
          if n <= 255 then
            [Printf.sprintf "\tsub %s, %s, #%d" s s1 n]
          else
            [Printf.sprintf "\tldr r12, =#%d" n; Printf.sprintf "\tsub %s, %s, r12" s s1]
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
      
      | If (cmp_type, (r1, Int n), true_branch, false_branch) ->
        let true_label = generate_if_label () in
        let end_label = generate_if_label () in
        let cmp_instruction =
          if n <= 255 then
            Printf.sprintf "\tcmp %s, #%d" r1 n
          else
            Printf.sprintf "\tldr r12, =%d\n\tcmp %s, r12" n r1
        in
        [cmp_instruction;
        Printf.sprintf "\tb%s %s" cmp_type true_label;
        Printf.sprintf "\tb %s" end_label;
        Printf.sprintf "\t%s:" true_label] @
        List.concat (List.map generate_asm_regt true_branch) @
        Printf.sprintf "\t%s:" end_label ::
        List.concat (List.map generate_asm_regt false_branch) 
      | Call (func_name, nb_params) ->
        let temp_loads = ref [] in
        for i = 0 to min 3 (nb_params - 1) do
          let param_reg = "r" ^ string_of_int i in
          let param_offset = (i + 1) * 4 in
          let param_address = "-" ^ string_of_int param_offset in
          temp_loads := !temp_loads @ [Printf.sprintf "\tldr %s, [fp, #%s]" param_reg param_address];
        done;
    
        [ Printf.sprintf "\tpush {r0-r10}";
          Printf.sprintf "\tbl %s" func_name;
          Printf.sprintf "\tmov r12, r0";
          Printf.sprintf "\tpop {r0-r10}";
          Printf.sprintf "\tmov r0, r12";
          Printf.sprintf "\tmov %s, r0" s
        ] @ !temp_loads;      
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
    
    | If (cmp_type, (r1, Int n), true_branch, false_branch) ->
      let true_label = generate_if_label () in
      let end_label = generate_if_label () in
      let cmp_instruction =
        if n <= 255 then
          Printf.sprintf "\tcmp %s, #%d" r1 n
        else
          Printf.sprintf "\tldr r12, =%d\n\tcmp %s, r12" n r1
      in
      [cmp_instruction;
      Printf.sprintf "\tb%s %s" cmp_type true_label;
      Printf.sprintf "\tb %s" end_label;
      Printf.sprintf "\t%s:" true_label] @
      List.concat (List.map generate_asm_regt true_branch) @
      Printf.sprintf "\t%s:" end_label ::
      List.concat (List.map generate_asm_regt false_branch)  
    | Int n ->
      if n <= 255 then
        [Printf.sprintf "\tmov r0, #%d" n]
      else
        [Printf.sprintf "\tldr r0, =#%d" n]      
    | Reg reg -> [Printf.sprintf "\tmov r0, %s" reg]
    | Add (s1, expr) ->
      (match expr with
      | Int n -> 
        if n <= 255 then
          [Printf.sprintf "\tadd r0, %s, #%d" s1 n]
        else
          [Printf.sprintf "\tldr r0, =#%d" n; Printf.sprintf "\tadd r0, %s, r0" s1]
      | Reg reg -> [Printf.sprintf "\tadd r0, %s, %s" s1 reg]
      | _ -> assert false)
    | Sub (s1, expr) ->
      (match expr with
      | Int n -> 
        if n <= 255 then
          [Printf.sprintf "\tsub r0, %s, #%d" s1 n]
        else
          [Printf.sprintf "\tldr r0, =#%d" n; Printf.sprintf "\tsub r0, %s, r0" s1]
      | Reg reg -> [Printf.sprintf "\tsub r0, %s, %s" s1 reg]
      | _ -> assert false)
    | Call (func_name, nb_params) ->
        let temp_loads = ref [] in
        for i = 0 to min 3 (nb_params - 1) do
          let param_reg = "r" ^ string_of_int i in
          let param_offset = (i + 1) * 4 in
          let param_address = "-" ^ string_of_int param_offset in
          temp_loads := !temp_loads @ [Printf.sprintf "\tldr %s, [fp, #%s]" param_reg param_address];
        done;

        [Printf.sprintf "\tpush {r0-r10}";
        Printf.sprintf "\tbl %s" func_name;
        Printf.sprintf "\tpop {r0-r10}"]
        @ !temp_loads;
    | Neg s1 -> [Printf.sprintf "\tneg r0, %s" s1]
    | Unit -> []
    | _ -> assert false)
  | Store (Reg reg, mem) -> [Printf.sprintf "\tstr %s, [fp, #%s]" reg mem]
  | Load (s, Reg reg) -> [Printf.sprintf "\tldr %s, [fp, #%s]" reg s]
  | Push (r) -> [Printf.sprintf "\tpush {%s}" r]
  | _ -> assert false

and generate_asm_fun_internal : reg_function -> string list = fun { name; body } ->
  let rec extract_negative_sizes_regt : regt -> int list = function
  | Let (_, If (_, _, rts1, rts2)) | Exp (If (_, _, rts1, rts2)) ->
    extract_negative_sizes_regt_list rts1 @ extract_negative_sizes_regt_list rts2
  | Store (_, address_str) ->
    begin
      try
        let num = int_of_string address_str in
        if num < 0 then [num] else []
      with
      | Failure _ -> []
    end
  | _ -> []

  and extract_negative_sizes_regt_list rts =
    List.flatten (List.map extract_negative_sizes_regt rts)
  in

  let extract_negative_sizes body =
    List.flatten (List.map extract_negative_sizes_regt body)
  in

  let negative_sizes = extract_negative_sizes body in
  let min_negative_size =
    match negative_sizes with
    | [] -> 0
    | _ -> List.fold_left min max_int negative_sizes
  in
  let size = abs min_negative_size in

  [Printf.sprintf "%s:" name]
  @ generate_prologue size
  @ List.concat (List.map generate_asm_regt body)
  @ generate_epilogue

and generate_prologue size =
  if size <= 255 then
    ["\tpush {fp, lr}"; "\tadd fp, sp, #0"; "\tsub sp, sp, #" ^ string_of_int size]
  else
    let label = generate_num_label () in
    consts := !consts @ [label ^ ": .word " ^ string_of_int size ^ "\n"];
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
        match hd with
        | Fun f when f.name = "_start" ->
          let asm_hd = generate_asm_fun_internal f in
          generate_asm_internal (asm_hd @ acc) tl
        | Fun f ->
          let asm_hd = generate_asm_fun_internal f in
          generate_asm_internal (acc @ asm_hd) tl
    in
    let asm_code = generate_asm_internal [] defs in !header @ !consts @ !floats @ asm_code
  
