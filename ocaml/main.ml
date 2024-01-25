(* USAGE DEFINITION & HELP *)

(* version *)
let show_version () =
  print_endline "MinCamlCompiler v0.3 - 25-01-2024";
  exit 0

(* Global variables for Arg's parser *)
let input = ref "" and output = ref ""

(* -p, -t, -asml *)
let type_only = ref false and asml_only = ref false and parse_only = ref false

(* -test *)
let test = ref false 
let knorm_only = ref false and alpha = ref false 
and let_reduc = ref false and closure = ref false and optim = ref false
and back_print = ref false


(* -n_iter *)
let n_iter_optim = ref 50

(* -show_type *)
let show_type = ref false


(************************************************************************)

let usage_msg = Printf.sprintf "Usage: %s [options]" (Array.get Sys.argv 0)

(* Arg parser speclist : CF Arg library *)
let speclist = [
  ("-i", Arg.Set_string (input), "Input file");
  ("-o", Arg.Set_string (output), "Output file");
  ("-v", Arg.Unit (show_version), "Show version");
  ("-t", Arg.Unit (fun () -> type_only := true), "Type checking only");
  ("-asml", Arg.Unit (fun () -> asml_only := true), "Print asml");
  ("-p", Arg.Unit (fun () -> parse_only := true), "Parse only");
  ("-n_iter", Arg.Set_int(n_iter_optim), "<integer> Set the number of optimisation iterations");
  ("-inline_depth", Arg.Set_int(Inline.max_depth), "<i> Set the max_deep of inline expansion");
  ("-show_type", Arg.Unit (fun () -> show_type := true), "show type informations");
  ("-test-knorm", Arg.Unit(fun () -> test := true; knorm_only :=true), "Knormalization only");
  ("-test-alpha", Arg.Unit(fun () -> test := true; alpha :=true), "Alpha-reduction");
  ("-test-let", Arg.Unit(fun () -> test := true; let_reduc :=true), "Reduction of nested let-expression");
  ("-test-closure", Arg.Unit(fun () -> test := true; closure :=true), "Closure conversion");
  ("-test-back", Arg.Unit(fun () -> test := true; back_print :=true), "Back code intermediaire");
  ("-test-optim", Arg.Unit(fun () -> test := true; optim :=true), "test optimisation")
]

(* SHOW HELP IN TERM (-h option) *)
let show_help r =
  Arg.usage speclist usage_msg;
  exit r


(*********************
    MAIN FUNCTIONS
 *********************)

(*======= ancillary functions =======*)
let get_ast file = 
  let inchan = open_in file in
  (try
    Parser.exp Lexer.token (Lexing.from_channel inchan)
  with e -> (close_in inchan; raise e))

let set_arm_file arm out_file =
  let oc_reg = open_out out_file in
    List.iter (fun instruction -> output_string oc_reg (instruction ^ "\n")) arm;
    close_out oc_reg

let set_asml_file asml out_file =
  let oc_reg = open_out out_file in
    output_string oc_reg (Asml.to_string asml);
    close_out oc_reg


(*======= Tests fonctions =======*)
let test_parse f =
  let _ = get_ast f in ()

(* Type Checking function *)
let type_check_only f = 
  let ast = get_ast f in
    Typechecker.type_check ast

(* Test compiler steps functions *)
let print_knorm ast =
  Typechecker.type_check ast;
  let res = Knorm.normalize ast in
    if !show_type then
      print_endline (Knorm.to_string_with_type res)
    else
      print_endline (Knorm.to_string res)

let print_alpha ast =
  Typechecker.type_check ast;
  let res = Knorm.normalize ast in
  let res = Alpha.conversion res in
    if !show_type then
      print_endline (Knorm.to_string_with_type res)
    else
      print_endline (Knorm.to_string res)

let print_let_reduc ast =
  Typechecker.type_check ast;
  let res = Knorm.normalize ast in
  let res = Alpha.conversion res in
  let res = Reduction.reduction res in
    if !show_type then
      print_endline (Knorm.to_string_with_type res)
    else
      print_endline (Knorm.to_string res)

let print_optim ast =
  Typechecker.type_check ast;
  let res = Knorm.normalize ast in
  let res = Alpha.conversion res in
  let res = Beta.reduction res in
  let res = Reduction.reduction res in
  let res = Inline.expansion res in
  let res = Constant.folding res in
  let res = Elim.elim_definition res in
    if !show_type then
      print_endline (Knorm.to_string_with_type res)
    else
      print_endline (Knorm.to_string res)

let print_closure ast =
  Typechecker.type_check ast;
  let res = Knorm.normalize ast in
  let res = Alpha.conversion res in
  let res = Reduction.reduction res in
  let res = Closure.closure res in
    print_endline (Closure.to_string res)

let print_back ast =
  Typechecker.type_check ast;
  let ast = Knorm.normalize ast in
  let ast = Alpha.conversion ast in
  let ast = Reduction.reduction ast in
  let ast = Closure.closure ast in
  let asml = Asml.generation ast in
  let asml = ImmOptim.optim asml in
  let b = RegAlloc.parcours asml in 
  RegAlloc.print_reg_function b
    
let print_test f =
  let ast = get_ast f in
    if !knorm_only then
      print_knorm ast
    else if !alpha then
      print_alpha ast
    else if !optim then 
      print_optim ast
    else if !let_reduc then
      print_let_reduc ast
    else if !closure then
      print_closure ast
    else if !back_print then
      print_back ast
    else
      print_endline "The function you want to test is missing ! Put the corresponding argument : -knorm; -alpha; -let; -closure"


(********************************************************)
(********************************************************)
let iter_optim ast =
  let rec iter_rec ast n =
    if n = 0 then ast
    else
      let a = Beta.reduction ast in          (* Beta reduction *)
      let a = Reduction.reduction a in       (* Reduction of nested-let *)
      let a = Inline.expansion a in          (* Inline expansion *)
      let a = Constant.folding a in          (* Constant folding *)
      let a = Elim.elim_definition a in      (* Elim. unnecessary def *)
      iter_rec a (n-1) (* to do : si point fixe ???? *)
  in iter_rec ast !n_iter_optim


(* Display asml of file f*)
let print_asml f_in f_out =
  let ast = get_ast f_in in
    Typechecker.type_check ast;          (* Typechecking *)
    let ast = Knorm.normalize ast in     (* K-normalization *)
    let ast = Alpha.conversion ast in    (* Alpha-conversion *)
    let ast = iter_optim ast in          (* Optimizations *)
    let ast = Reduction.reduction ast in (* Reduction of nested-let *)
    let ast = Closure.closure ast in     (* Closure conversion *)
    let asml = Asml.generation ast in    (* ASML generation *)
    let asml = ImmOptim.optim asml in    (* Immediate optimization *)
    (match f_out with
    | None -> print_endline (Asml.to_string asml)
    | Some out -> set_asml_file asml out
    )


(* Compile code file f to arm (32?) *)
let main (inp:string) (out:string) : unit = 
  let ast = get_ast inp in
    Typechecker.type_check ast;           (* Typechecking *)
    let ast = Knorm.normalize ast in      (* K-normalization *)
    let ast = Alpha.conversion ast in     (* Alpha-conversion *)
    let ast = iter_optim ast in           (* Optimisations *)
    let ast = Reduction.reduction ast in 
    let ast = Closure.closure ast in      (* Closure conversion *)
    let asml = Asml.generation ast in     (* ASML generation *)
    let b = RegAlloc.parcours asml in          (* Register allocation *)                     
    let arm = Generation.generate_asm_reg b in (* ARM generation *)
    set_arm_file arm out                       (* Saving result in file *)


(* MAIN *)
let () = 
  try 
    Arg.parse speclist (fun x -> input := x) usage_msg;

    if String.length !input = 0 then
      show_help 1
    else if !parse_only then
      test_parse !input
    else if !type_only then
      type_check_only !input
    else if !test then
      print_test !input
    else if !asml_only then
      (if String.length !output = 0 then
        print_asml !input None
      else
        print_asml !input (Some !output))
    else if String.length !output = 0 then
      show_help 1
    else
      main !input !output
  with
  | e -> Printf.eprintf "%s\n" (Printexc.to_string e); exit(1)
