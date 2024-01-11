(* USAGE DEFINITION & HELP *)

(* version *)
let show_version () =
  print_endline "MinCamlCompiler v0.1 - 21-12-2023";
  exit 0

(* Global variables for Arg's parser *)
let input = ref "" and output = ref ""

(* -p, -t, -asml *)
let type_only = ref false and asml_only = ref false and parse_only = ref false

(* -test *)
let test = ref false 
let knorm_only = ref false and alpha = ref false 
and let_reduc = ref false and closure = ref false and optim = ref false

(* -n_iter *)
let n_iter_optim = ref 100

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
  ("-test", Arg.Unit (fun () -> test := true), "Show test results");
  ("-knorm", Arg.Unit(fun () -> knorm_only :=true), "Knormalization only");
  ("-alpha", Arg.Unit(fun () -> alpha :=true), "Alpha-reduction");
  ("-let", Arg.Unit(fun () -> let_reduc :=true), "Reduction of nested let-expression");
  ("-closure", Arg.Unit(fun () -> closure :=true), "Closure conversion");
  ("-optim", Arg.Unit(fun () -> optim :=true), "test optimisation")
]

(* SHOW HELP IN TERM (-h option) *)
let show_help r =
  Arg.usage speclist usage_msg;
  exit r



(*********************
    MAIN FUNCTIONS
 *********************)

let get_ast file = 
  let inchan = open_in file in
  (try
    Parser.exp Lexer.token (Lexing.from_channel inchan)
  with e -> (close_in inchan; raise e))

let print_ast f =
  let ast = get_ast f in
    print_endline (Syntax.to_string ast)

(* Type Checking function *)
let type_check_only f = 
  let ast = get_ast f in
    Typechecker.type_check ast;
    print_endline (Syntax.to_string_with_types ast);
    print_endline "Type inference : OK"

(* Test compiler steps functions *)
let print_knorm ast =
  Typechecker.type_check ast;
  let res = Knorm.normalize ast in
    print_endline (Knorm.to_string_with_type res)

let print_alpha ast =
  Typechecker.type_check ast;
  let res = Knorm.normalize ast in
    let res = Alpha.conversion res in
    print_endline (Knorm.to_string res)

let print_let_reduc ast =
  Typechecker.type_check ast;
  let res = Knorm.normalize ast in
    let res = Alpha.conversion res in
      let res = Reduction.reduction res in
      print_endline (Knorm.to_string res)

let print_optim ast =
  Typechecker.type_check ast;
  let res = Knorm.normalize ast in
  let res = Alpha.conversion res in
  let res = Beta.reduction res in 
  let res = Reduction.reduction res in
  let res = Inline.expansion res in
  print_endline (Knorm.to_string res)

let print_closure ast =
  Typechecker.type_check ast;
  let res = Knorm.normalize ast in
    let res = Alpha.conversion res in
      let res = Reduction.reduction res in
        let res = Closure.closure res in
        print_endline (Closure.to_string res)

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
    else
      print_endline "The function you want to test is missing ! Put the corresponding argument : -knorm; -alpha; -let; -closure"

let iter_optim ast =
  let rec iter_rec ast n =
    if n = 0 then ast
    else
      let a = Beta.reduction ast in          (* Beta reduction *)
      let a = Reduction.reduction a in       (* Reduction of nested-let *)
      let a = Inline.expansion a in          (* Inline expansion *)
      (* Constant folding | Elim. unnecessary def *)
      a (* to_do : ne plus itérérer si on atteint un point fixe *)
  in iter_rec ast !n_iter_optim

(* Display asml of file f*)
let print_asml f =
  let ast = get_ast f in
    Typechecker.type_check ast;              (* Typechecking *)
    let ast = Knorm.normalize ast in         (* K-normalization *)
    let ast = Alpha.conversion ast in        (* Alpha-conversion *)
    let ast = iter_optim ast in              (* Optimisations *)
    let ast = Closure.closure ast in         (* Closure conversion *)
    let asml = Asml.generation ast in        (* ASML generation *)
                                             (* Immediate optimisation *)
    print_endline (Asml.to_string asml)      (* Displaying *)


(* Compile code file f to arm (32?) *)
let main (inp:string) (out:string) : unit = 
  let ast = get_ast inp in
    Typechecker.type_check ast;              (* Typechecking *)
    let ast = Knorm.normalize ast in         (* K-normalization *)
    let ast = Alpha.conversion ast in        (* Alpha-conversion *)
    let ast = iter_optim ast in              (* Optimisations *)
    let ast = Closure.closure ast in         (* Closure conversion *)
    let asml = Asml.generation ast in        (* ASML generation *)
                                             (* Immediate optimisation *)
                                             (* ARM generation *)
    ();                                      (* Saving result in file *)
    print_endline (Asml.to_string asml)


(* MAIN *)
let () = 
  Arg.parse speclist (fun x -> input := x) usage_msg;

  if String.length !input = 0 then
    show_help 1
  else if !parse_only then
    print_ast !input
  else if !type_only then
    type_check_only !input
  else if !asml_only then
    print_asml !input
  else if !test then
    print_test !input
  else if String.length !output = 0 then
    show_help 1
  else
    main !input !output