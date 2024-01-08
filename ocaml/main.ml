(* USAGE DEFINITION & HELP *)

(* version *)
let show_version () =
  print_endline "MinCamlCompiler v0.1 - 21-12-2023";
  exit 0

(* Global variables for Arg's parser *)
let input = ref "" and output = ref ""
let type_only = ref false and asml_only = ref false and parse_only = ref false
let test = ref false and knorm_only = ref false and alpha = ref false and let_reduc = ref false and closure = ref false

let usage_msg = Printf.sprintf "Usage: %s [options]" (Array.get Sys.argv 0)

let speclist = [
  ("-i", Arg.Set_string (input), "Input file");
  ("-o", Arg.Set_string (output), "Output file");
  ("-v", Arg.Unit (show_version), "Show version");
  ("-t", Arg.Unit (fun () -> type_only := true), "Type checking only");
  ("-asml", Unit (fun () -> asml_only := true), "Print asml");
  ("-p", Arg.Unit (fun () -> parse_only := true), "Parse only");
  ("-test", Unit (fun () -> test := true), "Show test results");
  ("-knorm", Unit(fun () -> knorm_only :=true), "Knormalization only");
  ("-alpha", Unit(fun () -> alpha :=true), "Alpha-reduction");
  ("-let", Unit(fun () -> let_reduc :=true), "Reduction of nested let-expression");
  ("-closure", Unit(fun () -> closure :=true), "Closure conversion")
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
  let res = Knorm.k_normalization ast in
    print_endline (Knorm.to_string_with_type res)

let print_alpha ast =
  Typechecker.type_check ast;
  let res = Knorm.k_normalization ast in
    let res = Alpha.conversion res in
    print_endline (Knorm.to_string res)

let print_let_reduc ast =
  Typechecker.type_check ast;
  let res = Knorm.k_normalization ast in
    let res = Alpha.conversion res in
      let res = Reduction.reduction res in
      print_endline (Knorm.to_string res)

let print_closure ast =
  Typechecker.type_check ast(*;
  let res = Knorm.k_normalization ast in
    let res = Alpha.conversion res in
      let res = Reduction.reduction res in
        let res = Closure.conversion res in
        print_endline (Closure.to_string res)*)

let print_test f =
  let ast = get_ast f in
    if !knorm_only then
      print_knorm ast
    else if !alpha then
      print_alpha ast
    else if !let_reduc then
      print_let_reduc ast
    else if !closure then
      print_closure ast
    else
      print_endline "The function you want to test is missing ! Put the corresponding argument : -knorm; -alpha; -let; -closure"

(* Display asml of file f*)
let print_asml f =
  let ast = get_ast f in
    Typechecker.type_check ast;              (* Typechecking *)
    let ast = Knorm.k_normalization ast in   (* K-normalization *)
    let ast = Alpha.conversion ast in        (*  alpha-conversion *)
    let ast = Reduction.reduction ast in     (* reduction of nested-let *)
    (*let ast = Closure.conversion ast in *)     (* closure conversion *)
    (*let asml = Asml. *)                        (* ASML generation *)
    print_endline (Knorm.to_string ast)      (* Affichage *)


(* Compile code file f to arm (32?) *)
let main (inp:string) (out:string) : unit = 
  print_endline "compilation.... todo"


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