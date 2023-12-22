(* USAGE DEFINITION & HELP *)

(* version *)
let show_version () =
  print_endline "MinCamlCompiler v0.1 - 21-12-2023";
  exit 0

let input = ref "" and output = ref ""

let type_only = ref false and asml_only = ref false

let usage_msg = Printf.sprintf "Usage: %s [options]" (Array.get Sys.argv 0)

let speclist = [
  ("-i", Arg.Set_string (input), "Input file");
  ("-o", Arg.Set_string (output), "Output file");
  ("-v", Arg.Unit (show_version), "Show version");
  ("-t", Arg.Unit (fun () -> type_only := true), "Type checking only");
  ("-asml", Unit (fun () -> asml_only := true), "Print asml")
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

(* Type Checking function *)
let type_check_only f = 
  let ast = get_ast f in
    Typechecker.type_check ast;
    print_endline "Type inference : OK"

(* Display asml of file f*)
let print_asml f =
  let ast = get_ast f in
    (*Typechecker.type_check ast;*)              (* Typechecking *)
    let ast = Knorm.k_normalization ast in  (* K-normalization *)
    (* let ast = Alpha.conversion ast in      (*  alpha-conversion *)*)
    let ast = Reduction.reduction ast in     (* reduction of nested-let *)
    let ast = Closure.conversion ast in      (* closure conversion *)
                                             (* ASML generation *)
    print_endline (Closure.to_string ast)     (* Affichage *)


let main (inp:string) (out:string) : unit = 
  print_endline "compilation.... todo"

(* MAIN *)
let () = 
      Arg.parse speclist (fun x -> input := x) usage_msg;

      if String.length !input = 0 then
        show_help 1
      else if !type_only then
        type_check_only !input
      else if !asml_only then
        print_asml !input
      else if String.length !output = 0 then
        show_help 1
      else
        main !input !output