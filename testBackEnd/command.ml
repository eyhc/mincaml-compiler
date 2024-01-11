open Arg
(*Pour utiliser : ocamlc -o command command.ml
   ./command -h*)
let input_file = ref ""
let output_file = ref ""
let type_check_only = ref false
let print_asml = ref false

let display_help = "Usage: " ^ Sys.argv.(0) ^ " [options]\n-o <file> : output file \n-h : display help \n-v : display version \n-t : only type checking \n-asml : print ASML"


let options_spec = [
  ("-o", Set_string output_file, "Output file");
  ("-h", Unit (fun _ -> print_endline display_help; exit 0), display_help);
  ("-v", Unit (fun _ -> print_endline "Display version"; exit 0), "Display version");
  ("-t", Set type_check_only, "Only type checking");
  ("-asml", Set print_asml, "Print ASML");
]


let handle_error_msg = "Try '" ^ Sys.argv.(0) ^ " -h' for more information."

let () =
    match Sys.argv.(1) with
    | "-o" -> output_file := Sys.argv.(2);
            Printf.printf "Output file: %s\n" !output_file;
            exit(0);
    | "-h" -> print_endline display_help;
            exit(0);
    | "-v" -> Printf.printf "Version : 0.0.1 \n";
            exit(0);
    | "-t" -> Printf.printf "-t\n" (*TODO*);
            exit(0);
    | "-asml" -> Printf.printf "-asml\n" (*TODO*);
            exit(0);
    | _ -> prerr_endline (handle_error_msg);
            exit (1);

        

            