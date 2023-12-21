let print_ast ast =
  print_string (Syntax.to_string ast); print_newline ()

let file f = 
  let inchan = open_in f in
  try
    let ast = Parser.exp Lexer.token (Lexing.from_channel inchan) in
    (*let env = Typechecker.predef in
    let liste_equation = Typechecker.genEquations ast env Int in
    let res = Typechecker.to_string liste_equation in*)
    Typechecker.typeCheck(ast);
    Printf.printf("Resultat\n");
    (*Printf.printf("%s") res;*)

    (*let table = (Syntax.find_variables ast []) in
    let c_ast = (K_normalization.k_normalization (Syntax.clone_ast ast)) in
    (* let table = (Syntax.find_variables ast []) in
    Printf.printf "Table des symboles\n";
    List.iter (Printf.printf "%s ") res;
    print_newline ();*)
    List.iter (Printf.printf "%s ") table;
    print_newline (); *)
    print_ast ast;
    print_newline ();
    print_ast c_ast;
    close_in inchan
  with e -> (close_in inchan; raise e)

let () = 
  let files = ref [] in
  Arg.parse
    [ ]
    (fun s -> files := !files @ [s])
    (Printf.sprintf "usage: %s filenames" Sys.argv.(0));
  List.iter
    (fun f -> ignore (file f))
    !files