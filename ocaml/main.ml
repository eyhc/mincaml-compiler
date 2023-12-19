let file f = 
  let inchan = open_in f in
  try
    let ast = Parser.exp Lexer.token (Lexing.from_channel inchan) in
    let table = (Syntax.find_variables ast []) in
    Printf.printf "Table des symboles\n";
    List.iter (Printf.printf "%s ") table;
    print_newline ();

    (*  (* Type Checking *)
      let ast = Parser.exp Lexer.token (Lexing.from_channel inchan) in
      Typechecker.typeCheck ast;   
    *)

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