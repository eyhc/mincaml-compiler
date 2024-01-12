(*exemple de modification de la valeur d'un élément d'un tableau pour tester la génération de code asml et arm*)
let rec modify_value tableau index valeur = 
  let old_value = tableau.(index) in
  (tableau.(index) <- valeur;
  print_int old_value) in
let tab = Array.create 8 4 in
modify_value tab 5 10;
print_newline();
modify_value tab 5 7