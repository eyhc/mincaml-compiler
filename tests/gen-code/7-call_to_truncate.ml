(* exemple de code avec l'appel à truncate pour tester la génération de l'asml et arm avec un float et l'appel à la fonction externe truncate*)
let x = 120.0 in
  let y = sin x in
  let z = sqrt x in
  print_int (truncate (y+.z))