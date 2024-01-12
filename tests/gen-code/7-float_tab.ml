(*exemple de programme manipulant des floats dans un tableau pour tester la génération de code asml et arm*)
let tableau = Array.create 4 6.0 in
let rec applyOperation t valeur =
  t.(0) <- t.(0) +. valeur; print_float(t.(0)); print_newline();
  t.(1) <- t.(1) -. valeur; print_float(t.(1)); print_newline();
  t.(2) <- t.(2) /. valeur; print_float(t.(2)); print_newline();
  t.(3) <- t.(3) *. valeur; print_float(t.(3))
in applyOperation tableau 2.5