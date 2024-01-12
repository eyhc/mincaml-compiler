(*exemple de code pour utiliser des if avec des arguments de type unit pour tester la génération de code arm*)
let x = () in
  let y = (if x = () then print_int (-1) else print_newline ()) in
    print_int 1010