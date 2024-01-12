(*exemple de code pour utiliser des if imbriqués pour tester la génération de code asml et arm*)
let x = 0 in
let y = 0 in
let x = 
  (if x = (if x = (if x = 1 then 0 else 2) then 1 else 5) then x+2 else 33)
in print_int x