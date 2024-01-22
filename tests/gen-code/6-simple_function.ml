(*exemple de code avec une fonction simple et longue pour tester la génération de code ASML et ARM*)
let rec addition a b c =
  let a = 2 in 
  let b = 3 in 
  let d = a + b in
  let rec add2 y = 
    let x = 2 in
    let z = 4 in 
    let w = 6 in
    let j = x + z + w in
   j + a 
  in
  let res = add2 c in
  print_int (d + res)
in
addition 1 2 4