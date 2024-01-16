(*exemple de création d'un tuple avec des opérations sur ses éléments pour tester la génération de code asml et arm*)
let rec add_tuple_and_numb t1 t2 g = 
  let (a, b, c) = t1 in 
  let (d, e, f) = t2 in
  let x = a+d in
  let y = b+e in
  let z = c+f in
  let t3 = (x, y, z) in 
  let t4 = t3 in
  let rec get_first v = (let (x, y, z) = v in x) in
  (get_first t4 + g) in
  let res = add_tuple_and_numb (1, 1, 1) (3, 3, 4) 8 in
  print_int(res)