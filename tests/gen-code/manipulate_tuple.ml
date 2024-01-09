(*exemple de création d'un tuple et de manipulation de ses éléments pour tester la génération du code asml*)
let rec create_tuple a b c = (a, b, c) in
  let rec change_elements tuple = 
    let (x, y, z) = tuple in
    (y, z, x) in
    let rec get_first v = (let (x, y, z) = v in x) in
    let tuple = create_tuple 1 2 3 in
    print_int(get_first (change_elements (change_elements tuple)))