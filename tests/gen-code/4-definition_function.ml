(*exemple de code avec une fonction imbriquée pour tester la génération de code asml et arm*)
let rec find_max_diff a b c =
  let rec max_diff a b =
    if a > b then a 
    else b in
    max_diff (a-b) (max_diff (b-c) (a-c)) in
    print_int(find_max_diff 5 8 2)