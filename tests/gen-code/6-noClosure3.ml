let rec f x =
  let rec g y =
    y + 10
  in
  (g x) - 5
in print_int (f 10)