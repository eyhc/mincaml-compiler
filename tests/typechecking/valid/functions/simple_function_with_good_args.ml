let rec find_greater x y =
  if x < y then
    print_int(y)
  else if x > y then
    print_int(x)
  else
    print_int(0)
  in
  find_greater 2 5