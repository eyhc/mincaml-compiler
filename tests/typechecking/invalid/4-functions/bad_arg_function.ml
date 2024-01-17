let rec int_div x y z =
  if x-y < 0 then
    print_int(z)
  else
    int_div (x-y) y (z+1)
  in
int_div 3 2.1 0