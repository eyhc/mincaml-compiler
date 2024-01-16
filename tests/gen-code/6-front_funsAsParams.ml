let rec apply_to_zero f g =
  f (g 0)
in
let rec increment x =
  x + 1
in
let rec double x =
  x + x
in
print_int (apply_to_zero double increment)