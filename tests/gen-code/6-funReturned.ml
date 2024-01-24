let rec double x =
  x + x
in
let rec sub10 x =
  x - 10
in
let rec choose x = 
  if x > 0 then
    double
  else
    sub10
in
print_int ((choose 10) 10)