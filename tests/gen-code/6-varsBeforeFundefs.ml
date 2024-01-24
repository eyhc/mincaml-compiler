let a = 10 in
let b = 87 in
let rec f x =
  if x > 0 then
    a + f (x - 1)
  else
    b
in
print_int (f 10)