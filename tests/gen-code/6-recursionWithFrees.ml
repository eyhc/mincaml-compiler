let a = 14 in
let rec f x =
  if x > 0 then
    a + f (x - 1)
  else
    a
in
print_int (f 10)