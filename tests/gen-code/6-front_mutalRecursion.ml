let a = 5 in
let rec f x =
  let rec g y =
    if y > 0 then
      a + f (y - 1)
    else
      0
  in
  if x > 0 then
    a + g (x - 1)
  else
    0
in
print_int (f 10)