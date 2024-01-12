let rec multiply a b x =
  if x = 0 then a
  else multiply (a *. b) b (x-1) in
  print_float(multiply 2.5 3.0 10)