let rec subBy x = 
  let rec substractor y = x - y in
  substractor in
  let res = (subBy 3) 6.0 in
  print_int(res)