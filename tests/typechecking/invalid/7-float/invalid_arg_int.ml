let rec superior a b =
  if a /. b > 1.0 then a
  else if b /. a > 1.0 then b
  else 0.0
in print_float(superior 5.6 3)