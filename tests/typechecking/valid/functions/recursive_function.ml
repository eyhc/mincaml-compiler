let rec reste x y a =
  let rec quotient x y a =
    let diff = x-y in
      if diff < 0 then
        a
      else
        quotient diff y (a+x)
    in
  let b = x - (quotient x y a) in
  print_int b in
  reste 10 3 0