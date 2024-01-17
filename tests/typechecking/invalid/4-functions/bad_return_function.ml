let rec reste x y a =
  let rec quotient x y a =
    if (x-y) < 0 then
      a
    else
      quotient (x-y) y (a+x)
    in
    let b = x -. (quotient x y a) in
  print_float b in
  reste 10.0 3 0