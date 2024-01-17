let x = 5.0 in
  let y = 6.0 in
    if x = y then
      let z = x -. y in
        print_float(z)
    else
      let z = x +. y in
        print_int(z)