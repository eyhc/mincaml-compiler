let x = 5.0 in
  let y = 6.0 in
    let b = x < y in
      if b then
        let z = x +. y in
          print_float(z)
      else
        let z = x -. y in
          print_float(z)