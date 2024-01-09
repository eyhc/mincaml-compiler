let rec odd_application x =
  let rec applicate y =
    if y = 10 then 10 else
    x + applicate (y + 1) in
    applicate 0 in
    let res = 30 in
    print_int(odd_application res)