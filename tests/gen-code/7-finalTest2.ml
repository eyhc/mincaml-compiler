let rec swap_tuple3 tuple =
  let (a, b, c) = tuple in
  (c, b, a)
in
let rec tuple3_to_array tuple =
  let array = Array.create 3 1 in
  let (a, b, c) = tuple in
  let d = array.(0) <- a in
  let e = array.(1) <- b in
  let f = array.(2) <- c in
  array
in
let rec compare_tuple3_array tuple array =
  let (a, b, c) = tuple in
  let d = array.(0) in
  let e = array.(1) in
  let f = array.(2) in
  if a = d then
    if b = e then
      if c = f then
        0
      else
        1
    else
      1
  else
    1
in
let t = (15, 8, 12) in
let arr = tuple3_to_array t in
let te = swap_tuple3 t in
let res = compare_tuple3_array t arr in
print_int res;
print_newline();
let res = compare_tuple3_array te arr in
print_int res;
print_newline()