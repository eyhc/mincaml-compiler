let rec get_first v = (let (x, y, z) = v in x) in
let rec get_second v = (let (x, y, z) = v in y) in
let rec get_third v = (let (x, y, z) = v in z) in
let rec total tuple = get_first tuple + get_second tuple + get_third tuple in
let rec soustraire a b =
  let x = get_first a - get_first b in
  let y = get_second a - get_second b in
  let z = get_third a - get_third b in
  (x, y, z) in
let res = soustraire (5, 8, 2) (7, 1, 15) in
print_int(total res)