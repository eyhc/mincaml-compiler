let rec copy_array arr copy size =
  let rec copy_array_rec i =
    if i = size then
      copy
    else
      let x = arr.(i) in
      let y = copy.(i) <- x in
      copy_array_rec (i+1)
  in copy_array_rec 0
in
let rec sort_array arr size =
  let rec for_i i =
    let rec for_j j =
      if j + i + 1 < size then
        let a = arr.(j) in
        let b = arr.(j+1) in
        if a > b then
          let c = arr.(j) <- b in
          let d = arr.(j+1) <- a in
          for_j (j+1)
        else
          for_j (j+1)
      else
        arr
    in
    (* Code de for_i *)
    if i < size then
      let a = for_j 0 in
      for_i (i+1)
    else
      arr
  (* Code de sort_array *)
  in (for_i 0)
in
let rec sum_array arr size =
  let rec sum_array_rec i =
    if i >= size then
      0
    else
      let a = sum_array_rec (i+1) in
      let b = arr.(i) in
      a + b
  in sum_array_rec 0
in
let rec print_arr arr size =
  let rec print_arr_rec i =
    if i = size then
      print_newline()
    else
      let x = arr.(i) in
      print_int x;
      print_newline();
      print_arr_rec (i+1)
  in print_arr_rec 0
in
let size = 6 in
let array = Array.create size 1 in
let a = array.(0) <- 12 in
let b = array.(1) <- 15 in
let c = array.(2) <- 4 in
let d = array.(3) <- 12 in
let e = array.(4) <- 28 in
let f = array.(5) <- 12 in
let copy = Array.create size 1 in
let copy = (copy_array array copy size) in
let sorted = sort_array copy size in
print_arr array size;
print_arr copy size;
print_int (sum_array array size);
print_newline()