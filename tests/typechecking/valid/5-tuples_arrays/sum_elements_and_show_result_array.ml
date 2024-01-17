let rec change_val tab index new_val =
  tab.(index) <- new_val in
  let tableau = Array.create 6 2 in
  change_val tableau 2 4;
  change_val tableau 0 1;
  change_val tableau 5 9;
  let rec sum_tab tab index result =
    if index = 5 then
      result
    else sum_tab tab (index + 1) (result + tab.(index)) in
    print_int(sum_tab tableau 0 0)