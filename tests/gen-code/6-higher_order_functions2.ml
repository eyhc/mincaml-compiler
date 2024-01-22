let rec composition f g x = f (g x) in
let rec double x = x + x in
let rec add_5 x = x + 5 in
print_int (composition double add_5 10) 