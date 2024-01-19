let rec g x y = x + y in
let rec f x y = g x (x - y) in
print_int (f 12 5)