let rec f x = 2.5 in
let rec g x = 5.5 in
if (f () <= g()) then print_float (f ()) else print_float (g ())