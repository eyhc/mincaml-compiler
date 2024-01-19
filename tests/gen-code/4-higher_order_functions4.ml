let rec g x = x + 5 in
let rec f x = g 
in print_int ((f ()) 6)