(*exemple de programme définissant une fonction de plus haut niveau pour tester la génération de code asml*)
let rec double f x = f (f x) in
let rec ajoute_5 x = x + 5 in
print_int(double ajoute_5 12)