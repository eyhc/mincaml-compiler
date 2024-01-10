(*exemple de programme avec une closure (qui créer un additionneur) pour tester la génération de code asml*)
let rec addBy x = 
  let y = 40 in
  let rec adder z = x - y + z in
  adder in
  print_int((addBy 1) 35);
  print_newline();
  print_int((addBy 0) 2);
  print_newline();
  print_int((addBy (-3)) 50)