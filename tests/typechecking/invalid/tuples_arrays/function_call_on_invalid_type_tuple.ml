let numeros = (1, 5.06, 10) in
  let rec show_numbers t =
    let(n1, n2, n3) = t in
    print_int n1;
    print_int n2; 
    print_int n3
  in show_numbers numeros
