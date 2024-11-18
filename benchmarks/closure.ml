let g =
    let rec f1 n = let rec f2 m = n + m in f2 in f1 in
    print_int ((g 10) 5)
