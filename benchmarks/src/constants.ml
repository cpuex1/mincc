let a = 10 in
    let rec f x = a + (let rec g x = a in g 0) in
        print_int (f 0)
