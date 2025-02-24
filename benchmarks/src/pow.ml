let rec pow x n =
    if n = 0 then 1.0 else x *. pow x (n - 1) in
    print_int (int_of_float (pow 2.0 10))
