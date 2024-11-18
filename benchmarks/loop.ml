let rec loop n =
    if n < 0 then
        ()
    else
        (print_int n; loop (n - 1)) in
    loop 10
