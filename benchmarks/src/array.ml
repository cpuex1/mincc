let a = Array.make 3 4 in
  let b = Array.make 3 a in
    let rec f x =
      let c = Array.make 3 x in
        b.(1) <- c in
      f 10; print_int (b.(0).(0) + b.(1).(1) + b.(2).(2))
