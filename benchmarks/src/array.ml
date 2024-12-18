let a = Array.make 3 4 in
  let b = Array.make 3 a in
    print_int (b.(0).(0) + b.(1).(1) + b.(2).(2))
