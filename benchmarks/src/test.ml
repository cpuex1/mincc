let rec read_sum _ =
  let x = read_int () in
  let y = read_int () in
  x + y in

print_int (read_sum ())
