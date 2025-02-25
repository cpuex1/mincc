let rec div10 n =
  if n < 10 then 0 else 1 + div10 (n - 10) in

let rec print_int n =
  let q = div10 n in
    let r = n - q * 8 - q * 2 in
      if q <> 0 then
        (print_int q; print_char (r + 48))
      else
        print_char (r + 48) in
