(* The king of constant: PI *)
let c_PI = 3.1415927410125732421875 in

(* Check whether it is zero *)
let rec fiszero x = x = 0.0 in

(* Check whether it is positive *)
let rec fispos x = x > 0.0 in

(* Check whether it is negative *)
let rec fisneg x = x < 0.0 in

(* Compare two float values *)
let rec fless x y = x < y in

(* Negative *)
let rec fneg x = -. x in

(* Calculate the squared value *)
let rec fsqr x = x *. x in

(* Calculate the absolute value *)
let rec fabs x = if x > 0.0 then x else -. x in

(* Divide by 2 *)
let rec fhalf x = x /. 2.0 in

(* Floor *)
let rec floor x =
  let a = float_of_int (int_of_float x) in
    if a > x then a -. 1.0 else a in

(* Utility functions for sine and cosine *)
let rec reduction_2pi a =
  let rec double_loop a p =
    if a < p then p else double_loop a (p *. 2.0) in
    let rec half_loop a p =
      if a < c_PI *. 2.0 then
        a
      else
        if a >= p then half_loop (a -. p) (fhalf p)
        else half_loop a (fhalf p) in
      half_loop a (double_loop a (c_PI *. 2.0)) in
let rec kernel_sin x =
  let x2 = x *. x in
    let x3 = x *. x2 in
      let x5 = x3 *. x2 in
        let x7 = x5 *. x2 in
          x -. 0.16666668 *. x3 +. 0.008332824 *. x5 -. 0.00019587841 *. x7 in
let rec kernel_cos x =
  let x2 = x *. x in
    let x4 = x2 *. x2 in
      let x6 = x4 *. x2 in
        1.0 -. 0.5 *. x2 +. 0.04166368 *. x4 -. 0.0013695068 *. x6 in

(* Calculate sine *)
let rec sin x =
  (* The king of constant: PI *)
  let c_PI = 3.1415927410125732421875 in
    let flag = x > 0.0 in
      let a = fabs x in
        let a = reduction_2pi a in
          let is_less = a >= c_PI in
            let flag = if is_less then not flag else flag in
              let a = if is_less then a -. c_PI else a in
                let a = if a >= fhalf c_PI then c_PI -. a else a in
                  if a <= c_PI /. 4.0 then
                    if flag then
                      kernel_sin a
                    else
                      -. kernel_sin a
                  else
                    if flag then
                      kernel_cos (fhalf c_PI -. a)
                    else
                      -. kernel_cos (fhalf c_PI -. a) in

(* Calculate cosine *)
let rec cos x =
  (* The king of constant: PI *)
  let c_PI = 3.1415927410125732421875 in
    let flag = true in
      let a = reduction_2pi (fabs x) in
        let is_ge = a >= c_PI in
          let a = if is_ge then a -. c_PI else a in
            let flag = if is_ge then not flag else flag in
              let is_ge = a >= fhalf c_PI in
                let a = if is_ge then c_PI -. a else a in
                  let flag = if is_ge then not flag else flag in
                    if a <= c_PI /. 4.0 then
                      if flag then
                        kernel_cos a
                      else
                        -. kernel_cos a
                    else
                      if flag then
                        kernel_sin (fhalf c_PI -. a)
                      else
                        -. kernel_sin (fhalf c_PI -. a) in

(* Calculate arc tangent *)
let rec atan x =
  let rec kernel_atan x =
    let x2 = x *. x in
      let x3 = x *. x2 in
        let x5 = x3 *. x2 in
          let x7 = x5 *. x2 in
            let x9 = x7 *. x2 in
              let x11 = x9 *. x2 in
                let x13 = x11 *. x2 in
                  x -. 0.3333333 *. x3 +. 0.2 *. x5 -. 0.142857142 *. x7 +. 0.111111104 *. x9 -. 0.08976446 *. x11 +. 0.060035485 *. x13 in
    let a = fabs x in
      if a < 0.4375 then
        kernel_atan x
      else
        if a < 2.4375 then
          if x > 0.0 then
            c_PI /. 4.0 +. kernel_atan ((a -. 1.0) /. (a +. 1.0))
          else
            -. c_PI /. 4.0 -. kernel_atan ((a -. 1.0) /. (a +. 1.0))
        else
          if x > 0.0 then
            c_PI /. 2.0 -. kernel_atan (inv a)
          else
            -. c_PI /. 2.0 +. kernel_atan (inv a) in
