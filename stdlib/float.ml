(* The king of constant: PI *)
let c_PI = 3.14159265358979323846 in

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
let rec floor x = float_of_int (int_of_float x) in

(* Utility functions for sine and cosine *)
let rec reduction_2pi a =
  let rec double_loop a p =
    if a < p then p else double_loop a (p *. 2.0) in
    let rec half_loop a p =
      if a < c_PI *. 2.0 then
        a
      else
        if a >= p then half_loop (a -. p) (p /. 2.0)
        else half_loop a (p /. 2.0) in
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
  let flag = x > 0.0 in
    let a = fabs x in
      let a = reduction_2pi a in
        if a >= c_PI then
          let a = a -. c_PI in
            let a = if a >= c_PI /. 2.0 then c_PI -. a else a in
              let a = if a <= c_PI /. 4.0 then kernel_sin a else kernel_cos (c_PI /. 2.0 -. a) in
                if flag then -. a else a
        else
          let a = if a >= c_PI /. 2.0 then c_PI -. a else a in
              let a = if a <= c_PI /. 4.0 then kernel_sin a else kernel_cos (c_PI /. 2.0 -. a) in
                if flag then a else -. a in

(* Calculate cosine *)
let rec cos x =
  let a = reduction_2pi (fabs x) in
    let f = a >= c_PI in
      let a = if f then a -. c_PI else a in
        let f2 = a >= fhalf c_PI in
          let f = if f2 then not f else f in
            let a = if f2 then c_PI -. a else a in
              let a = if a <= c_PI /. 4.0 then kernel_cos a else kernel_sin (c_PI /. 2.0 -. a) in
                if f then a else -. a in

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
      if a < 0.4375 then kernel_atan x
      else if a < 2.4375 then
        if x > 0.0 then c_PI /. 4.0 +. kernel_atan ((a -. 1.0) /. (a +. 1.0))
        else -. c_PI /. 4.0 -. kernel_atan ((a -. 1.0) /. (a +. 1.0))
      else if x > 0.0 then c_PI /. 2.0 -. kernel_atan (inv a)
      else -. c_PI /. 2.0 +. kernel_atan (inv a) in
