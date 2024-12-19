let rec target x = sin x in

let width = read_int () in
let height = read_int () in

let left = read_float () in
let right = read_float () in
let bottom = read_float () in
let top = read_float () in

let screen = let dummy = Array.make height (-1) in
  Array.make width dummy in
let rec init_screen x =
  if x >= width then
    ()
  else
    (
      let new_arr = Array.make height 0 in
        screen.(x) <- new_arr;
        init_screen (x + 1)
    ) in

let rec x_loop x =
  if x >= width then
    ()
  else
    let x_f = left +. (right -. left) *. float_of_int x /. float_of_int width in
      let y_f = target x_f in
        let y = int_of_float ((y_f -. bottom) /. (top -. bottom) *. float_of_int height) in
          (
            if y >= 0 then
              if y < height then
                screen.(x).(y) <- 1
              else
                ()
            else
              ()
          ); x_loop (x + 1) in

let rec render_pixel x y =
  if x >= width then
    ()
  else
    (
      print_int screen.(x).(y);
      print_char 32; (* LF *)
      render_pixel (x + 1) y
    ) in

let rec render_line y =
  if y >= height then
    ()
  else
    (
      render_pixel 0 (height - y - 1);
      print_char 10; (*   *)
      render_line (y + 1)
    ) in

let rec render _ =
  print_char 80; (* P *)
  print_char 49; (* 1 *)
  print_char 10; (*   *)
  print_int width;
  print_char 32; (* LF *)
  print_int height;
  print_char 10; (*   *)
  render_line 0 in

init_screen 0;
x_loop 0;
render ()
