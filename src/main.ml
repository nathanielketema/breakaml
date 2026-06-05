open Raylib

let setup () =
  Raylib.init_window 700 450 "Breakaml";
  Raylib.set_target_fps 60

let game_loop () =
  let x = ref 10 in
  let y = ref 10 in
  while not (window_should_close ()) do
    begin_drawing ();
    clear_background Color.raywhite;
    draw_rectangle !x !y 100 100 Color.red;
    x := !x + 1;
    incr y;
    end_drawing ()
  done;
  close_window ()

let () = setup () |> game_loop
