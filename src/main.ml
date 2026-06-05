let setup () =
  Raylib.init_window 700 450 "Breakaml";
  Raylib.set_target_fps 60

let loop () =
  let open Raylib in
  while not (window_should_close ()) do
    begin_drawing ();
    clear_background Color.raywhite;
    draw_rectangle 300 150 100 100 Color.red;
    end_drawing ()
  done;
  close_window ()

let () = setup () |> loop
