open Raylib

type screen = Start | Playing | GameOver | Victory

type bricks = {
  x : float array;
  y : float array;
  mutable count : int;
  width : float;
  height : float;
}

type ball = {
  mutable x : float;
  mutable y : float;
  mutable velocity_x : float;
  mutable velocity_y : float;
  radius : float;
}

type paddle = {
  mutable x : float;
  y : float;
  width : float;
  height : float;
  speed : float;
}

type world = {
  mutable screen : screen;
  ball : ball;
  paddle : paddle;
  bricks : bricks;
}

let window_width = 650.0
let window_height = 700.0

let brick_columns = 10
let brick_rows = 4
let bricks_count_max = brick_columns * brick_rows
let brick_gap_x = 10.0
let brick_gap_y = 10.0
let brick_margin_x = 50.0
let brick_margin_y = 50.0
let brick_height = 20.0

let paddle_width = 150.0
let paddle_height = 20.0
let paddle_margin_bottom = 100.0
let paddle_speed = 650.0

let ball_radius = 8.0
let ball_velocity_x_start = 420.0
let ball_velocity_y_start = -480.0

let brick_width =
  (window_width
  -. (2.0 *. brick_margin_x)
  -. (float_of_int (brick_columns - 1) *. brick_gap_x))
  /. float_of_int brick_columns

let brick_origin_x = brick_margin_x
let brick_origin_y = brick_margin_y

let paddle_x_start = (window_width -. paddle_width) /. 2.0
let paddle_y = window_height -. paddle_margin_bottom

let bricks_bottom =
  brick_origin_y
  +. (float_of_int brick_rows *. brick_height)
  +. (float_of_int (brick_rows - 1) *. brick_gap_y)

let ball_x_start = window_width /. 2.0
let ball_y_start = (bricks_bottom +. paddle_y) /. 2.0

let text_title_y = int_of_float (window_height *. 0.36)
let text_subtitle_y = int_of_float (window_height *. 0.50)

let make_bricks () : bricks =
  {
    x = Array.make bricks_count_max 0.0;
    y = Array.make bricks_count_max 0.0;
    count = 0;
    width = brick_width;
    height = brick_height;
  }

let fill_bricks (bricks : bricks) =
  let stride_x = brick_width +. brick_gap_x in
  let stride_y = brick_height +. brick_gap_y in
  let index = ref 0 in
  for row = 0 to brick_rows - 1 do
    for column = 0 to brick_columns - 1 do
      bricks.x.(!index) <- brick_origin_x +. (float_of_int column *. stride_x);
      bricks.y.(!index) <- brick_origin_y +. (float_of_int row *. stride_y);
      incr index
    done
  done;
  bricks.count <- bricks_count_max;
  bricks

let destroy_brick (bricks : bricks) index =
  let index_last = bricks.count - 1 in
  bricks.x.(index) <- bricks.x.(index_last);
  bricks.y.(index) <- bricks.y.(index_last);
  bricks.count <- index_last

let make_world () : world =
  {
    screen = Start;
    ball =
      {
        x = ball_x_start;
        y = ball_y_start;
        velocity_x = ball_velocity_x_start;
        velocity_y = ball_velocity_y_start;
        radius = ball_radius;
      };
    paddle =
      {
        x = paddle_x_start;
        y = paddle_y;
        width = paddle_width;
        height = paddle_height;
        speed = paddle_speed;
      };
    bricks = make_bricks () |> fill_bricks;
  }

let reset_play (world : world) =
  world.screen <- Playing;
  world.ball.x <- ball_x_start;
  world.ball.y <- ball_y_start;
  world.ball.velocity_x <- ball_velocity_x_start;
  world.ball.velocity_y <- ball_velocity_y_start;
  world.paddle.x <- paddle_x_start;
  world.bricks |> fill_bricks |> ignore;
  world

let system_input frame_time (world : world) =
  let paddle = world.paddle in
  if is_key_down Key.Left then
    paddle.x <- paddle.x -. (paddle.speed *. frame_time);
  if is_key_down Key.Right then
    paddle.x <- paddle.x +. (paddle.speed *. frame_time);
  if paddle.x < 0.0 then paddle.x <- 0.0;
  let paddle_x_max = window_width -. paddle.width in
  if paddle.x > paddle_x_max then paddle.x <- paddle_x_max;
  world

let system_integrate frame_time (world : world) =
  let ball = world.ball in
  ball.x <- ball.x +. (ball.velocity_x *. frame_time);
  ball.y <- ball.y +. (ball.velocity_y *. frame_time);
  world

let system_walls (world : world) =
  let ball = world.ball in
  let radius = ball.radius in
  if ball.x -. radius < 0.0 then begin
    ball.x <- radius;
    ball.velocity_x <- abs_float ball.velocity_x
  end;
  if ball.x +. radius > window_width then begin
    ball.x <- window_width -. radius;
    ball.velocity_x <- -.abs_float ball.velocity_x
  end;
  if ball.y -. radius < 0.0 then begin
    ball.y <- radius;
    ball.velocity_y <- abs_float ball.velocity_y
  end;
  world

let circle_hits_rect circle_x circle_y radius rect_x rect_y rect_width
    rect_height =
  circle_x +. radius > rect_x
  && circle_x -. radius < rect_x +. rect_width
  && circle_y +. radius > rect_y
  && circle_y -. radius < rect_y +. rect_height

let system_paddle (world : world) =
  let ball = world.ball and paddle = world.paddle in
  if
    ball.velocity_y > 0.0
    && circle_hits_rect ball.x ball.y ball.radius paddle.x paddle.y paddle.width
         paddle.height
  then begin
    let hit = (((ball.x -. paddle.x) /. paddle.width) -. 0.5) *. 2.0 in
    let speed = hypot ball.velocity_x ball.velocity_y in
    let angle = hit *. 1.0 in
    ball.velocity_x <- speed *. sin angle;
    ball.velocity_y <- -.speed *. cos angle;
    ball.y <- paddle.y -. ball.radius
  end;
  world

let system_bricks (world : world) =
  let ball = world.ball and bricks = world.bricks in
  let index = ref 0 in
  while !index < bricks.count do
    let brick_x = bricks.x.(!index) and brick_y = bricks.y.(!index) in
    if
      circle_hits_rect ball.x ball.y ball.radius brick_x brick_y bricks.width
        bricks.height
    then begin
      let overlap_x =
        let left = ball.x +. ball.radius -. brick_x in
        let right = brick_x +. bricks.width -. (ball.x -. ball.radius) in
        min left right
      in
      let overlap_y =
        let top = ball.y +. ball.radius -. brick_y in
        let bottom = brick_y +. bricks.height -. (ball.y -. ball.radius) in
        min top bottom
      in
      if overlap_x < overlap_y then ball.velocity_x <- -.ball.velocity_x
      else ball.velocity_y <- -.ball.velocity_y;
      destroy_brick bricks !index
    end
    else incr index
  done;
  world

let system_outcome (world : world) =
  if world.ball.y -. world.ball.radius > window_height then
    world.screen <- GameOver
  else if world.bricks.count = 0 then world.screen <- Victory;
  world

let draw_text_centered text y size color =
  measure_text text size |> fun text_width ->
  draw_text text ((int_of_float window_width - text_width) / 2) y size color

let system_draw_start () =
  begin_drawing ();
  clear_background Color.black;
  draw_text_centered "BREAKAML" text_title_y 60 Color.white;
  draw_text_centered "Press SPACE to start" text_subtitle_y 20 Color.gray;
  end_drawing ()

let system_draw_game_over () =
  begin_drawing ();
  clear_background Color.black;
  draw_text_centered "GAME OVER" text_title_y 50 Color.red;
  draw_text_centered "Press SPACE to restart" text_subtitle_y 20 Color.gray;
  end_drawing ()

let system_draw_victory () =
  begin_drawing ();
  clear_background Color.black;
  draw_text_centered "YOU WIN!" text_title_y 50 Color.green;
  draw_text_centered "Press SPACE to restart" text_subtitle_y 20 Color.gray;
  end_drawing ()

let system_draw_play (world : world) =
  begin_drawing ();
  clear_background Color.black;

  let paddle = world.paddle in
  draw_rectangle (int_of_float paddle.x) (int_of_float paddle.y)
    (int_of_float paddle.width)
    (int_of_float paddle.height)
    Color.blue;

  let ball = world.ball in
  draw_circle (int_of_float ball.x) (int_of_float ball.y) ball.radius
    Color.white;

  let bricks = world.bricks in
  for index = 0 to bricks.count - 1 do
    draw_rectangle
      (int_of_float bricks.x.(index))
      (int_of_float bricks.y.(index))
      (int_of_float bricks.width)
      (int_of_float bricks.height)
      Color.red
  done;

  end_drawing ();
  world

let tick_playing frame_time (world : world) =
  world |> system_input frame_time
  |> system_integrate frame_time
  |> system_walls |> system_paddle |> system_bricks |> system_outcome
  |> system_draw_play |> ignore

let game_loop (world : world) =
  while not (window_should_close ()) do
    let frame_time = get_frame_time () in
    match world.screen with
    | Start ->
        system_draw_start ();
        if is_key_pressed Key.Space then world.screen <- Playing
    | Playing -> world |> tick_playing frame_time
    | GameOver ->
        system_draw_game_over ();
        if is_key_pressed Key.Space then world |> reset_play |> ignore
    | Victory ->
        system_draw_victory ();
        if is_key_pressed Key.Space then world |> reset_play |> ignore
  done;
  close_window ()

let setup () =
  init_window
    (int_of_float window_width)
    (int_of_float window_height)
    "Breakaml";
  set_target_fps 60;
  make_world ()

let () = setup () |> game_loop
