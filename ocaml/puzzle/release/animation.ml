
module type PUZZLE = sig
  include Solver.PUZZLE

  type animstate

  val init     : state     -> animstate
  val render   : animstate -> state -> (float*move) option -> unit
end

module Make (Puzzle : PUZZLE) = struct
  open Puzzle
  open Sdlevent
  open Sdlkey

  type state = {
    animstate : Puzzle.animstate;
    state     : Puzzle.state;
    moves     : Puzzle.move list;
    start     : float;
    dtime     : float;
  }

  let gettime () =
    float_of_int (Sdltimer.get_ticks ()) /. 1000.

  let rec update initial state =
    let now = gettime () in
    let dt  = now -. state.start in
    let state = {state with dtime = dt} in
    match state with
      | {moves=[]}    when dt > 3.0 -> {initial with start = now}
      | {moves=m::tl} when dt > 1.0 -> update initial {
                                         state with
                                         moves = tl;
                                         state = apply state.state m;
                                         start = state.start +. 1.0;
                                         dtime = dt;
                                       }
      | _ -> state
  
  let rec handle_events () = match Sdlevent.poll () with
    | Some QUIT                      -> false
    | Some KEYUP {keysym=KEY_ESCAPE} -> false
    | None                           -> true
    | _                              -> handle_events ()

  let curve f = 0.5 *. (1.0 -. cos (min (f *. 3.8) 3.1415))

  let rec main_loop initial state =
    Sdltimer.delay 10;

    let state  = update initial state in
    let offset = match state with
      | {moves=[]}             -> None
      | {moves=m::_; dtime=dt} -> Some (curve dt,m)
    in render state.animstate state.state offset;

    if handle_events () then main_loop initial state

  let run state moves =
    Sdl.init [`VIDEO];
    let animstate = init state in
    let state = {
      animstate = animstate;
      state = state;
      moves = moves;
      start = gettime ();
      dtime = 0.;
    } in
    main_loop state state;
    Sdl.quit ();

end

