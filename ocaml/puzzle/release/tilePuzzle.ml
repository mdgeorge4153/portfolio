(* This is the implementation of the tiling puzzle! *)


(** row, col *)
type pos  = int * int
type move = N | S | E | W
type tile = int

type state = {
  (* invariant: pieces is an nxn matrix containing the integers [0, n*n) *)
  pieces: tile array array;

  (* precomputed values *)
  blank:   pos; (* computed by find 0 pieces *)
  badness: int; (* computed by manhattan_distance pieces *)
}

let size  st    = Array.length st.pieces

let equal s1 s2 = s1.pieces = s2.pieces

let is_goal s   = s.badness = 0

let goodness s1 s2 =
  if      s1.badness < s2.badness then Util.Gt
  else if s1.badness = s2.badness then Util.Eq
  else (* s1.badness > s2.badness *)   Util.Lt

(****************************************************************************)
(* utilities ****************************************************************)
(****************************************************************************)

let lookup st (i,j) = st.pieces.(i).(j)
let int_of_pos s (i,j) = s*i + j
let pos_of_int s n     = n / s, n mod s

(** folds f over the array, passing the index as the first argument *)
let fold_arri f acc arr =
  snd (Array.fold_left (fun (i,a) n -> i + 1, f i a n) (0,acc) arr)

(** folds f over the matrix, passing the row and column as the first argument *)
let fold_mati f =
  fold_arri (fun i ->
    fold_arri (fun j ->
      f (i,j)
    )
  )

(** returns the index of n in a.  Fails if n is not in a *)
let find n a =
  match fold_mati (fun p a n' -> if n = n' then Some p else a) None a with
    | Some p -> p
    | None   -> failwith "no element found"

(****************************************************************************)
(** distance heuristic ******************************************************)
(****************************************************************************)

(** the sum over all the tiles of the (manhattan) distance from the current
    location to the desired location *)
let manhattan_distance pieces =
  let size = Array.length pieces in
  fold_mati (fun (i,j) a n -> let (i',j') = pos_of_int size n in
                                   let d = abs (i - i') + abs (j - j') in
                                   a + d
                 ) 0 pieces

(****************************************************************************)
(* creation *****************************************************************)
(****************************************************************************)

(* Creates a game board *)
let create pieces =
  {
    pieces  = pieces;
    blank   = find 0 pieces;
    badness = manhattan_distance pieces;
  }

let of_list n tiles =
  create (
    Array.init n (fun i ->
      Array.init n (fun j ->
        List.nth tiles (int_of_pos n (i,j))
      )
    )
  )

let goal n =
  create (
    Array.init n (fun i ->
      Array.init n (fun j ->
        int_of_pos n (i,j)
      )
    )
  )

(****************************************************************************)
(* transitions **************************************************************)
(****************************************************************************)

(* [offset pos dir] is the position found by starting at pos and moving in dir *)
let offset (r,c) = function
    N -> (r-1, c)
  | S -> (r+1, c)
  | E -> (r, c+1)
  | W -> (r, c-1)

(* valid_move p m  is whether the move m is valid on puzzle state p. *)
let valid_move p m = 
  let r',c' = offset p.blank m in
  let n     = size p in
  0 <= r' && r' < n && 0 <= c' && c' < n  

(* apply p m  is the puzzle that results when the blank space
   is swapped with the tile in the specified direction. If there
   is no tile in that direction, returns p. *)
let apply (p:state) (m:move) : state =
  if not (valid_move p m) then p
  else
    let n         = size p in
    let oldblank  = p.blank in
    let newblank  = offset oldblank m in
    let newpieces = Array.init n (fun i ->
                      Array.init n (fun j ->
                        if      (i,j) = oldblank then lookup p newblank
                        else if (i,j) = newblank then lookup p oldblank
                        else                          lookup p (i,j)
                      )
                    )
    in create newpieces

(* This allows us to generate a list of valid moves from a state *)
let moves (p: state) : move list = 
  List.filter (valid_move p) [N;E;S;W]

(****************************************************************************)
(** animation ***************************************************************)
(****************************************************************************)

open Sdlvideo

type animstate = {
  screen    : surface;
  tiles     : surface array array;
  tile_size : int;
  bgcolor   : int32;
}

(* misc SDL junk ************************************************************)

let align r1 r2 =
  let dx = r1.r_w - r2.r_w in
  let dy = r1.r_h - r2.r_h in
  {r2 with r_x = r1.r_x + (dx/2); r_y = r1.r_y + (dy/2)}

let surface_rect surf =
  let (w,h,_) = surface_dims surf in
  {r_x = 0; r_y = 0; r_w = w; r_h = h}

let create_surface w h =
  (* just copy a bunch of junk from get_video_format *)
  let {bits_pp=bpp; rmask=rmask; gmask=gmask; bmask=bmask; amask=amask} =
    get_video_info_format ()
  in
  create_RGB_surface [] ~w:w ~h:h ~bpp:bpp
                        ~rmask:rmask ~gmask:gmask ~bmask:bmask ~amask:amask

let lookup n a = a.(n / 3).(n mod 3)

let round f =
  int_of_float f +
  if f -. floor f > 0.5 then 1 else 0

(* initialization ***********************************************************)

(** returns an nxn array of surfaces, and the size of each surface *)
let mknumbers size =
  Sdlttf.init ();
  let font     = Sdlttf.open_font "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf" 20 in
  let surf i j = Sdlttf.render_text_blended font (string_of_int (size * i + j)) black in
  let tiles    = Array.init size (fun i ->
                   Array.init size (fun j ->
                     surf i j
                   )
                 ) in
  Sdlttf.quit ();
  tiles, 100


(** returns an nxn array of surfaces, and the size of each surface *)
let mkpics size =
  (* TODO: fallback to mknumbers if failed to load *)
  let source    = Sdlloader.load_image "zardoz.png" in
  let w, h, _   = surface_dims source in
  let tile_size = min w h / size in
  let rect      = {r_x = 0; r_y = 0; r_w = tile_size; r_h = tile_size} in
  Array.init size (fun i ->
    Array.init size (fun j ->
      let tile = create_surface tile_size tile_size in
      blit_surface ~src:source
                   ~src_rect:{rect with r_y = tile_size * i; r_x = tile_size * j}
                   ~dst:tile
                   ~dst_rect:rect
                   ();
      tile
    )
  ), tile_size

(* see animation.mli *)
let init board =
  Sdlwm.set_caption ~title:"SteamTiles" ~icon:"";

  let size = size board in
  let tiles, tile_size = mkpics size in
  let total = size * tile_size in
  {
    screen    = set_video_mode total total [`DOUBLEBUF];
    tiles     = tiles;
    tile_size = tile_size;
    bgcolor   = 0xffffffffl;
  }

(* drawing ********************************************************************)

(* return the rect for the given position [p], optionally offset towards [p'] *)
let rect tile_size p1 f p2 =
  let interp (i,j) f (i',j') =
    let interp a b = round (float_of_int b *. f +.
                            float_of_int a *. (1. -. f)) in
    interp i i', interp j j'
  in

  let scale (i,j) = tile_size * i, tile_size * j in
  let i,j = interp (scale p1) f (scale p2)
  in
  { r_x = j; r_w = tile_size; r_y = i; r_h = tile_size }

let draw_tile sdl t rect =
  let ssurf = lookup t sdl.tiles in
  let srect = surface_rect ssurf in
  let drect = align rect srect in
  blit_surface ~src:ssurf ~src_rect:srect ~dst:sdl.screen ~dst_rect:drect ()

(* see animation.mli *)
let render sdl board move =
  Sdlvideo.fill_rect sdl.screen sdl.bgcolor;

  let blank = board.blank in
  fold_mati begin fun pos _ t ->
    match move with
      | _ when pos = blank
          -> ()
      | Some (f,m) when pos = offset blank m
          -> draw_tile sdl t (rect sdl.tile_size pos f  blank)
      | _ -> draw_tile sdl t (rect sdl.tile_size pos 0. pos)
  end () board.pieces;

  Sdlvideo.flip sdl.screen;
  ()

