(******************************************************************************)
(** Representing the cube *****************************************************)
(******************************************************************************)

type move = U | D | L | R | F | B

type side  = move
type color = side

type face  = color * color * color * color
type state = face * face * face * face * face * face

let moves state = [U; R; F]
let equal st1 st2 = st1 = st2

let all c = (c,c,c,c)
let goal : state = (all U, all D, all L, all R, all F, all B)

let is_goal s = s = goal

(******************************************************************************)
(** Applying moves ************************************************************)
(******************************************************************************)

type rot = RFU | RLF

let side_to_up move = match move with
  | U -> []
  | D -> [RFU; RFU]
  | L -> [RLF; RFU]
  | R -> [RLF; RLF; RLF; RFU]
  | F -> [RFU]
  | B -> [RLF; RLF; RFU]

let up_to_side move = match move with
  | U -> []
  | D -> [RFU; RFU]
  | L -> [RFU; RFU; RFU; RLF; RLF; RLF]
  | R -> [RFU; RFU; RFU; RLF]
  | F -> [RFU; RFU; RFU]
  | B -> [RFU; RFU; RFU; RLF; RLF]
  
let rotate_cw  ((tl, tr, bl, br):face) : face = (bl, tl, br, tr)
let rotate_ccw ((tl, tr, bl, br):face) : face = (tr, br, tl, bl)
let rotate2    face = rotate_cw (rotate_cw face)

let ucw ((u, d, (ltl, ltr, lbl, lbr),
                (rtl, rtr, rbl, rbr),
                (ftl, ftr, fbl, fbr),
                (btl, btr, bbl, bbr)):state) : state =
        (rotate_cw u, d,
                (ftl, ftr, lbl, lbr),
                (btl, btr, rbl, rbr),
                (rtl, rtr, fbl, fbr),
                (ltl, ltr, bbl, bbr))

let rec shuffle rots (u, d, l, r, f, b) = match rots with
  | RFU::tl -> shuffle tl (f, rotate2 b, rotate_ccw l, rotate_cw r, d, rotate2 u)
  | RLF::tl -> shuffle tl (rotate_ccw u, rotate_cw d, b, f, l, r)
  | []      -> (u, d, l, r, f, b)

let apply state move =
  let state = shuffle (side_to_up move) state in
  let state = ucw state in
  let state = shuffle (up_to_side move) state in
  state

(******************************************************************************)
(** Goodness heuristic ********************************************************)
(******************************************************************************)

(* First attempt:
 *
 * we break the cube up into 8 "cubies": each combination of up/down, left/right
 * and back/front.
 *
 * For each cubie position, we determine which cubie is actually in that
 * position.  We then take the manhattan distance between these.  For example,
 * if the ULF position contains DLB, then that cubie adds 2 to the badness.
 *
 * In addition, we account for rotation, by adding one to each cubie if it is
 * not rotated correctly.
 *
 * We return a very large badness for invalid cubies.  These shouldn't occur.
 *)

type pos = ULF | ULB | URF | URB | DLF | DLB | DRF | DRB

let cubie pos
          ((utl, utr, ubl, ubr),
           (dtl, dtr, dbl, dbr),
           (ltl, ltr, lbl, lbr),
           (rtl, rtr, rbl, rbr),
           (ftl, ftr, fbl, fbr),
           (btl, btr, bbl, bbr)) =
  match pos with
    | ULF -> [ubl; ltr; ftl]
    | ULB -> [utl; ltl; btr]
    | URF -> [ubr; rtl; ftr]
    | URB -> [utr; rtr; btl]
    | DLF -> [dtl; lbr; fbl]
    | DLB -> [dbl; lbl; bbr]
    | DRF -> [dtr; rbl; fbr]
    | DRB -> [dbr; rbr; bbl]

(* returns the distance between the actual and expected cubie as described above *)
let cubie_penalty actual expected =
  let dir (a,b) cubie = List.fold_left (fun acc x ->
      if x = a || x = b then Some x else acc
    ) None cubie
  in

  if actual = expected then 0
  else List.fold_left (fun sum axis ->
    sum + match dir axis actual, dir axis expected with
      | _,      None              -> failwith "this shouldn't happen"
      | None,   _                 -> 1000 (* malformed cubie *)
      | Some a, Some e when a = e -> 0
      | _                         -> 1
    ) 1 [(U,D); (L,R); (F,B)]
 

(* returns the total penalty for the whole cube. *)
let cube_penalty cube = List.fold_left (fun sum pos ->
    sum + cubie_penalty (cubie pos cube) (cubie pos goal)
  ) 0 [ULF; ULB; URF; URB; DLF; DLB; DRF; DRB]


let goodness_1 s1 s2 =
  let open Util in
  let p1, p2 = cube_penalty s1, cube_penalty s2 in
  if p1 < p2 then Gt
  else if p1 > p2 then Lt
  else Eq


(* Second attempt: just take the number of squares in the wrong place *)

(* note: this is pretty ugly *)
let fold4 f (a,b,c,d)     acc = f a (f b (f c (f d acc)))
let fold6 g (a,b,c,d,e,f) acc = g a (g b (fold4 g (c,d,e,f) acc))

let fold4_2 f (a1,b1,c1,d1)
              (a2,b2,c2,d2)
              acc = f a1 a2 (f b1 b2 (f c1 c2 (f d1 d2 acc)))
let fold6_2 f (a1,b1,c1,d1,e1,f1)
              (a2,b2,c2,d2,e2,f2)
              acc = f a1 a2 (f b1 b2 (fold4_2 f (c1,d1,e1,f1) (c2,d2,e2,f2) acc))

let fold f   = fold6 (fold4 f)
let fold_2 f = fold6_2 (fold4_2 f)

let penalty cube = fold_2 (fun a b acc -> if a = b then acc else 1 + acc) cube goal 0

let goodness_2 s1 s2 =
  let open Util in
  let p1, p2 = penalty s1, penalty s2 in
  if p1 < p2 then Gt
  else if p1 = p2 then Eq
  else Lt

(* Third attempt: KISS *)

let goodness_3 s1 s2 = Util.Eq

let goodness = goodness_3

(******************************************************************************)
(** Drawing the cube **********************************************************)
(******************************************************************************)

let color side = match side with
  | U -> (1.0, 0.0, 0.0)
  | D -> (0.0, 1.0, 0.0)
  | L -> (0.0, 0.0, 1.0)
  | R -> (0.0, 1.0, 1.0)
  | F -> (1.0, 0.0, 1.0)
  | B -> (1.0, 1.0, 0.0)

(* given [R1; R2; ...] multiply the gl matrix by R1 R2 ... *)
let rec transform rots = match rots with
  | []      -> ()
  | RLF::tl -> transform tl;
               GlMat.rotate ~angle:90. ~y:1. ();
  | RFU::tl -> transform tl;
               GlMat.rotate ~angle:(-90.) ~x:1. ()

type animstate = unit

(* see animation.mli *)
let render () cube offset =
  let percent,move = match offset with
    | None   -> 0., U
    | Some o -> o
  in

  let draw_square c x z =
    GlDraw.begins `quads;
    GlDraw.color (color c);
    GlDraw.vertex ~x:(0.05*.x) ~y:1. ~z:(0.05*.z) ();
    GlDraw.vertex ~x:(0.05*.x) ~y:1. ~z:(0.95*.z) ();
    GlDraw.vertex ~x:(0.95*.x) ~y:1. ~z:(0.95*.z) ();
    GlDraw.vertex ~x:(0.95*.x) ~y:1. ~z:(0.05*.z) ();
    GlDraw.ends ();
    ()
  in

  let draw_top which (tl, tr, _, _) =
    GlMat.push ();
    transform (up_to_side which);
    draw_square tl (-1.0) (-1.0);
    draw_square tr ( 1.0) (-1.0);
    GlMat.pop ();
  in

  let draw_bot which (_, _, bl, br) =
    GlMat.push ();
    transform (up_to_side which);
    draw_square bl (-1.) (1.);
    draw_square br ( 1.) (1.);
    GlMat.pop ();
  in

  let draw (u,d,l,r,f,b) =
    (* orientation
    GlDraw.point_size 10.;
    GlDraw.begins `points;
    List.iter (fun v -> GlDraw.color v; GlDraw.vertex3 v)
              [0.,0.,0.; 1.,0.,0.; 0.,1.,0.; 0.,0.,1.];
    GlDraw.ends ();
    Glut.wireCube 2.;
    *)

    (* unrotated bottom *)
    draw_top D d; draw_bot D d;
    draw_bot L l; draw_bot R r;
    draw_bot F f; draw_bot B b;

    (* rotated top *)
    GlMat.rotate ~angle:(percent *. 90.) ~y:(-1.) ();
    draw_bot U u; draw_top U u;
    draw_top L l; draw_top R r;
    draw_top F f; draw_top B b;
  in

  GlClear.clear [`color; `depth];
  GlMat.push ();
  transform (up_to_side move);
  draw      (shuffle (side_to_up move) cube);
  (*
  draw cube;
  *)
  GlMat.pop ();
  Sdlgl.swap_buffers ();
  ()

(******************************************************************************)
(** Initialization ************************************************************)
(******************************************************************************)

let reshape ~w ~h =
  GlDraw.viewport 0 0 w h;
  GlMat.mode `projection;
  GlMat.load_identity ();
  GluMat.perspective ~fovy:60. ~aspect:1. ~z:(1.5,20.);
  GlMat.mode `modelview;
  ()

(* see animation.mli *)
let init _ =
  Sdlwm.set_caption ~title:"SteamCube" ~icon:"";
  ignore(Glut.init [| |]);
  ignore(Sdlvideo.set_video_mode 500 500 [`OPENGL]);
  reshape ~w:500 ~h:500;

  Gl.enable `depth_test;
  GlClear.color (1.0, 1.0, 1.0);
  GlMat.load_identity ();
  GluMat.look_at ~eye:(2.,2.,5.) ~center:(0.,0.,0.) ~up:(0.,1.,0.);
  ()

