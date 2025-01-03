Graphics.open_graph " 1920x1080"

type point = { abs : float ; ord : float }
type vector = { x : float ; y : float }
type segment = { p1 : point ; p2 : point }

let make_vector (a : point) (b : point) : vector =
  { x = b.abs -. a.abs; y = b.ord -. a.ord };;

let mul_scal (s : float) (v : vector) : vector =
  { x = s *. v.x; y = s *. v.y };;

let rotate_vector (v : vector) (theta : float) : vector =
  let cos_theta = cos theta in
  let sin_theta = sin theta in
  { x = v.x *. cos_theta -. v.y *. sin_theta;
    y = v.x *. sin_theta +. v.y *. cos_theta };;

let translate_point (p : point) (v : vector) : point =
  { abs = p.abs +. v.x; ord = p.ord +. v.y };;

let draw_segment (s : segment) : unit =
  Graphics.moveto s.p1.abs s.p1.ord;
  Graphics.lineto s.p2.abs s.p2.ord;;

let process_segment (s : segment) (theta : float) : segment * segment =
  (* Calcul du vecteur direction du segment *)
  let v = make_vector s.p1 s.p2 in

  (* Calcul du vecteur perpendiculaire à v, pour le carré et le triangle rectangle *)
  let v_perp = rotate_vector v (Float.pi /. 2.) in

  (* Calcul des points pour le carré *)
  let p3 = translate_point s.p2 v_perp in
  let p4 = translate_point s.p1 v_perp in
  let square_segment1 = { p1 = s.p1; p2 = p4 } in
  let square_segment2 = { p1 = s.p2; p2 = p3 } in

  (* Calcul du triangle rectangle *)
  let p5 = translate_point s.p1 v_perp in
  let triangle_segment1 = { p1 = s.p1; p2 = p5 } in
  let triangle_segment2 = { p1 = s.p2; p2 = p5 } in

  (* Dessiner le carré et le triangle *)
  draw_segment square_segment1;
  draw_segment square_segment2;
  draw_segment triangle_segment1;
  draw_segment triangle_segment2;

  (* Retourner les segments du carré et du triangle *)
  (square_segment1, square_segment2);;


let _ = Graphics.wait_next_event [Key_pressed] in Graphics.close_graph ();;
