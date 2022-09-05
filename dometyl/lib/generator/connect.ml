open! Scad_ml
open! Syntax

type t =
  { scad : Scad.d3
  ; outline : Path3.t
  ; inline : Path3.t
  }
[@@deriving scad]

(* TODO:
   - break bezier spline and linear connectors into separate functions, and the
    work that they share into a higher level helper. Another router function
    can handle deciding which one to use.
   - better angle logic for the linear connector (better knobs for how to do
    rotation stepping, and handling the >90degrees cases)z
   - elbow bezier case (subcase of bezier spline?), largely as an option
    solution for the southern pinky to ring connection *)

let clockwise_union ts =
  let collect ~init line = List.fold_left (fun ps p -> p :: ps) init line in
  let f (scads, out_pts, in_pts) { scad; outline; inline } =
    scad :: scads, collect ~init:out_pts outline, collect ~init:in_pts inline
  in
  let scads, out_pts, in_pts = List.fold_left f ([], [], []) ts in
  { scad = Scad.union3 scads; outline = List.rev out_pts; inline = List.rev in_pts }

let outline_2d t = Path3.to_path2 t.outline
let inline_2d t = Path3.to_path2 t.inline

(* true if connection between w1 and w2 moves away from centre of mass *)
let is_outward (w1 : Wall.t) (w2 : Wall.t) =
  V3.distance w1.foot.top_right w2.foot.top_left
  > V3.distance w1.foot.top_right w2.foot.bot_left

let outward_sign (w1 : Wall.t) (w2 : Wall.t) =
  V3.clockwise_sign w1.foot.top_left w1.foot.top_right w2.foot.top_left

(* let is_outward (w1 : Wall.t) (w2 : Wall.t) = *)
(*   V2.ccw_theta V3.(to_v2 @@ (w1.foot.top_right -@ w2.foot.top_left)) *)
(*   *. V3.angle (Wall.foot_direction w1) (Wall.foot_direction w2) *)
(*   > 0. *)

(* let facet_points ?(rev = false) ?(n_facets = 1) ?(init = []) ~height bez = *)
(*   let step = height /. Float.of_int n_facets *)
(*   and start, continue, next = *)
(*     if rev then 1, ( >= ) n_facets, ( + ) 1 else n_facets, ( < ) 0, ( + ) (-1) *)
(*   in *)
(*   let rec loop ps i = *)
(*     if continue i *)
(*     then loop (Wall.Edge.point_at_z bez (Float.of_int i *. step) :: ps) (next i) *)
(*     else ps *)
(*   in *)
(*   loop init start *)

(* let endpoints ?n_facets ~height top top_bez bot_bez bot = *)
(*   top *)
(*   :: facet_points *)
(*        ?n_facets *)
(*        ~height *)
(*        ~init:(facet_points ~rev:true ?n_facets ~height ~init:[ bot ] bot_bez) *)
(*        top_bez *)

(* let base_endpoints ?(n_facets = 1) ~height hand (w : Wall.t) = *)
(*   let top, bot = *)
(*     match hand with *)
(*     | `Left -> `TL, `BL *)
(*     | `Right -> `TR, `BR *)
(*   in *)
(*   endpoints *)
(*     ~n_facets *)
(*     ~height *)
(*     (Points.get w.foot top) *)
(*     (Wall.Edges.get w.edges top) *)
(*     (Wall.Edges.get w.edges bot) *)
(*     (Points.get w.foot bot) *)

(* let base_steps ~n_steps starts dests = *)
(*   let norms = List.map2 V3.distance starts dests in *)
(*   let lowest_norm = List.fold_left ~init:Float.max_value Float.min norms in *)
(*   let adjust norm = Float.(to_int (norm /. lowest_norm *. of_int n_steps)) in *)
(*   `Ragged (List.map adjust norms) *)

(* Apply the roundover corner to the two topmost points of the connector drawn
   by bot_edge and top_edge. Edges must have the same length. *)
let rounder ?fn ~corner bot_edge top_edge =
  (* TODO: ability to autocalculate cut/joint based on
      the distance between the upper (first) points of bot' and top'?
    (corner as a poly variant e.g. [`AutoBez | `AutoChamf | `Corner of corner]) *)
  let corners = Some corner :: List.init (List.length bot_edge - 1) (Fun.const None) in
  let zip a b = List.map2 (fun a b -> a, b) a b in
  List.rev_append (zip bot_edge corners) (zip top_edge corners)
  |> Path3.Round.mix
  |> Path3.roundover ?fn

let fillet ?(style = `Scale) ~d ~h rows =
  let rel_dists, total_dist =
    let f (dists, sum, last) row =
      let p = V3.(mean row *@ v 1. 1. 0.) in
      let sum = sum +. V3.distance p last in
      sum :: dists, sum, p
    and start = V3.(mean (List.hd rows) *@ v 1. 1. 0.) in
    let dists, sum, _ = List.fold_left f ([ 0. ], 0., start) (List.tl rows) in
    List.rev_map (fun d -> d /. sum) dists, sum
  in
  let ez =
    let d =
      match d with
      | `Rel d -> d
      | `Abs d -> d /. total_dist
    in
    Easing.make (v2 d 1.) (v2 d 1.)
  in
  let f =
    match style with
    | `Scale ->
      let h =
        match h with
        | `Rel h -> h
        | `Abs h -> h /. (Path3.bbox (List.hd rows)).max.z
      in
      fun u row ->
        let u = if u > 0.5 then 1. -. u else u in
        Path3.scale (v3 1. 1. (1. -. (ez u *. h))) row
    | `Shift ->
      let range =
        match h with
        | `Rel h -> (Path3.bbox (List.hd rows)).max.z *. h
        | `Abs h -> h
      in
      fun u row ->
        let u = if u > 0.5 then 1. -. u else u in
        let diff = ez u *. range in
        let shift p = if p.z > diff +. 0.1 then V3.ztrans (-.diff) p else p in
        List.map shift row
  in
  List.map2 f rel_dists rows

let id = ref 0

let spline_base ?(height = 11.) ?(n_steps = 6) (w1 : Wall.t) (w2 : Wall.t) =
  let dir1 = Wall.foot_direction w1
  and dir2 = Wall.foot_direction w2 in
  let end_edges ?frac left =
    let edger (w : Wall.t) corner =
      let edge = w.drawer corner in
      let u =
        Path3.continuous_closest_point
          (Path3.to_continuous edge)
          (V3.add (Util.last edge) (v3 0. 0. height))
      in
      snd @@ Path3.split ~distance:(`Rel u) edge
    in
    let w, dir =
      if left then w1, V3.neg @@ Wall.foot_direction w1 else w2, Wall.foot_direction w2
    in
    let b_loc, t_loc =
      match left, frac with
      | true, None -> `B 0.99, `T 0.99 (* fudging for union *)
      | false, None -> `B 0.01, `T 0.01
      | true, Some frac -> `XY (1. -. frac, 0.05), `XY (1. -. frac, 0.95)
      | false, Some frac -> `XY (frac, 0.05), `XY (frac, 0.95)
    in
    let bot = edger w b_loc
    and top = edger w t_loc in
    let plane = Plane.of_normal ~point:(Util.last bot) dir in
    let dedup =
      let proj = V3.project plane in
      (* TODO: expose as max connection resolution? *)
      let eq a b = V2.approx ~eps:2. (proj a) (proj b) in
      Path3.deduplicate_consecutive ~keep:`FirstAndEnds ~eq
    in
    plane, dedup bot, dedup top
  in
  let plane_l, bot_l, top_l = end_edges true
  and plane_r, bot_r, top_r = end_edges false in
  let edge_len =
    let l = Int.max (List.length bot_l) (List.length top_l)
    and r = Int.max (List.length bot_r) (List.length top_r) in
    Int.max l r
  in
  let subdiv = Path3.subdivide ~freq:(`N (edge_len, `ByLen)) in
  let bot_l, top_l, bot_r, top_r =
    subdiv bot_l, subdiv top_l, subdiv bot_r, subdiv top_r
  in
  let planer plane bot top =
    let f (d, acc) p =
      let p' = Plane.lift plane (Plane.project plane p) in
      Float.max (Plane.distance_to_point plane p) d, p' :: acc
    in
    let d, top' = List.fold_left f (0., []) top in
    let d, bot' = List.fold_left f (d, []) bot in
    (* NOTE: 0.3 feels like a pretty large fudge, but I have seen issues with
        lower, how often do those cases actually come up, I have had it working
        with 0.05 before running into a situation that seemed to call for 0.3.
        Another param to expose? *)
    let f = V3.(translate (Plane.normal plane *$ (d +. 0.3))) in
    List.rev_map f bot', List.rev_map f top'
  in
  let bot_l', top_l' = planer plane_l bot_l top_l
  and bot_r', top_r' = planer plane_r bot_r top_r in
  let corner = Path3.Round.(chamf (`Joint 0.7)) in
  (* let corner = Path3.Round.(bez (`Joint 1.)) in *)
  let a = rounder ~fn:16 ~corner bot_l top_l
  and a' = rounder ~fn:16 ~corner bot_l' top_l'
  and b = rounder ~fn:16 ~corner bot_r top_r
  and b' = rounder ~fn:16 ~corner bot_r' top_r' in
  let base_points = function
    | inner :: pts -> inner, Util.last pts
    | [] -> failwith "unreachable"
  in
  let shrink_edges prof =
    let m = V3.(mean prof *@ v3 1. 1. 0.)
    and s = v3 0.8 0.8 1. in
    List.map (fun p -> V3.(((p -@ m) *@ s) +@ m)) prof
  in
  let a = shrink_edges a
  and b = shrink_edges b in
  let p0 = V3.(mean a' *@ v3 1. 1. 0.)
  and p3 = V3.(mean b' *@ v3 1. 1. 0.) in
  let a' = Path3.translate (V3.neg p0) a'
  and b' = Path3.translate (V3.neg p3) b' in
  let align_q = Quaternion.align dir2 dir1 in
  let b' = Path3.quaternion align_q b' in
  (* FIXME: is_outward scheme does not work for elbow corners like on west of
    thumb, or on full bows like the pinky N-S connection. For the elbow and bow
    conditions, the rotation angle must be opposite for the linear skin
    profiles as well. *)
  (* TODO: try using the angle between the dir vectors to flag whether the wall
   relationship calls for elbow/bow (issue mentioned above). I think that it's
    greater than (pi / 2) reliably when these special cases arise.  *)
  let ang =
    (* account for whether this connection moves away from centre of mass
              (positive) or inward (negative) to set angle  polarity  *)
    (* let sign = if is_outward w1 w2 then 1. else -1. in *)
    let sign = outward_sign w1 w2 in
    V3.angle (V3.neg dir1) V3.(p3 -@ p0) *. sign
  in
  let n_steps =
    ignore n_steps;
    (* 32 *)
    64
  in
  let out = 1.5 in
  (* let min_spline_dist = 15. in *)
  let min_spline_dist = out *. 4. in
  let d = V2.(distance (v p0.x p0.y) (v p3.x p3.y)) in
  let min_spline_ratio = 6. in
  let ang_ratio = d /. Float.abs ang /. Float.pi in
  Printf.printf "id %i: ang = %f; ratio = %f\n" !id ang ang_ratio;
  Printf.printf "dir angle = %f\n" (V3.angle dir1 dir2);
  Printf.printf
    "3pt angle = %f\n"
    (V3.angle_points w1.foot.top_left w1.foot.top_right w2.foot.top_left);
  Printf.printf
    "3pt cw sign = %f\n"
    (V3.clockwise_sign w1.foot.top_left w1.foot.top_right w2.foot.top_left);
  Printf.printf "outward sign = %f\n" (if is_outward w1 w2 then 1. else -1.);
  let step = 1. /. Float.of_int n_steps in
  let fillet_d = `Rel 0.2 in
  let fillet_h = `Rel 0.3 in
  (* let fillet_h = 0. in *)
  let transition i = List.map2 (fun a b -> V3.lerp a b (Float.of_int i *. step)) a' b' in
  (* TODO: experiment with automatic "out" d adjustment as well as size as
    mentioned above. I think I can remove the need for most uses of the
    linear/rot connectors this way and make it more of an option rather than a
     necessity. The auto function could optionally take bow params to only be
    used when one is encountered for example (e.g.
     - decrease out d when harsh angles or short distances are detected
     - increasing d, when bowing is detected
      *)
  (* NOTE: try minimal out shift for tight connections, and a separate
        parameter for more extreme foot direction angles (over 90 deg)? *)
  let dir_angle = V3.(angle dir1 dir2) in
  let out =
    if d < min_spline_dist || ang_ratio < min_spline_ratio
    then 0.1
    else if dir_angle > 3.
    then
      out
      (* else if dir_angle > Float.pi /. 2. *)
      (* then 2. *)
    else out
  in
  (* TODO: what about trying a range of out/size values and keeping the one that
    drops the least? *)
  let transforms =
    let path =
      let p1 = V3.(p0 +@ (dir1 *$ -.out))
      and p2 = V3.(p3 +@ (dir2 *$ out)) in
      (* TODO: related to above, the size param of of_path may be useful for
           automatic mesh intersection avoidance (e.x. allow more "S" shape when tight). *)
      Bezier3.curve ~fn:n_steps
      @@ Bezier3.of_path ~size:(`Abs [ out *. 0.1; 8.; out *. 0.1 ]) [ p0; p1; p2; p3 ]
    in
    Util.prune_transforms ~shape:transition @@ Path3.to_transforms ~mode:`NoAlign path
  in
  let in_start', out_start' = base_points a' in
  let mesh =
    let rows =
      List.map (fun (i, m) -> Path3.affine m (transition i)) transforms
      |> fillet ~style:`Scale ~d:fillet_d ~h:fillet_h
    in
    let last_row = Util.last rows in
    let b =
      let d =
        let true_b' = Path3.quaternion (Quaternion.conj align_q) b' in
        let end_plane = Path3.to_plane (Path3.translate p3 true_b') in
        let f (mn, mx) p =
          let d = Plane.distance_to_point end_plane p in
          Float.min mn d, Float.max mx d
        in
        let below, above = List.fold_left f (Float.max_float, Float.min_float) last_row in
        (* compute how angled the last row is relative to the end_plane, and
            use that slope to scale how much the wall profile should be slid
            away to avoid self intersection during the linear skin transition *)
        let slope =
          let bot, top = base_points last_row in
          (above -. below) /. V3.distance bot top
        in
        above -. (below *. slope)
      in
      let rel = d /. V3.distance w2.foot.top_left w2.foot.top_right in
      let _, bot, top = end_edges ~frac:(Float.min 0.99 rel) false in
      rounder ~fn:16 ~corner (subdiv bot) (subdiv top)
    in
    Mesh.of_rows ~style:`MinEdge
    @@ List.concat
         [ Mesh.slice_profiles ~slices:(`Flat 5) [ a; List.hd rows ]
         ; rows
         ; List.tl @@ Mesh.slice_profiles ~slices:(`Flat 5) [ last_row; b ]
         ]
  and inline = List.map (fun (_, m) -> V3.affine m in_start') transforms
  and outline = List.map (fun (_, m) -> V3.affine m out_start') transforms in
  let () =
    let show = Path3.show_points (Fun.const (Scad.sphere 0.4)) >> Scad.color Color.Red in
    Scad.union [ Mesh.to_scad mesh; show a; show b ]
    |> Scad.to_file (Printf.sprintf "conn_%i.scad" !id)
  in
  id := !id + 1;
  { scad = Mesh.to_scad mesh; inline; outline }

(* let bez_base ?(n_facets = 1) ?(height = 11.) ?(n_steps = 6) (w1 : Wall.t) (w2 : Wall.t) = *)
(*   let ({ x = dx; y = dy; z = _ } as dir1) = Wall.foot_direction w1 *)
(*   and dir2 = Wall.foot_direction w2 in *)
(*   let mask = if (abs dx > abs dy) then v3 1. 0. 0. else v3 0. 1. 0. in *)
(*   let get_bez start dest = *)
(*     let diff = V3.(dest -@ start) in *)
(*     let p1 = V3.(start +@ mul dir1 (v3 0.01 0.01 0.)) (\* fudge for union *\) *)
(*     and p2 = V3.(start +@ mul mask diff) *)
(*     and p3 = V3.(dest -@ mul dir2 (v3 0.01 0.01 0.)) in *)
(*     Bezier3.make [ p1; p2; p3 ] *)
(*   in *)
(*   let starts = base_endpoints ~n_facets ~height `Right w1 in *)
(*   let dests = base_endpoints ~n_facets ~height `Left w2 in *)
(*   let steps = base_steps ~n_steps starts dests *)
(*   and bezs = List.map2 get_bez starts dests in *)
(*   prism_connection bezs steps *)

(* let cubic_base *)
(*     ?(n_facets = 1) *)
(*     ?(height = 10.) *)
(*     ?(scale = 1.1) *)
(*     ?(d = 2.) *)
(*     ?(n_steps = 10) *)
(*     ?(bow_out = true) *)
(*     (w1 : Wall.t) *)
(*     (w2 : Wall.t) *)
(*   = *)
(*   let dir1 = Wall.foot_direction w1 *)
(*   and dir2 = Wall.foot_direction w2 *)
(*   and dist = v3 d d 0. *)
(*   and width = V3.distance w1.foot.top_right w1.foot.bot_right *. scale in *)
(*   let get_bez top start dest = *)
(*     let outward = *)
(*       if Bool.equal top bow_out then V3.add (v3 width width 0.) dist else dist *)
(*     in *)
(*     let p1 = V3.(start +@ mul dir1 (v3 0.01 0.01 0.)) (\* fudge for union *\) *)
(*     and p2 = V3.(start -@ mul dir1 outward) *)
(*     and p3 = V3.(dest +@ mul dir2 outward) *)
(*     and p4 = V3.(dest -@ mul dir2 (v3 0.01 0.01 0.)) in *)
(*     Bezier3.make [ p1; p2; p3; p4 ] *)
(*   in *)
(*   let starts = base_endpoints ~n_facets ~height `Right w1 in *)
(*   let dests = base_endpoints ~n_facets ~height `Left w2 in *)
(*   let steps = base_steps ~n_steps starts dests *)
(*   and bezs = *)
(*     List.fold2 *)
(*       ~init:(0, []) *)
(*       (fun (i, bs) s d -> i + 1, get_bez (i <= n_facets) s d :: bs) *)
(*       starts *)
(*       dests *)
(*     |> snd *)
(*     |> List.rev *)
(*   in *)
(*   prism_connection bezs steps *)

(* let snake_base *)
(*     ?(n_facets = 1) *)
(*     ?(height = 11.) *)
(*     ?(scale = 1.5) *)
(*     ?(d = 2.) *)
(*     ?(n_steps = 12) *)
(*     (w1 : Wall.t) *)
(*     (w2 : Wall.t) *)
(*   = *)
(*   let dir1 = Wall.foot_direction w1 *)
(*   and dir2 = Wall.foot_direction w2 *)
(*   and dist = v3 d d 0. *)
(*   and width = V3.distance w1.foot.top_right w1.foot.bot_right *. scale in *)
(*   let get_bez top start dest = *)
(*     let outward = V3.add (v3 width width 0.) dist in *)
(*     let p1 = V3.(start +@ mul dir1 (v3 0.01 0.01 0.)) (\* fudge for union *\) *)
(*     and p2 = V3.(start -@ mul dir1 (if top then dist else outward)) *)
(*     and p3 = V3.(dest +@ mul dir2 (if top then outward else dist)) *)
(*     and p4 = V3.(dest -@ mul dir2 (v3 0.01 0.01 0.)) in *)
(*     Bezier3.make [ p1; p2; p3; p4 ] *)
(*   in *)
(*   let starts = base_endpoints ~n_facets ~height `Right w1 in *)
(*   let dests = base_endpoints ~n_facets ~height `Left w2 in *)
(*   let steps = base_steps ~n_steps starts dests *)
(*   and bezs = *)
(*     List.fold2 *)
(*       ~init:(0, []) *)
(*       (fun (i, bs) s d -> i + 1, get_bez (i <= n_facets) s d :: bs) *)
(*       starts *)
(*       dests *)
(*     |> snd *)
(*     |> List.rev *)
(*   in *)
(*   prism_connection bezs steps *)

(* let inward_elbow_base *)
(*     ?(n_facets = 1) *)
(*     ?(height = 11.) *)
(*     ?(n_steps = 6) *)
(*     ?(d = 0.) *)
(*     (w1 : Wall.t) *)
(*     (w2 : Wall.t) *)
(*   = *)
(*   (\* Quad bezier, but starting from the bottom (inside face) of the wall and *)
(*    * projecting inward. This is so similar to bez_base that some generalization may *)
(*    * be possible to spare the duplication. Perhaps an option of whether the start is *)
(*    * the inward face (on the right) or the usual CW facing right side. *\) *)
(*   let dir1 = Wall.foot_direction w1 *)
(*   and dir2 = Wall.foot_direction w2 *)
(*   and ({ x = inx; y = iny; z = _ } as inward_dir) = *)
(*     V3.normalize V3.(w1.foot.bot_right -@ w1.foot.top_right) *)
(*   in *)
(*   let mask = if (abs inx > abs iny) then v3 1. 0. 0. else v3 0. 1. 0. in *)
(*   let get_bez start dest = *)
(*     let diff = V3.(dest -@ start) in *)
(*     let p1 = V3.(start -@ mul inward_dir (v3 0.01 0.01 0.)) (\* fudge for union *\) *)
(*     and p2 = V3.(start +@ mul mask diff +@ map (( *. ) d) dir2) *)
(*     and p3 = V3.(dest -@ mul dir2 (v3 0.01 0.01 0.)) in *)
(*     Bezier3.make [ p1; p2; p3 ] *)
(*   in *)
(*   let starts = *)
(*     let w = V3.distance w1.foot.bot_right w1.foot.top_right in *)
(*     let slide p = V3.(add p (mul dir1 (v3 w w 0.))) in *)
(*     endpoints *)
(*       ~n_facets *)
(*       ~height *)
(*       w1.foot.bot_right *)
(*       w1.edges.bot_right *)
(*       (w1.edges.bot_right >> slide) *)
(*       (slide w1.foot.bot_right) *)
(*   and dests = base_endpoints ~n_facets ~height `Left w2 in *)
(*   let steps = base_steps ~n_steps starts dests *)
(*   and bezs = List.map2 get_bez starts dests in *)
(*   let t = prism_connection bezs steps in *)
(*   { t with outline = w1.foot.top_right :: t.outline } *)

(* let straight_base *)
(*     ?(n_facets = 1) *)
(*     ?(height = 11.) *)
(*     ?(fudge_factor = 6.) *)
(*     ?(overlap_factor = 1.2) *)
(*     ?(min_width = 4.5) *)
(*     (w1 : Wall.t) *)
(*     (w2 : Wall.t) *)
(*   = *)
(*   let ({ x = dx; y = dy; z = _ } as dir1) = Wall.foot_direction w1 *)
(*   and dir2 = Wall.foot_direction w2 *)
(*   and ({ x = lx; y = ly; z = _ } as line) = *)
(*     V3.(mid w1.foot.bot_right w1.foot.top_right -@ mid w2.foot.bot_left w2.foot.top_left) *)
(*   in *)
(*   let major_diff, minor_diff = if (abs dx > abs dy) then lx, ly else ly, lx in *)
(*   let fudge d = *)
(*     (\* For adjustment of bottom (inside face) points to account for steep angles *)
(*      * that would otherwise cause the polyhedron to fail. Distance moved is a *)
(*      * function of how far apart the walls are along the major axis of the first. *\) *)
(*     let angle_extra = *)
(*       if (abs minor_diff > abs major_diff) *)
(*       then (abs (min (abs major_diff -. fudge_factor) 0.)) /. 2. *)
(*       else 0. *)
(*     and width_extra = *)
(*       (\* Check now thick the connection is in the middle, and create a rough adjustment *)
(*          if it is less than the specified minimum. *\) *)
(*       let mid_top = V3.mid w1.foot.top_right w2.foot.top_left *)
(*       and mid_bot = V3.mid w1.foot.bot_right w2.foot.bot_left in *)
(*       let mid_diff = V3.(mid_top -@ mid_bot) in *)
(*       let align = 1. -. V3.(norm @@ cross (normalize mid_diff) (normalize line)) in *)
(*       let thickness = *)
(*         Float.max *)
(*           0. *)
(*           V3.(norm mid_diff -. (distance w1.foot.top_right w1.foot.bot_right *. align)) *)
(*       in *)
(*       if (thickness < min_width) then min_width -. thickness else 0. *)
(*     in *)
(*     let extra = Float.max angle_extra width_extra in *)
(*     V3.(add (mul d (v3 extra extra 0.))) *)
(*   and overlap = *)
(*     let major_ax = if (abs dx > abs dy) then dx else dy *)
(*     and intersect = Points.overlapping_bounds w1.foot w2.foot in *)
(*     if ( *)
(*          ( (not (Sign.equal (sign major_diff) (sign major_ax))) *)
(*          && abs major_diff > abs minor_diff ) *)
(*          || intersect > 0.) *)
(*     then ( *)
(*       let rough_area = *)
(*         V3.( *)
(*           distance w1.foot.top_right w1.foot.top_left *)
(*           *. distance w1.foot.top_right w1.foot.bot_right) *)
(*       in *)
(*       Float.max (Float.abs major_diff) (intersect *. rough_area) *. overlap_factor ) *)
(*     else 0.01 *)
(*   (\* If the walls are overlapping, move back the start positions to counter. *\) *)
(*   and outward = *)
(*     (\* away from the centre of mass, or not? *\) *)
(*     ( *)
(*       V3.distance w1.foot.top_right w2.foot.top_left *)
(*       > V3.distance w1.foot.top_right w2.foot.bot_left) *)
(*   in *)
(*   (\* The connector has two parts, and is drawn depending on whether it is going *)
(*      outward (away from plate). If moving inward, the straight move (along wall *)
(*      axis) occurs before turning towards the next wall, for outward, the *)
(*      opposite. `og` indicates whether the points include the true inner wall *)
(*      start/end positions, or are intermediaries. *\) *)
(*   let starts og = *)
(*     let bot_bez = *)
(*       if og *)
(*       then w1.edges.bot_right *)
(*       else if not outward *)
(*       then w1.edges.bot_right >> fudge dir1 *)
(*       else w1.edges.bot_right >> fudge (V3.neg dir1) *)
(*     and bot_pt = *)
(*       if og *)
(*       then w1.foot.bot_right *)
(*       else if not outward *)
(*       then fudge dir1 w1.foot.bot_right *)
(*       else fudge (V3.neg dir1) w1.foot.bot_right *)
(*     in *)
(*     endpoints ~n_facets ~height w1.foot.top_right w1.edges.top_right bot_bez bot_pt *)
(*     |> List.map V3.(add (mul dir1 (v3 overlap overlap 0.))) *)
(*   and dests og = *)
(*     let slide = fudge (V3.neg dir2) in *)
(*     let bot_bez = *)
(*       if og *)
(*       then w2.edges.bot_left *)
(*       else if outward *)
(*       then w2.edges.bot_left >> slide *)
(*       else w2.edges.bot_left >> fudge dir2 *)
(*     and bot_pt = *)
(*       if og *)
(*       then w2.foot.bot_left *)
(*       else if not outward *)
(*       then fudge dir2 w2.foot.bot_left *)
(*       else fudge (V3.neg dir2) w2.foot.bot_left *)
(*     in *)
(*     endpoints ~n_facets ~height w2.foot.top_left w2.edges.top_left bot_bez bot_pt *)
(*     |> List.map V3.(add (mul dir2 (v3 (-0.05) (-0.05) 0.))) *)
(*   in *)
(*   let extra_starts = starts false *)
(*   and extra_dests = dests false in *)
(*   { scad = *)
(*       Scad.union *)
(*         [ Util.prism extra_starts extra_dests *)
(*         ; ( if outward *)
(*           then Util.prism (starts true) extra_starts *)
(*           else Util.prism extra_dests (dests true) ) *)
(*         ] *)
(*   ; outline = [ w1.foot.top_right; w2.foot.top_left ] *)
(*   ; inline = *)
(*       List.map *)
(*         Util.last *)
(*         ( if outward *)
(*         then [ starts true; extra_starts; extra_dests ] *)
(*         else [ extra_starts; extra_dests; dests true ] ) *)
(*   } *)

(* let join_walls *)
(*     ?(n_steps = `Flat 6) *)
(*     ?(fudge_factor = 3.) *)
(*     ?(overlap_factor = 1.2) *)
(*     (w1 : Wall.t) *)
(*     (w2 : Wall.t) *)
(*   = *)
(*   let ({ x = dx; y = dy; z = _ } as dir1) = Wall.foot_direction w1 *)
(*   and dir2 = Wall.foot_direction w2 *)
(*   and n_steps = *)
(*     let summit (w : Wall.t) = *)
(*       Points.fold (fun m p -> Float.max (V3.get_z p) m) ~init:Float.min_value w.start *)
(*     in *)
(*     Wall.Steps.to_int n_steps (Float.max (summit w1) (summit w2)) *)
(*   in *)
(*   let major_diff, minor_diff = *)
(*     let { x; y; z = _ } = *)
(*       V3.( *)
(*         mid w1.foot.bot_right w1.foot.top_right -@ mid w2.foot.bot_left w2.foot.top_left) *)
(*     in *)
(*     if (abs dx > abs dy) then x, y else y, x *)
(*   in *)
(*   let outward = *)
(*     (\* away from the centre of mass, or not? *\) *)
(*     ( *)
(*       V3.distance w1.foot.top_right w2.foot.top_left *)
(*       > V3.distance w1.foot.top_right w2.foot.bot_left) *)
(*   and overhang = *)
(*     (\* Obtuse angle between wall top difference and the foot difference indicates *)
(*        that the start wall is likely dodging the column below. If this is a corner, *)
(*        don't flag. *\) *)
(*     let foot_diff = V3.(w1.foot.top_right -@ w2.foot.top_right) *)
(*     and top_diff = V3.(mul (w1.start.top_right -@ w2.start.top_left) (v3 1. 1. 0.)) in *)
(*     let mag = V3.norm top_diff in *)
(*     if (mag > 0.1 && V3.dot foot_diff top_diff < 0.) then Some mag else None *)
(*   in *)
(*   (\* Move the start or destination points along the outer face of the wall to improve angle. *\) *)
(*   let fudge start = *)
(*     if (not outward) && not start *)
(*     then ( *)
(*       let extra = *)
(*         if (V3.(get_z w1.start.top_right > get_z w2.start.top_left)) *)
(*         then (abs (max (fudge_factor -. major_diff) 0.)) *)
(*         else 0. *)
(*       in *)
(*       V3.(add (mul dir2 (v3 (-.extra) (-.extra) 0.))) ) *)
(*     else Fun.id *)
(*   and overlap = *)
(*     match overhang with *)
(*     | Some over -> over *. overlap_factor *)
(*     | None -> *)
(*       let major_ax = if (abs dx > abs dy) then dx else dy *)
(*       and intersect = Points.overlapping_bounds w1.foot w2.foot in *)
(*       if ( *)
(*            ( (not (Sign.equal (sign major_diff) (sign major_ax))) *)
(*            && abs major_diff > abs minor_diff ) *)
(*            || intersect > 0.) *)
(*       then ( *)
(*         let rough_area = *)
(*           V3.( *)
(*             distance w1.foot.top_right w1.foot.top_left *)
(*             *. distance w1.foot.top_right w1.foot.bot_right) *)
(*         in *)
(*         Float.max (Float.abs major_diff) (intersect *. rough_area) *. overlap_factor ) *)
(*       else 0.01 *)
(*     (\* If the walls are overlapping, move back the start positions to counter. *\) *)
(*   in *)
(*   (\* HACK: The way I am using the overhang flag here seemed to partially rescue one case, *)
(*      but I am not confident that it is a solid fix. *\) *)
(*   let top_start, starts = *)
(*     let shove = V3.(add (mul dir1 (v3 overlap overlap 0.))) in *)
(*     let top = *)
(*       w1.edges.top_right 0. *)
(*       |> fudge true *)
(*       |> (if Option.is_some overhang then Fun.id else shove) *)
(*       |> w1.edge_drawer.top *)
(*     in *)
(*     ( top *)
(*     , Bezier3.curve *)
(*         ~rev:true *)
(*         ~fn:n_steps *)
(*         ~init: *)
(*           (Bezier3.curve *)
(*              ~fn:n_steps *)
(*              (w1.edge_drawer.bot (shove @@ w1.edges.bot_right 0.)) ) *)
(*         top ) *)
(*   and top_dest, dests = *)
(*     let shove = V3.(add (mul dir2 (v3 (-.overlap) (-.overlap) 0.))) in *)
(*     let top = *)
(*       w2.edges.top_left 0. *)
(*       |> fudge false *)
(*       |> (if Option.is_some overhang then Fun.id else shove) *)
(*       |> w2.edge_drawer.top *)
(*     in *)
(*     ( top *)
(*     , Bezier3.curve *)
(*         ~rev:true *)
(*         ~fn:n_steps *)
(*         ~init: *)
(*           (Bezier3.curve ~fn:n_steps (w2.edge_drawer.bot (shove @@ w2.edges.bot_left 0.))) *)
(*         top ) *)
(*   in *)
(*   let wedge = *)
(*     (\* Fill in the volume between the "wedge" hulls that are formed by swinging the *)
(*      * key face and moving it out for clearance prior to drawing the walls. *\) *)
(*     Util.prism *)
(*       (List.map *)
(*          V3.(add (mul (Wall.start_direction w1) (v3 overlap overlap 0.))) *)
(*          [ w1.start.top_right *)
(*          ; w1.start.bot_right *)
(*          ; w1.edges.bot_right 0. *)
(*          ; top_start 0.001 *)
(*          ] ) *)
(*       (List.map *)
(*          V3.(add (mul (Wall.start_direction w2) (v3 (-0.01) (-0.01) 0.))) *)
(*          [ w2.start.top_left; w2.start.bot_left; w2.edges.bot_left 0.; top_dest 0.001 ] ) *)
(*   in *)
(*   { scad = Scad.union [ Util.prism starts dests; wedge ] *)
(*   ; outline = [ fudge true w1.foot.top_right; fudge false w2.foot.top_left ] *)
(*   ; inline = *)
(*       [ V3.(add (mul dir1 (v3 overlap overlap 0.)) w1.foot.bot_right) *)
(*       ; V3.(add (mul dir2 (v3 (-.overlap) (-.overlap) 0.)) w2.foot.bot_left) *)
(*       ] *)
(*   } *)

type config =
  | Straight of
      { n_facets : int option
      ; height : float option
      ; fudge_factor : float option
      ; overlap_factor : float option
      ; min_width : float option
      }
  | Bez of
      { n_facets : int option
      ; height : float option
      ; n_steps : int option
      }
  | Cubic of
      { n_facets : int option
      ; height : float option
      ; scale : float option
      ; d : float option
      ; n_steps : int option
      ; bow_out : bool option
      }
  | Snake of
      { n_facets : int option
      ; height : float option
      ; scale : float option
      ; d : float option
      ; n_steps : int option
      }
  | FullJoin of
      { n_steps : Wall.Steps.t option
      ; fudge_factor : float option
      ; overlap_factor : float option
      }
  | InwardElbow of
      { n_facets : int option
      ; height : float option
      ; n_steps : int option
      ; d : float option
      }
  | Spline of
      { height : float option
      ; n_steps : int option
      }

let straight ?n_facets ?height ?fudge_factor ?overlap_factor ?min_width () =
  Straight { n_facets; height; fudge_factor; overlap_factor; min_width }

let bez ?n_facets ?height ?n_steps () = Bez { n_facets; height; n_steps }

let cubic ?n_facets ?height ?scale ?d ?n_steps ?bow_out () =
  Cubic { n_facets; height; scale; d; n_steps; bow_out }

let snake ?n_facets ?height ?scale ?d ?n_steps () =
  Snake { n_facets; height; scale; d; n_steps }

let full_join ?n_steps ?fudge_factor ?overlap_factor () =
  FullJoin { n_steps; fudge_factor; overlap_factor }

let elbow ?n_facets ?height ?n_steps ?d () = InwardElbow { n_facets; height; n_steps; d }
let spline ?height ?n_steps () = Spline { height; n_steps }

let connect = function
  | Spline { height; n_steps } -> spline_base ?height ?n_steps
  (* | _ -> failwith "only spline implemented" *)
  | _ -> spline_base ~height:11. ~n_steps:6
(* | Straight { n_facets; height; fudge_factor; overlap_factor; min_width } -> *)
(*   straight_base ?n_facets ?height ?fudge_factor ?overlap_factor ?min_width *)
(* | Bez { n_facets; height; n_steps } -> bez_base ?n_facets ?height ?n_steps *)
(* | Cubic { n_facets; height; scale; d; n_steps; bow_out } -> *)
(*   cubic_base ?n_facets ?height ?scale ?d ?n_steps ?bow_out *)
(* | Snake { n_facets; height; scale; d; n_steps } -> *)
(*   snake_base ?n_facets ?height ?scale ?d ?n_steps *)
(* | FullJoin { n_steps; fudge_factor; overlap_factor } -> *)
(*   join_walls ?n_steps ?fudge_factor ?overlap_factor *)
(* | InwardElbow { n_facets; height; n_steps; d } -> *)
(*   inward_elbow_base ?n_facets ?height ?n_steps ?d *)

let manual_joiner ~join key next (i, last, scads) =
  let scads' =
    match Option.map (fun l -> connect (join i) l next) last with
    | Some j -> j :: scads
    | None -> scads
  in
  key, Some next, scads'

(* TODO: Now that Walls.t is simplified somewhat, this can likely be cleaned up
   a bit. It's been adjusted to work with the change, but there is likely some
   uneccesary repetition. Go over it again soon. *)
let manual
    ?(west = fun _ -> bez ())
    ?(north =
      function
      | i when i < 2 -> full_join ()
      | i when i = 4 -> cubic ()
      | _ -> straight ())
    ?(south = fun _ -> straight ())
    ?(east = fun _ -> bez ())
    ?(east_link = snake ())
    ?(thumb_east = fun _ -> full_join ())
    ?(thumb_south = fun _ -> full_join ())
    ?(thumb_west = fun _ -> full_join ())
    ?(thumb_north = fun _ -> straight ())
    ?(west_link = cubic ~bow_out:false ())
    Walls.{ body; thumb }
  =
  let southeast = Option.map snd (IMap.max_binding_opt body.south) in
  let west =
    let northwest =
      let+ _, nw = IMap.min_binding_opt body.north in
      nw
    and last_idx, last, side =
      let f = manual_joiner ~join:west in
      IMap.fold f body.west (0, None, [])
    in
    List.rev
    @@ Util.prepend_opt (Util.map2_opt (connect (west last_idx)) last northwest) side
  in
  let north =
    let last_idx, last, side =
      let f = manual_joiner ~join:north in
      IMap.fold f body.north (0, None, [])
    and next =
      (* if there is nothing in the east, connect to the southern corner *)
      Util.first_some (Option.map snd @@ IMap.max_binding_opt body.east) southeast
    in
    List.rev @@ Util.prepend_opt (Util.map2_opt (connect (north last_idx)) last next) side
  in
  let east =
    let south_corner =
      let last_idx, last =
        match IMap.min_binding_opt body.east with
        | Some (i, w) -> i, Some w
        | None -> 0, None
      in
      Util.map2_opt (connect (east last_idx)) last southeast
    and _, _, side =
      let f = manual_joiner ~join:east in
      IMap.fold_right f body.east (0, None, [])
    in
    Util.prepend_opt south_corner side |> List.rev
  and last_south, south =
    let _, last, side =
      let f = manual_joiner ~join:south in
      IMap.fold_right f body.south (0, None, [])
    in
    last, List.rev side
  in
  let thumb_swoop =
    let last_idx, last_thumb_south, swoop =
      let southeast = Option.map snd (IMap.max_binding_opt thumb.south) in
      let first =
        Util.first_some (Option.map snd (IMap.min_binding_opt thumb.east)) southeast
      in
      let e_link = Util.map2_opt (connect east_link) last_south first in
      let last_idx, last_east, east =
        let f = manual_joiner ~join:thumb_east in
        IMap.fold f thumb.east (0, None, Option.to_list e_link)
      in
      let se = Util.map2_opt (connect (thumb_east last_idx)) last_east southeast in
      let f = manual_joiner ~join:thumb_south in
      IMap.fold_right f thumb.south (0, None, Util.prepend_opt se east)
    in
    let last_thumb_west, swoop =
      let sw =
        let first_west = Option.map snd (IMap.max_binding_opt thumb.west) in
        Util.map2_opt (connect (thumb_south last_idx)) last_thumb_south first_west
      in
      let northwest =
        let+ _, nw = IMap.min_binding_opt thumb.north in
        nw
      and last_idx, last, side =
        let f = manual_joiner ~join:thumb_west in
        IMap.fold_right f thumb.west (0, None, Util.prepend_opt sw swoop)
      in
      ( last
      , Util.prepend_opt
          (Util.map2_opt (connect (thumb_west last_idx)) last northwest)
          side )
    in
    let _, last, swoop =
      let f = manual_joiner ~join:thumb_north in
      IMap.fold f thumb.north (0, None, swoop)
    in
    Util.prepend_opt
      (Util.map2_opt
         (connect west_link)
         (Util.first_some last (Util.first_some last_thumb_west last_thumb_south))
         (Option.map snd @@ IMap.min_binding_opt body.west) )
      swoop
    |> List.rev
  in
  (* unions separately, followed by final union so failures in CGAL can be narrowed
     down more easily (a section disappears, rather than the whole thing) *)
  ignore (south, thumb_swoop, west, north, east);
  List.map clockwise_union [ west; north; east; south; thumb_swoop ] |> clockwise_union
(* List.map clockwise_union [ west; north; east ] |> clockwise_union *)
(* List.map clockwise_union [ west; north; east; south ] |> clockwise_union *)
(* List.map clockwise_union [ west; north; east; thumb_swoop ] |> clockwise_union *)

let skeleton
    ?(n_facets = 1)
    ?(index_height = 11.)
    ?height
    ?min_straight_width
    ?n_steps
    ?body_join_steps
    ?thumb_join_steps
    ?fudge_factor
    ?join_fudge_factor
    ?overlap_factor
    ?cubic_d
    ?cubic_scale
    ?thumb_height
    ?(east_link = snake ())
    ?(west_link = cubic ~bow_out:false ())
    ?(north_joins = fun i -> i < 2)
    ?(south_joins = fun _ -> false)
    ?(pinky_idx = 4)
    ?(pinky_elbow = true)
    ?(close_thumb = false)
    (Walls.{ body; thumb } as walls)
  =
  (* TODO:
     - bez_base is not great on the northeast corner when there is actually a wall
       on that side that needs to be connected, because it was designed for cornering
       like it is used on the west side. *)
  let body_join =
    full_join ?n_steps:body_join_steps ?fudge_factor:join_fudge_factor ?overlap_factor ()
  and thumb_join =
    full_join ?n_steps:thumb_join_steps ?fudge_factor:join_fudge_factor ?overlap_factor ()
  and body_straight index =
    straight
      ~n_facets
      ?height:(if index then Some index_height else height)
      ?min_width:min_straight_width
      ?fudge_factor
      ?overlap_factor
      ()
  and thumb_straight =
    straight
      ~n_facets
      ?height:thumb_height
      ?min_width:min_straight_width
      ?fudge_factor
      ?overlap_factor
      ()
  and thumb_corner =
    if close_thumb
    then full_join ?n_steps:thumb_join_steps ~fudge_factor:0. ?overlap_factor ()
    else bez ?height ?n_steps ()
  in
  let thumb_mid = if close_thumb then thumb_join else thumb_straight in
  let west =
    let last_idx = Util.value_map_opt fst ~default:0 (IMap.max_binding_opt body.west) in
    fun i ->
      if i = last_idx
      then bez ~n_facets ~height:index_height ?n_steps ()
      else body_straight true
  in
  let north =
    let last_idx = Util.value_map_opt fst ~default:0 (IMap.max_binding_opt body.north) in
    fun i ->
      match last_idx = i, north_joins i with
      | true, _ ->
        if IMap.is_empty body.east
        then cubic ~n_facets ?height ?d:cubic_d ?scale:cubic_scale ?n_steps ()
        else bez ?height ?n_steps ()
      | false, true -> body_join
      | _ -> body_straight (i < 2)
  in
  let east =
    let last_idx = Util.value_map_opt fst ~default:0 (IMap.min_binding_opt body.east) in
    fun i -> if i = last_idx then body_straight false else bez ?height ?n_steps ()
  in
  let south =
    let last_idx = Util.value_map_opt fst ~default:0 (IMap.min_binding_opt body.south) in
    fun i ->
      match last_idx = i, south_joins i with
      | true, _ -> east_link
      | false, true -> body_join
      | _ ->
        if i = pinky_idx && pinky_elbow
        then elbow ~n_facets ~d:1.5 ?height ?n_steps ()
        else body_straight false
  in
  let thumb_side last_idx i = if i = last_idx then thumb_corner else thumb_mid
  and idx elt = Util.value_map_opt fst ~default:0 elt in
  manual
    ~west
    ~north
    ~east
    ~south
    ~east_link
    ~thumb_east:(thumb_side (idx @@ IMap.max_binding_opt thumb.east))
    ~thumb_south:(thumb_side (idx @@ IMap.min_binding_opt thumb.south))
    ~thumb_west:(thumb_side (idx @@ IMap.min_binding_opt thumb.west))
    ~thumb_north:(thumb_side (idx @@ IMap.max_binding_opt thumb.north))
    ~west_link
    walls

let closed
    ?body_steps
    ?thumb_steps
    ?fudge_factor
    ?overlap_factor
    ?(west_link = full_join ~fudge_factor:0. ())
    ?(east_link = snake ())
    (Walls.{ body; _ } as walls)
  =
  let join = full_join ?fudge_factor ?overlap_factor
  and corner = full_join ~fudge_factor:0. ?overlap_factor in
  let side ?n_steps last_idx i =
    if i = last_idx then corner ?n_steps () else join ?n_steps ()
  and max_key m = fst (IMap.max_binding m) in
  let north =
    let last_idx = max_key body.north in
    if IMap.cardinal walls.body.east = 0
    then
      fun i ->
      if i = last_idx then cubic ~height:10. () else join ?n_steps:body_steps ()
    else side ?n_steps:body_steps last_idx
  in
  manual
    ~west:(side ?n_steps:body_steps (max_key body.west))
    ~north
    ~east:(side ?n_steps:body_steps 0)
    ~south:(fun _ -> join ?n_steps:body_steps ())
    ~east_link
    ~thumb_east:(fun _ -> corner ?n_steps:thumb_steps ())
    ~thumb_south:(side ?n_steps:thumb_steps 0)
    ~thumb_west:(fun _ -> corner ?n_steps:thumb_steps ())
    ~thumb_north:(fun _ -> join ?n_steps:thumb_steps ())
    ~west_link
    walls

let to_scad t = t.scad
