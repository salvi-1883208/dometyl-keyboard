open! Base
open! Scad_ml

type idx =
  | First
  | Last
  | Idx of int

type bump_loc =
  | Col of idx * [ `N | `S ]
  | Thumb of [ `N of idx | `E | `S of idx | `W ]

let idx_to_find = function
  | First -> fun m -> Option.map ~f:snd @@ Map.min_elt m
  | Last  -> fun m -> Option.map ~f:snd @@ Map.max_elt m
  | Idx i -> fun m -> Map.find m i

let find_bump_wall (walls : Walls.t) = function
  | Col (i, side)             ->
    let%bind.Option c = idx_to_find i @@ walls.body.cols in
    Walls.Body.Cols.get c side
  | Thumb ((`W | `E) as side) -> Walls.Thumb.get_side walls.thumb.sides side
  | Thumb (`N i)              ->
    let%bind.Option k = idx_to_find i @@ walls.thumb.keys in
    k.north
  | Thumb (`S i)              ->
    let%bind.Option k = idx_to_find i @@ walls.thumb.keys in
    k.south

let default_bumps =
  [ Thumb `W
  ; Col (First, `N)
  ; Col (Idx 3, `N)
  ; Col (Last, `N)
  ; Col (Last, `S)
  ; Col (Idx 2, `S)
  ]

let bumpon ?(n_steps = 5) ~outer_rad ~inner_rad ~thickness ~inset foot =
  let Points.{ top_left; top_right; bot_left; bot_right; centre } =
    Points.map ~f:(Vec3.mul (1., 1., 0.)) foot
  in
  let normal = Vec3.(centre <-> mean [ top_left; top_right ] |> normalize) in
  let base_centre = Vec3.(map (( *. ) 0.5) (top_left <+> top_right) |> mul (1., 1., 0.))
  and hole_offset = Vec3.map (( *. ) outer_rad) normal in
  let centre = Vec3.(base_centre <+> hole_offset) in
  let circ = Scad.circle ~fn:32 outer_rad |> Scad.translate centre
  and swoop p =
    let rad_offset = Vec3.(map (( *. ) outer_rad) (normalize (p <-> base_centre))) in
    centre
    :: base_centre
    :: p
    :: Bezier.curve
         ~n_steps
         (Bezier.quad_vec3
            ~p1:p
            ~p2:Vec3.(mean [ base_centre <+> rad_offset; p ])
            ~p3:Vec3.(centre <+> rad_offset) )
    |> List.map ~f:Vec3.to_vec2
    |> Scad.polygon
  in
  let bump =
    Scad.union
      [ circ
      ; swoop (Vec3.mul bot_left (1., 1., 0.))
      ; swoop (Vec3.mul bot_right (1., 1., 0.))
      ]
    |> Scad.linear_extrude ~height:thickness
  and inset_cut = Scad.cylinder ~fn:16 inner_rad inset |> Scad.translate centre in
  bump, inset_cut

let make
    ?(degrees = 30.)
    ?(z_offset = 0.)
    ?fastener
    ?(foot_thickness = 2.)
    ?(foot_rad = 6.)
    ?(bumpon_rad = 5.5)
    ?(bumpon_inset = 0.5)
    ?(bump_locs = default_bumps)
    (case : _ Case.t)
  =
  let bb_index, bb_pinky, rot_sign =
    let _, bb_right, _, bb_left = Util.bounding_box case.connections.outline
    and pinky_home =
      let n = case.plate.config.n_cols - 1 in
      (Columns.key_exn case.plate.columns n (case.plate.config.row_centres n)).origin
    in
    if Float.(
         Vec3.(norm (pinky_home <-> (bb_right, 0., 0.)))
         < Vec3.(norm (pinky_home <-> (bb_left, 0., 0.))))
    then bb_left, bb_right, 1.
    else bb_right, bb_left, -1.
  and screws = Walls.collect_screws case.Case.walls
  and perimeter =
    Scad.difference
      (Scad.polygon (Connect.outline_2d case.connections))
      [ Scad.polygon (Connect.inline_2d case.connections) ]
  in
  let screw_config = (List.hd_exn screws).config in
  let fastener =
    match fastener with
    | None          ->
      ( match screw_config with
      | { hole = Through; _ } -> Screw.screw_fastener ()
      | _                     -> Magnet )
    | Some fastener -> fastener
  and rot = 0., Util.deg_to_rad degrees *. rot_sign, 0.
  and pivot_pt = -.bb_pinky, 0., 0. in
  let filled_top =
    let eyelets =
      List.map
        ~f:(fun Screw.{ centre; config = { inner_rad; hole; _ }; scad; _ } ->
          let proj = Scad.projection scad in
          match hole with
          | Inset _ -> proj
          | Through -> Scad.union [ Scad.translate centre (Scad.circle inner_rad); proj ]
          )
        screws
    in
    Scad.union (perimeter :: eyelets)
  and trans s = Scad.rotate_about_pt rot pivot_pt s |> Scad.translate (0., 0., z_offset)
  and base_height = Vec3.(get_z (rotate_about_pt rot pivot_pt (bb_index, 0., 0.))) in
  let hole, top_height, clearances =
    match fastener with
    | Screw { head_rad; shaft_rad; sink; height; clearance } ->
      let head_disc = Scad.circle head_rad in
      let hole =
        match sink with
        | Counter   -> Scad.linear_extrude ~height ~scale:(shaft_rad /. head_rad) head_disc
        | Pan inset ->
          Scad.union
            [ Scad.linear_extrude ~height:inset head_disc
            ; Scad.cylinder ~fn:32 shaft_rad height
            ]
      and clearances =
        let cyl =
          Scad.linear_extrude ~height:clearance head_disc
          |> Scad.translate (0., 0., -.clearance)
        in
        List.map ~f:(fun Screw.{ centre; _ } -> trans @@ Scad.translate centre cyl) screws
      in
      hole, height, clearances
    | Magnet ->
      let Screw.{ inner_rad; thickness; hole; _ } = screw_config in
      let h =
        match hole with
        | Screw.Inset inset -> inset
        | _                 -> 0.
      in
      let hole =
        Scad.union
          [ Scad.translate (0., 0., thickness -. h) @@ Scad.cylinder ~fn:32 inner_rad h
          ; Scad.cylinder ~fn:32 (inner_rad /. 2.) (thickness -. h)
          ]
      in
      hole, thickness, []
  in
  let top =
    Scad.difference
      (Scad.linear_extrude ~height:top_height filled_top)
      (List.map ~f:(fun Screw.{ centre; _ } -> Scad.translate centre hole) screws)
    |> trans
  and shell =
    trans (Scad.linear_extrude ~height:0.001 perimeter)
    |> Scad.projection
    |> Scad.linear_extrude ~height:base_height
  in
  let cut =
    let bulked_top =
      Scad.offset (`Delta 2.) filled_top
      |> Scad.linear_extrude ~height:top_height
      |> trans
    in
    Scad.hull [ bulked_top; Scad.translate (0., 0., base_height) bulked_top ]
  in
  let feet, final_cuts =
    let tilted =
      Case.rotate_about_pt rot pivot_pt case |> Case.translate (0., 0., z_offset)
    in
    let f (bumps, insets) loc =
      match find_bump_wall tilted.walls loc with
      | Some Wall.{ foot; _ } ->
        let bump, inset =
          bumpon
            ~outer_rad:foot_rad
            ~inner_rad:bumpon_rad
            ~thickness:foot_thickness
            ~inset:bumpon_inset
            foot
        in
        bump :: bumps, inset :: insets
      | None                  -> bumps, insets
    in
    List.fold ~init:([], clearances) ~f bump_locs
  in
  Scad.difference
    (Scad.union
       ( Scad.difference
           top
           [ Scad.projection top
             |> Scad.offset (`Delta 2.)
             |> Scad.linear_extrude ~height:10.
             |> Scad.translate (0., 0., -10.)
           ]
       :: Scad.difference shell [ Scad.translate (0., 0., -0.00001) cut ]
       :: feet ) )
    final_cuts
