open OCADml
open OSCADml
open Dometyl
open! Examples

(* TODO the bridges between 1 2 3 columns need to be more flat
   (they get cut right now)
   also a problem with the around walls (they get deleted when rendering)*)
let body_lookups =
  let offset = function
    | 2 -> v3 4. 3.5 (-9.) (* middle *)
    | 3 -> v3 5.6 (-2.5) (-2.) (* ring *)
    | i when i >= 4 -> v3 (-3.) (-26.5) 7. (* pinky *)
    | 0 -> v3 (-2.5) 0. 5.
    | _ -> v3 0. 0. 0.
  and curve = function
    | i when i = 2 ->
      (* index*)
      Curvature.(curve ~well:(well ~radius:49. (Float.pi /. 6.)) ())
    | i when i = 0 ->
      (* inner index *)
      Curvature.(
        curve ~well:(well ~tilt:(Float.pi /. 8.7) ~radius:38.5 (Float.pi /. 4.25)) () )
    | _ -> Curvature.(curve ~well:(well ~radius:38. (Float.pi /. 4.25)) ())
  and splay = function
    | i when i = 2 -> Float.pi /. -30.
    | i when i = 3 -> Float.pi /. -17. (* ring *)
    | i when i >= 4 -> Float.pi /. -11. (* pinky *)
    | _ -> 0.
  and rows = function
    | i when i = 2 -> 4
    | i when i = 4 -> 2
    | _ -> 3
  and centre = function
    | i when i = 2 -> 2.
    | i when i >= 4 -> 0.7
    | _ -> 1.
  in
  Plate.Lookups.body ~offset ~curve ~splay ~rows ~centre ()

let thumb_lookups =
  let curve _ =
    Curvature.(
      curve
        ~fan:(fan ~radius:71.5 ~tilt:(Float.pi /. 48.) (Float.pi /. 9.))
        ~well:(well ~radius:47. (Float.pi /. 7.5))
        () )
  in
  Plate.Lookups.thumb ~curve ()

let plate_builder =
  Plate.make
    ~n_body_cols:5 (* number of columns *)
    ~body_lookups
    ~thumb_lookups
    ~thumb_offset:(v3 0. (-49.) (-5.)) (* translation *)
    ~thumb_angle:Float.(v3 (pi /. 40.) (pi /. -14.) (pi /. 24.))
      (* ~caps:Caps.Matty3.row *)
    ~caps:Caps.SA.(fun i -> if i = 0 then r4 else if i = 1 then r3 else r2)
      (* ~thumb_caps:Caps.MT3.(fun i -> if i = 1 then space_1_25u else space_1u) *)
    ~thumb_caps:Caps.SA.(fun _ -> r3)

let wall_builder plate =
  Walls.
    { body =
        auto_body
          ~d1:(`Abs 14.)
          ~d2:10.
          ~n_steps:(`PerZ 0.5)
          ~scale:(v2 0.7 0.6)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          plate
    ; thumb =
        auto_thumb
          ~d1:(`Abs 14.)
          ~d2:8.
          ~n_steps:(`PerZ 0.5)
          ~north_lookup:(fun _ -> false)
          ~south_lookup:(fun i -> i <> 1)
          ~east_lookup:(fun _ -> false)
          ~west_lookup:(fun _ -> true)
          ~scale:(v2 0.8 0.9)
          ~scale_ez:(v2 0.42 1., v2 1. 1.)
          plate
    }

let base_connector =
  Connect.skeleton
    ~height:13.
    ~index_height:16.
    ~thumb_height:14.
    ~corner:(Path3.Round.bez (`Joint 2.))
    ~corner_fn:16
    ~close_thumb:false
    ~north_joins:(fun i -> i < 2)
    ~south_joins:(Fun.const false)
(* ~east_link:(Connect.full_join ~max_angle:Float.(pi /. 2.) ()) *)

let plate_welder plate =
  Scad.union
    [ Plate.skeleton_bridges plate
      (* Plate.skeleton_bridges ~in_d:0.5 ~out_d1:0.5 ~out_d2:0.5 plate *)
      (* ; Bridge.cols ~columns:plate.body 1 2
         ; Bridge.cols ~columns:plate.body 2 3 *)
    ]

let ports_cutter = BastardShield.(cutter ~x_off:0. ~y_off:(-2.) (make ()))
(* let ports_cutter = Ports.reversible_holder ~x_off:0. ~reset_button:true ~y_off:0.5 () *)

let build ?right_hand ?hotswap () =
  (* let eyelets = Case.eyelets ~config:Eyelet.magnet_6x3_config () in *)
  let eyelets = Case.eyelets ~config:Eyelet.m4_config () in
  Case.make
    ?right_hand
    ~eyelets
    ~plate_builder
    ~plate_welder
    ~wall_builder
    ~base_connector
    ~ports_cutter
    (Mx.make_hole ?hotswap ~clearance:2. ~corner:(Path3.Round.bez (`Cut 0.5)) ~fn:16 ())

let case = build ~hotswap:`South ()
(* let thin_mag = Eyelet.Magnet { rad = 2.65; thickness = 1.2 } *)

let bottom =
  (*  For a thinner plate with 5x1 magnets, try providing [~fastener:thin_mag]
      (assuming you are using bigger magnets as the [~eyelets] for the case above). If
      [~fastener] is not specified, [Bottom.make] will default to the same magnet
      used for the case. *)
  let bump_locs =
    Bottom.
      [ thumb ~loc:(v2 0.5 0.2) Last First
      ; thumb ~loc:(v2 0.7 0.) Last Last
      ; body ~loc:(v2 0. 1.2) First Last (* top index *)
      ; body ~loc:(v2 0.5 1.3) (Idx 3) Last (* top ring *)
      ; body ~loc:(v2 0.9 0.8) Last Last (* top pinky *)
      ; body ~loc:(v2 0.7 (-0.3)) Last First (* bottom pinky *)
      ]
  in
  Bottom.make ~bump_locs case

let tent = Tent.make ~degrees:40. case
let to_file = Scad.to_file ~incl:true

let () =
  to_file "encoderyl_right.scad" (Case.to_scad ~show_caps:false case);
  to_file
    "encoderyl_left.scad"
    (Case.to_scad ~show_caps:false (build ~hotswap:`South () ~right_hand:false));
  to_file "bottom.scad" bottom;
  to_file "tent_right.scad" tent

let bastard_compare () =
  Scad.union
    [ Skeletyl.bastard_compare () |> Scad.color ~alpha:0.5 Color.Blue
    ; Case.to_scad ~show_caps:true case
    ]

let () = to_file "bastard_compare.scad" (bastard_compare ())
