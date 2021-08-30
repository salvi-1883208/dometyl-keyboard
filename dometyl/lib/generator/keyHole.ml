open Base
open Scad_ml

module Face = struct
  type t =
    { scad : Model.t
    ; points : Points.t
    }

  let make ((x, y, _) as size) =
    let points =
      Points.
        { top_left = x /. -2., y /. 2., 0.
        ; top_right = x /. 2., y /. 2., 0.
        ; bot_left = x /. -2., y /. -2., 0.
        ; bot_right = x /. 2., y /. -2., 0.
        ; centre = 0., 0., 0.
        }
    in
    { scad = Model.cube ~center:true size; points }

  let translate p t =
    { scad = Model.translate p t.scad; points = Points.translate p t.points }

  let rotate r t = { scad = Model.rotate r t.scad; points = Points.rotate r t.points }

  let rotate_about_pt r p t =
    { scad = Model.rotate_about_pt r p t.scad
    ; points = Points.rotate_about_pt r p t.points
    }

  let quaternion q t =
    { scad = Model.quaternion q t.scad; points = Points.quaternion q t.points }

  let quaternion_about_pt q p t =
    { scad = Model.quaternion_about_pt q p t.scad
    ; points = Points.quaternion_about_pt q p t.points
    }

  let direction { points = { top_left; top_right; _ }; _ } =
    Vec3.normalize Vec3.(top_left <-> top_right)
end

module Faces = struct
  type t =
    { north : Face.t
    ; south : Face.t
    ; east : Face.t
    ; west : Face.t
    }

  let map ~f t =
    { north = f t.north; south = f t.south; east = f t.east; west = f t.west }

  let fold ~f ~init t =
    let flipped = Fn.flip f in
    f init t.north |> flipped t.south |> flipped t.east |> flipped t.west

  let make w h depth =
    let vert north =
      Face.rotate
        Float.(pi /. 2., 0., if north then 0. else pi)
        (Face.make (w, depth, 0.1))
    and lat west =
      Face.rotate
        Float.(pi /. 2., 0., pi /. if west then 2. else -2.)
        (Face.make (h, depth, 0.1))
    in
    { north = Face.translate (0., h /. 2., 0.) (vert true)
    ; south = Face.translate (0., h /. -2., 0.) (vert false)
    ; west = Face.translate (w /. -2., 0., 0.) (lat true)
    ; east = Face.translate (w /. 2., 0., 0.) (lat false)
    }

  let face t = function
    | `North -> t.north
    | `South -> t.south
    | `East  -> t.east
    | `West  -> t.west

  let translate p = map ~f:(Face.translate p)
  let rotate r = map ~f:(Face.rotate r)
  let rotate_about_pt r p = map ~f:(Face.rotate_about_pt r p)
  let quaternion q = map ~f:(Face.quaternion q)
  let quaternion_about_pt q p = map ~f:(Face.quaternion_about_pt q p)
end

module Kind = struct
  type niz =
    { clip_height : float
    ; snap_slot_h : float
    }

  type mx = unit

  type _ t =
    | Mx : mx -> mx t
    | Niz : niz -> niz t
end

type 'k config =
  { spec : 'k Kind.t
  ; outer_w : float
  ; outer_h : float
  ; inner_w : float
  ; inner_h : float
  ; thickness : float
  ; clip : Model.t -> Model.t
  ; cap_height : float
  ; clearance : float
  }

type 'k t =
  { config : 'k config
  ; scad : Model.t
  ; origin : Vec3.t
  ; faces : Faces.t
  ; cap : Model.t option
  ; cutout : Model.t option
  }

let orthogonal t side =
  Vec3.(normalize ((Faces.face t.faces side).points.centre <-> t.origin))

let normal t =
  let Points.{ top_left; bot_left; _ } = (Faces.face t.faces `North).points in
  Vec3.(normalize (top_left <-> bot_left))

let translate p t =
  { t with
    scad = Model.translate p t.scad
  ; origin = Vec3.add p t.origin
  ; faces = Faces.translate p t.faces
  ; cap = Option.map ~f:(Model.translate p) t.cap
  ; cutout = Option.map ~f:(Model.translate p) t.cutout
  }

let rotate r t =
  { t with
    scad = Model.rotate r t.scad
  ; origin = Vec3.rotate r t.origin
  ; faces = Faces.rotate r t.faces
  ; cap = Option.map ~f:(Model.rotate r) t.cap
  ; cutout = Option.map ~f:(Model.rotate r) t.cutout
  }

let rotate_about_pt r p t =
  { t with
    scad = Model.rotate_about_pt r p t.scad
  ; origin = Vec3.rotate_about_pt r p t.origin
  ; faces = Faces.rotate_about_pt r p t.faces
  ; cap = Option.map ~f:(Model.rotate_about_pt r p) t.cap
  ; cutout = Option.map ~f:(Model.rotate_about_pt r p) t.cutout
  }

let quaternion q t =
  { t with
    scad = Model.quaternion q t.scad
  ; origin = Quaternion.rotate_vec3 q t.origin
  ; faces = Faces.quaternion q t.faces
  ; cap = Option.map ~f:(Model.quaternion q) t.cap
  ; cutout = Option.map ~f:(Model.quaternion q) t.cutout
  }

let quaternion_about_pt q p t =
  { t with
    scad = Model.quaternion_about_pt q p t.scad
  ; origin = Quaternion.rotate_vec3_about_pt q p t.origin
  ; faces = Faces.quaternion_about_pt q p t.faces
  ; cap = Option.map ~f:(Model.quaternion_about_pt q p) t.cap
  ; cutout = Option.map ~f:(Model.quaternion_about_pt q p) t.cutout
  }

let rotate_about_origin r t =
  let p = Vec3.negate t.origin in
  { t with
    scad = Model.rotate_about_pt r p t.scad
  ; faces = Faces.rotate_about_pt r p t.faces
  ; cap = Option.map ~f:(Model.rotate_about_pt r p) t.cap
  ; cutout = Option.map ~f:(Model.rotate_about_pt r p) t.cutout
  }

let quaternion_about_origin angle t =
  let p = Vec3.negate t.origin
  and q = Quaternion.make (normal t) angle in
  { t with
    scad = Model.quaternion_about_pt q p t.scad
  ; faces = Faces.quaternion_about_pt q p t.faces
  ; cap = Option.map ~f:(Model.quaternion_about_pt q p) t.cap
  ; cutout = Option.map ~f:(Model.quaternion_about_pt q p) t.cutout
  }

let cycle_faces ({ faces = { north; south; east; west }; _ } as t) =
  { t with faces = { north = west; south = east; east = north; west = south } }

let make
    ?cap
    ?cutout
    ({ outer_w; outer_h; inner_w; inner_h; thickness; clip; cap_height; _ } as config)
  =
  let hole =
    let outer = Model.cube ~center:true (outer_w, outer_h, thickness) in
    let inner = Model.cube ~center:true (inner_w, inner_h, thickness +. 0.1) in
    Model.difference outer [ inner ]
  in
  { config
  ; scad = clip hole
  ; origin = 0., 0., 0.
  ; faces = Faces.make outer_w outer_h thickness
  ; cap = Option.map ~f:(Model.translate (0., 0., cap_height)) cap
  ; cutout
  }