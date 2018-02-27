
open Program

let optimize_scene = ref true

let color_step = 1.0 /. 256.0 

(* Scene description *)
type kind = (* section 3.2 *)
    SSphere of Matrix.v (* Center *) * float (* Square of the radius *)
  | SEllips
  | SCube of Matrix.v (* Normal x = 0 *) *
             Matrix.v (* Normal y = 0 *) *
             Matrix.v (* Normal z = 0 *)
  | SCylind of Matrix.v (* Normal *)
  | SCone of Matrix.v (* Normal *)
  | SPlane of Matrix.v (* Equation *) * Matrix.v (* Normal *)

type scene = (* section 3.7 *)
    SObj of kind * closure ref (* surface function *) * Matrix.t
  | SBound of scene * Matrix.v (* Center *) * float (* Square of the radius *)
  | SUnion of scene * scene
  | SInter of scene * scene
  | SDiff of scene * scene

type light = (* section 3.5 *)
    Light of Matrix.v (* negated & normalized *) * (float * float * float)
  | PtLight of Matrix.v * (float * float * float)
  | StLight of Matrix.v * Matrix.v (* negated & normalized *) *
               (float * float * float) * float (* cos *) * float

type desc =
  { amb : float * float * float;
    lights : light array;
    scene : scene }

let rec dump_scene o sc =
  match sc with
    SObj (kind, _, _) ->
      begin match kind with
        SSphere (x, r) ->
(*
          Format.fprintf o "@[<3>(sphere@ %a@ %f)@]"
            Matrix.vprint x r
*)
          Format.fprintf o "sphere"
      | SEllips ->
          Format.fprintf o "ellipsoid"
      | SCube _ ->
          Format.fprintf o "cube"
      | SCylind _ ->
          Format.fprintf o "cylinder"
      | SCone _ ->
          Format.fprintf o "cone"
      | SPlane _ ->
          Format.fprintf o "plane"
      end
  | SBound (sc1, x, r) ->
      Format.fprintf o "@[<3>(bound@ %a@ %f@ %a)@]"
        Matrix.vprint x r dump_scene sc1
  | SUnion (sc1, sc2) ->
      Format.fprintf o "@[<3>(union@ %a@ %a)@]"
        dump_scene sc1 dump_scene sc2
  | SInter (sc1, sc2) ->
      Format.fprintf o "@[<3>(inter@ %a@ %a)@]"
        dump_scene sc1 dump_scene sc2
  | SDiff (sc1, sc2) ->
      Format.fprintf o "@[<3>(diff@ %a@ %a)@]"
        dump_scene sc1 dump_scene sc2

open Math
open Matrix

(**** Scene calculation ****)

(* Plane equation and normal in world coordinates *)
let plane_eq m v =
  let n = vmul (transpose m) v in
  let n' = Array.copy n in
  n'.(3) <- 0.;
  (n, normalize n')

let origin = [| 0.; 0.; 0.; 1. |]
let cube_center = [| 0.5; 0.5; 0.5; 1. |]
let cylinder_center = [| 0.; 0.5; 0.; 1. |]
let cone_center = [| 0.; 1.; 0.; 1. |]

let rec intern_obj m m1 scale isom o =
(* apply transformations *)
  match o with
  | OObj (OPlane, f) ->
      let (n, n') = plane_eq m [| 0.; 1.; 0.; 0. |] in
      SObj (SPlane (n, n'), f, m)
  | OObj (k,f) ->
      let (so,center,radius) = 
	match k with
	| OSphere when isom ->
	    let center = vmul m1 origin in
	    let radius = scale *. scale in
	    (SObj (SSphere (center, radius), f, m),center,radius)
	| OSphere ->
	    let center = vmul m1 origin in
	    let radius = scale *. scale in
	    (SObj (SEllips, f, m), center, radius)
	| OCube ->
	    let (nx, nx') = plane_eq m [| 1.; 0.; 0.; 0. |] in
	    let (ny, ny') = plane_eq m [| 0.; 1.; 0.; 0. |] in
	    let (nz, nz') = plane_eq m [| 0.; 0.; 1.; 0. |] in
	    let c = SObj (SCube (nx', ny', nz'), f, m) in
	    (c, vmul m1 cube_center, scale *. scale *. 0.75)
	| OCylind ->
	    let (n, n') = plane_eq m [| 0.; 1.; 0.; 0. |] in
	    let c = SObj (SCylind n', f, m) in
	    (c, vmul m1 cylinder_center, scale *. scale *. 1.25)
	| OCone ->
	    let (n, n') = plane_eq m [| 0.; 1.; 0.; 0. |] in
	    let c = SObj (SCone n', f, m) in
	    (c, vmul m1 cone_center, scale *. scale)
	| OPlane -> raise Not_found (* IMPOSSIBLE *)
      in
      (if !optimize_scene then SBound(so,center,radius) else so)
(*
    OObj (OSphere, f) when isom ->
      let center = vmul m1 origin in
      let radius = scale *. scale in
      SBound (SObj (SSphere (center, radius), f, m), center, radius)
  | OObj (OSphere, f) ->
      let center = vmul m1 origin in
      let radius = scale *. scale in
      SBound (SObj (SEllips, f, m), center, radius)
  | OObj (OCube, f) ->
      let (nx, nx') = plane_eq m [| 1.; 0.; 0.; 0. |] in
      let (ny, ny') = plane_eq m [| 0.; 1.; 0.; 0. |] in
      let (nz, nz') = plane_eq m [| 0.; 0.; 1.; 0. |] in
      let c = SObj (SCube (nx', ny', nz'), f, m) in
      SBound (c, vmul m1 cube_center, scale *. scale *. 0.75)
  | OObj (OCylind, f) ->
      let (n, n') = plane_eq m [| 0.; 1.; 0.; 0. |] in
      let c = SObj (SCylind n', f, m) in
      SBound (c, vmul m1 cylinder_center, scale *. scale *. 1.25)
  | OObj (OCone, f) ->
      let (n, n') = plane_eq m [| 0.; 1.; 0.; 0. |] in
      let c = SObj (SCone n', f, m) in
      SBound (c, vmul m1 cone_center, scale *. scale)
  | OObj (OPlane, f) ->
      let (n, n') = plane_eq m [| 0.; 1.; 0.; 0. |] in
      SObj (SPlane (n, n'), f, m)
*)
  | OTransform (o', m', m'1, scale', isom') ->
      intern_obj
        (Matrix.mul m' m) (Matrix.mul m1 m'1)
        (scale *. scale') (isom && isom') o'
  | OUnion (o1, o2) ->
      SUnion (intern_obj m m1 scale isom o1, intern_obj m m1 scale isom o2)
  | OInter (o1, o2) ->
      SInter (intern_obj m m1 scale isom o1, intern_obj m m1 scale isom o2)
  | ODiff (ODiff (o1, o2), o3) ->
      (* Better to have unions that diffs for introducing bounds *)
      intern_obj m m1 scale isom (ODiff (o1, OUnion (o2, o3)))
  | ODiff (o1, o2) ->
      SDiff (intern_obj m m1 scale isom o1, intern_obj m m1 scale isom o2)

let intern_lights a =
  Array.map
    (fun o ->
       match o with
         VLight (VPoint (VFloat x, VFloat y, VFloat z),
                 VPoint (VFloat r, VFloat g, VFloat b)) ->
           Light (normalize (neg [|x; y; z; 0.|]), (r, g, b))
       | VPtLight (VPoint (VFloat x, VFloat y, VFloat z),
                   VPoint (VFloat r, VFloat g, VFloat b)) ->
           PtLight ([|x; y; z; 1.|], (r, g, b))
       | VStLight (VPoint (VFloat x, VFloat y, VFloat z),
                   VPoint (VFloat x', VFloat y', VFloat z'),
                   VPoint (VFloat r, VFloat g, VFloat b),
                   VFloat cutoff, VFloat exp) ->
           StLight ([|x; y; z; 1.|],
                    normalize [|x -. x'; y -. y'; z -. z'; 0.|],
                    (r, g, b), dcos cutoff, exp)
       | _ ->
           assert false)
    a

(**** Scene optimization ****)

let rec flatten_rec sc rem =
  match sc with
    SUnion (sc1, sc2) -> flatten_rec sc1 (flatten_rec sc2 rem)
  | sc                -> sc :: rem

let flatten_union sc = flatten_rec sc []

let object_cost k =
  match k with
    SSphere _ -> 1
  | SEllips   -> 2
  | SCube _   -> 4
  | SCylind _ -> 2
  | SCone _   -> 2
  | SPlane _  -> 0 (* Planes do not have a bounding box anyway *)

let add_bound r0 (x, r, cost, sc) =
  if r0 < 0. then begin
    if r < 0. || cost <= 1 then
      (cost, sc)
    else
      (1, SBound (sc, x, r))
  end else
    (* Cost with bounds *)
    let c0 = r0 +. r *. float cost in
    (* Cost without bounds *)
    let c1 = r0 *. float cost in
    if c0 < c1 then
      (1, SBound (sc, x, r))
    else
      (cost, sc)

let union_bound
    ((x1, r1, cost1, sc1) as dsc1) ((x2, r2, cost2, sc2) as dsc2) =
  if r1 < 0. then
    let (cost2', sc2') = add_bound r1 dsc2 in
    (x1, r1, cost1, SUnion (sc1, sc2'))
  else if r2 < 0. then
    let (cost1', sc1') = add_bound r2 dsc1 in
    (x2, r2, cost2, SUnion (sc1', sc2))
  else
    let d = sqrt (square (sub x2 x1)) in
    let r1' = sqrt r1 in
    let r2' = sqrt r2 in
    if d +. r2' <= r1' then
      let (cost2', sc2') = add_bound r1 dsc2 in
      (x1, r1, cost1 + cost2', SUnion (sc1, sc2'))
    else if d +. r1' <= r2' then
      let (cost1', sc1') = add_bound r2 dsc1 in
      (x2, r2, cost1' + cost2, SUnion (sc1', sc2))
    else
      let r' = (r1' +. r2' +. d) *. 0.5 in
      let r = r' *. r' in
      let x = add_scaled x1 ((r' -. r1') /. d) (sub x2 x1) in
      let (cost1', sc1') = add_bound r dsc1 in
      let (cost2', sc2') = add_bound r dsc2 in
      (x, r, cost1' + cost2', SUnion (sc1', sc2'))

let union_radius
    ((x1, r1, cost1, sc1) as dsc1) ((x2, r2, cost2, sc2) as dsc2) =
  let d = sqrt (square (sub x2 x1)) in
  let r1' = sqrt r1 in
  let r2' = sqrt r2 in
  if d +. r2' <= r1' then r1 else
  if d +. r1' <= r2' then r2 else
  let r' = (r1' +. r2' +. d) *. 0.5 in
  r' *. r'

let rec merge2 l =
  match l with
    sc1 :: sc2 :: r -> union_bound sc1 sc2 :: merge2 r
  | _               -> l

let rec merge_union l =
  match l with
    []    -> assert false
  | [sc1] -> sc1
  | l     -> merge_union (merge2 l)

let opt_union l =
  match l with
    [] | [_] ->
      l
  | [sc1; sc2] ->
      [union_bound sc1 sc2]
  | _ ->
      let c = Array.of_list l in
      let n = Array.length c in
      let m = Array.create_matrix n n infinity in
      for i = 0 to n - 1 do
        for j = 0 to n - 1 do
          if i != j then m.(i).(j) <- union_radius c.(i) c.(j)
        done
      done;
      let remain = Array.init n (fun i -> i) in
      for k = n - 1 downto 1 do
        let gain = ref infinity in
        let i0 = ref 0 in
        let j0 = ref 0 in
        for i = 0 to k do
          for j = 0 to k do
            let i' = remain.(i) in
            let j' = remain.(j) in
            if m.(i').(j') < !gain then begin
              gain := m.(i').(j'); i0 := i; j0 := j
            end
          done
        done;
        let i = remain.(!i0) in
        let j = remain.(!j0) in
        remain.(!j0) <- remain.(k);
        c.(i) <- union_bound c.(i) c.(j);
        for j0 = 0 to k - 1 do
          let j = remain.(j0) in
          if i <> j then begin
            m.(i).(j) <- union_radius c.(i) c.(j);
            m.(j).(i) <- union_radius c.(i) c.(j)
          end
        done
      done;
      [c.(remain.(0))]

let rec optimize_rec sc =
  match sc with
    SObj (kind, _, _) ->
      (origin, -1., object_cost kind, sc)
  | SUnion _ ->
      let l = List.map optimize_rec (flatten_union sc) in
      let unbounded = List.filter (fun (_, r, _, _) -> r < 0.) l in
      let bounded = List.filter (fun (_, r, _, _) -> r >= 0.) l in
      merge_union (opt_union bounded @ unbounded)
  | SInter (sc1, sc2) ->
      let (x1, r1, cost1, sc1) = optimize_rec sc1 in
      let (x2, r2, cost2, sc2) = optimize_rec sc2 in
      (* XXX We could have a tighter bound... *)
      if r2 < 0. then
        (x2, r2, cost2, SInter (sc1, sc2))
      else if r1 < 0. then
        (x1, r1, cost1, SInter (sc2, sc1))
      else if r1 < r2 then
        (x1, r1, cost1, SInter (sc1, sc2))
      else
        (x2, r2, cost1, SInter (sc2, sc1))
  | SDiff (sc1, sc2) ->
      let (x1, r1, cost1, sc1) = optimize_rec sc1 in
      let (x2, r2, cost2, sc2) as dsc2 = optimize_rec sc2 in
      let (cost2', sc2') = add_bound r1 dsc2 in
      (x1, r1, cost1, SDiff (sc1, sc2'))
  | SBound (sc1, x, r) ->
      let (_, _, cost1, sc1) = optimize_rec sc1 in
      (x, r, cost1, sc1)

let optimize sc = snd (add_bound (-1.) (optimize_rec sc))

let rec check_bounds x0 r0 sc =
  match sc with
    SObj (SSphere (x, r), _, _) ->
      let d = sqrt (square (sub x x0)) in
      if d +. sqrt r -. sqrt r0 > epsilon then
        Format.eprintf "@[<2>Bad bound:@ %a@ %f@ %a@ %f@ %f %f@]@."
          Matrix.vprint x0 (sqrt r0)
          Matrix.vprint x (sqrt r) d
          (d +. sqrt r -. sqrt r0);
  | SObj _ ->
      ()
  | SUnion (sc1, sc2) ->
      check_bounds x0 r0 sc1;
      check_bounds x0 r0 sc2 
 | SInter (sc1, sc2) ->
      check_bounds x0 r0 sc1;
      check_bounds x0 r0 sc2
  | SDiff (sc1, sc2) ->
      check_bounds x0 r0 sc1;
      check_bounds origin 1e5 sc2
  | SBound (sc1, x, r) ->
      let d = sqrt (square (sub x x0)) in
      if d +. sqrt r -. sqrt r0 > epsilon then
        Format.eprintf "@[<2>Bad bound:@ %a@ %f@ %a@ %f@ %f %f@]@."
          Matrix.vprint x0 (sqrt r0)
          Matrix.vprint x (sqrt r) d
          (d +. sqrt r -. sqrt r0);
      check_bounds x r sc1

(**** Rendering ****)

(* operations for intervals *)
let rec union l1 l2 = (* ES: checked *)
  match l1, l2 with
    [], _ ->
      l2
  | _, [] ->
      l1
  | ((t1, o1, t1', o1') as i1) :: r1,
    ((t2, o2, t2', o2') as i2) :: r2 when (t1' : float) < t2 ->
      i1 :: union r1 l2
  | ((t1, o1, t1', o1') as i1) :: r1,
    ((t2, o2, t2', o2') as i2) :: r2 when t2' < t1 ->
      i2 :: union l1 r2
  | (t1, o1, t1', o1') :: r1,
    (t2, o2, t2', o2') :: r2 ->
      if t1 < t2 then
        if t1' < t2' then
          union r1 ((t1, o1, t2', o2')::r2)
        else
          union ((t1, o1, t1', o1')::r1) r2
      else
        if t1' < t2' then
          union r1 ((t2, o2, t2', o2')::r2)
        else
          union ((t2, o2, t1', o1')::r1) r2

let rec inter l1 l2 = (* ES: checked *)
  match l1, l2 with
    [], _ ->
      []
  | _, [] ->
      []
  | (t1, o1, t1', o1') :: r1,
    (t2, o2, t2', o2') :: r2 when (t1' : float) <= t2 ->
      inter r1 l2
  | (t1, o1, t1', o1') :: r1,
    (t2, o2, t2', o2') :: r2 when t2' <= t1 ->
      inter l1 r2
  | ((t1, o1, t1', o1') as i1) :: r1,
    ((t2, o2, t2', o2') as i2) :: r2 ->
      if t1 < t2 then
        if t1' < t2' then
          (t2, o2, t1', o1') :: inter r1 l2
        else
          i2 :: inter l1 r2
      else
        if t1' < t2' then
          i1 :: inter r1 l2
        else
          (t1, o1, t2', o2') :: inter l1 r2

let rec diff l1 l2 = (* ES: checked *)
  match l1, l2 with
    [], _ ->
      []
  | _, [] ->
      l1
  | ((t1, o1, t1', o1') as i1) :: r1,
    (t2, o2, t2', o2') :: r2 when (t1' : float) <= t2 ->
      i1 :: diff r1 l2
  | (t1, o1, t1', o1') :: r1,
    (t2, o2, t2', o2') :: r2 when t2' <= t1 ->
      diff l1 r2
  | ((t1, o1, t1', o1') as i1) :: r1,
    ((t2, o2, t2', o2') as i2) :: r2 ->
      if t1 < t2 then
        if t1' <= t2' then
          (t1, o1, t2, o2) :: diff r1 l2
        else
          (t1, o1, t2, o2) :: diff ((t2', o2', t1', o1') :: r1) r2
      else
        if t1' <= t2' then
          diff r1 l2
        else
          diff ((t2', o2', t1', o1') :: r1) r2

(* intersection of ray and object *)
let plane orig dir scene eq =
(*
 (* ES: checked *)
  let x = vmul m orig in
  let v = vmul m dir in
  let t = -. x.(1) /. v.(1) in
  if x.(1) < 0. then
    if t > 0. then
      [(0., scene, t, scene)]
    else
      [(0., scene, infinity, scene)]
  else
    if t > 0. then
      [(t, scene, infinity, scene)]
    else
      []
*)
(* XXX Need to be checked *)
  let porig = prod eq orig in
  let pdir = prod eq dir in
  let t = -. porig /. pdir in
  if porig < 0. then
    if t > 0. then
      [(0., scene, t, scene)]
    else
      [(0., scene, infinity, scene)]
  else
    if t > 0. then
      [(t, scene, infinity, scene)]
    else
      []

let band obj x v i = (* ES: checked *)
  let t1 = -. x.(i) /. v.(i) in
  let t2 = (1. -. x.(i)) /. v.(i) in
  let t2' = if t1 >= t2 then t1 else t2 in
  if t2' < 0. then
    []
  else
    let t1' = if t1 <= t2 then t1 else t2 in
    if t1' < 0. then
      [(0., obj, t2', obj)]
    else
      [(t1', obj, t2', obj)]

let cube orig dir scene m = (* ES: checked *)
  let x = vmul m orig in
  let v = vmul m dir in
  match band scene x v 0 with
    [] -> []
  | l0 -> match inter l0 (band scene x v 1) with
            [] -> []
          | l1 -> inter l1 (band scene x v 2)

let sphere orig dir scene x r2 =
  let v = sub x orig in
  (* Square of the distance between the origin and the center of the sphere *)
  let v2 = square v in
  let dir2 = square dir in
  let p = prod v dir in
  (* Square of the distance between the ray and the center *)
  let d2 = v2 -. p *. p /. dir2 in
  let delta = r2 -. d2 in
  if delta <= 0. then
    []
  else
    let sq = sqrt (delta /. dir2) in
    let t1 = p /. dir2 -. sq in
    let t2 = p /. dir2 +. sq in
    if t2 < 0. then
      []
    else
      [(max_float 0. t1, scene, t2, scene)]

let ellipsoid orig dir scene m = (* ES: checked *)
  let x = vmul m orig in
  let v = vmul m dir in
  let x2 = square x in
  let v2 = square v in
  let xv = prod x v in
  let delta = xv *. xv -. v2 *. (x2 -. 2.) in
  if delta <= 0. then
    []
  else
    let sq = sqrt delta in
    let t1 = (-. xv -. sq) /. v2 in
    let t2 = (-. xv +. sq) /. v2 in
    if t2 < 0. then
      []
    else
      [(max_float 0. t1, scene, t2, scene)]

let cylinder orig dir scene m = (* ES: checked *)
  let x = vmul m orig in
  let v = vmul m dir in
  let x2 = x.(0) *. x.(0) +. x.(2) *. x.(2) -. 1. in
  let v2 = v.(0) *. v.(0) +. v.(2) *. v.(2) in
  let xv = x.(0) *. v.(0) +. x.(2) *. v.(2) in
  let delta = xv *. xv -. v2 *. x2 in
  if delta <= 0. then
    []
  else
    let sq = sqrt delta in
    let t1 = (-. xv -. sq) /. v2 in
    let t2 = (-. xv +. sq) /. v2 in
    if t2 < 0. then
      []
    else
      inter
        [(max_float 0. t1, scene, t2, scene)]
        (band scene x v 1)

let cone orig dir scene m = (* ES: checked *)
  let x = vmul m orig in
  let v = vmul m dir in
  let x2 = x.(0) *. x.(0) +. x.(2) *. x.(2) -. x.(1) *. x.(1) in
  let v2 = v.(0) *. v.(0) +. v.(2) *. v.(2) -. v.(1) *. v.(1) in
  let xv = x.(0) *. v.(0) +. x.(2) *. v.(2) -. x.(1) *. v.(1) in
  let delta = xv *. xv -. v2 *. x2 in
  if delta <= 0. then
    []
  else
    let sq = sqrt delta in
    let t1 = (-. xv -. sq) /. v2 in
    let t2 = (-. xv +. sq) /. v2 in
    if t1 <= t2 then begin
      if t2 < 0. then
        []
      else
        inter
          [(max_float 0. t1, scene, t2, scene)]
          (band scene x v 1)
    end else begin
      inter
        (if t1 <= 0. then
          [(0., scene, infinity, scene)]
         else if t2 <= 0. then
          [(t1, scene, infinity, scene)]
         else
          [(0., scene, t2, scene); (t1, scene, infinity, scene)])
        (band scene x v 1)
    end

(* XXX Maybe we should check whether the sphere is completely behind us ? *)
let intersect orig dir x r2 =
(*
  let v = sub x orig in
  (* Square of the distance between the origin and the center of the sphere *)
  let v2 = square v in
  let dir2 = square dir in
  let p = prod v dir in
  (* Square of the distance between the ray and the center *)
  let d2 = v2 -. p *. p /. dir2 in
  r2 > d2
*)
  let v = sub x orig in
  (* Square of the distance between the origin and the center of the sphere *)
  let vx = Array.unsafe_get v 0 in
  let vy = Array.unsafe_get v 1 in
  let vz = Array.unsafe_get v 2 in
  let vt = Array.unsafe_get v 3 in
  let v2 = vx *. vx +. vy *. vy +. vz *. vz +. vt *. vt in
  let vx = Array.unsafe_get dir 0 in
  let vy = Array.unsafe_get dir 1 in
  let vz = Array.unsafe_get dir 2 in
  let vt = Array.unsafe_get dir 3 in
  let dir2 = vx *. vx +. vy *. vy +. vz *. vz +. vt *. vt in
  let p =
    Array.unsafe_get v 0 *. Array.unsafe_get dir 0 +.
    Array.unsafe_get v 1 *. Array.unsafe_get dir 1 +.
    Array.unsafe_get v 2 *. Array.unsafe_get dir 2 +.
    Array.unsafe_get v 3 *. Array.unsafe_get dir 3
  in
  (* Square of the distance between the ray and the center *)
  let d2 = v2 -. p *. p /. dir2 in
  r2 > d2

let rec find_all orig dir scene =
  match scene with
    SObj (SSphere (x, r2), _, m) ->
      sphere orig dir scene x r2
  | SObj (SEllips, _, m) ->
      ellipsoid orig dir scene m
  | SObj (SCube _, _, m) ->
      cube orig dir scene m
  | SObj (SCylind _, _, m) ->
      cylinder orig dir scene m
  | SObj (SCone _, _, m) ->
      cone orig dir scene m
  | SObj (SPlane (eq, _), _, m) ->
      plane orig dir scene eq
  | SBound (sc, x, r2) ->
      if intersect orig dir x r2 then find_all orig dir sc else []
  | SUnion (sc1, sc2) ->
      union (find_all orig dir sc1) (find_all orig dir sc2)
  | SInter (sc1, sc2) ->
      let l1 = find_all orig dir sc1 in
      if l1 = [] then [] else
      inter l1 (find_all orig dir sc2)
  | SDiff (sc1, sc2) ->
      let l1 = find_all orig dir sc1 in
      if l1 = [] then [] else
      diff l1 (find_all orig dir sc2)

let rec filter_inter_list l =
  match l with
    (t, _, _, _)::r when t < epsilon -> filter_inter_list r
  | _                                -> l

let hit_from_inter bounded l0 =
  let l = filter_inter_list l0 in
  l <> [] &&
  (not bounded ||
   match l with
     []                          -> false
   | (t, _, _, _)::r when t > 1. -> false
   | _                           -> true)

let rec hit orig dir scene bounded =
  match scene with
    SObj (kind, _, m) ->
      begin match
        match kind with
          SSphere (x, r2) -> sphere orig dir scene x r2
        | SEllips         -> ellipsoid orig dir scene m
        | SCube _         -> cube orig dir scene m
        | SCylind _       -> cylinder orig dir scene m
        | SCone _         -> cone orig dir scene m
        | SPlane (eq, _)  -> plane orig dir scene eq
      with
        []                                    -> false
      | [(t, _, _, _)] when bounded && t > 1. -> false
      | [(t, _, _, _)] when t < epsilon       -> false
      | _                                     -> true
      end
  | SBound (sc, x, r2) ->
      intersect orig dir x r2  && hit orig dir sc bounded
  | SUnion (sc1, sc2) ->
      hit orig dir sc1 bounded
        ||
      hit orig dir sc2 bounded
  | SInter (sc1, sc2) ->
      let l1 = find_all orig dir sc1 in
      if l1 = [] then false else
      hit_from_inter bounded (inter l1 (find_all orig dir sc2))
  | SDiff (sc1, sc2) ->
      let l1 = find_all orig dir sc1 in
      if l1 = [] then false else
      hit_from_inter bounded (diff l1 (find_all orig dir sc2))

let visible desc orig dir bounded =
(*
  let l = filter_inter_list (find_all orig dir desc.scene) in
  if l = [] then true else
  if not bounded then false else
  match l with
    []                          -> true
  | (t, _, _, _)::r when t > 1. -> true
  | _                           -> false
*)
  not (hit orig dir desc.scene bounded)

let black = (0., 0., 0.)

let apply = ref (fun _ _ -> assert false)
let inline_closure = ref (fun _ -> assert false)

(* Value between 0 and 1 from the sinus and cosinus *)
(* Actually, only the sign of the sinus is used *)
let angle si co =
  let u = dacos co /. 360. in
  if si > 0. then u else 1. -. u

(* XXX Check that 0 <= u,v <= 1 *)
let texture_coord kind x = (* section 3.6 *) (* ES: checked *)
  (* [v; u; face] *)
  match kind with
    SEllips | SSphere _ ->
      let y = x.(1) in
      let v = (y +. 1.) *. 0.5 in
      if v < epsilon then
        [VFloat v; VFloat 0.; VInt 0]
      else
        let u = angle x.(0) (x.(2) /. sqrt (1. -. y *. y)) in
        [VFloat v; VFloat u; VInt 0]
  | SCube _ ->
      if abs_float x.(2) < epsilon then
        [VFloat x.(1); VFloat x.(0); VInt 0]
      else if abs_float (x.(2) -. 1.) < epsilon then
        [VFloat x.(1); VFloat x.(0); VInt 1]
      else if abs_float x.(0) < epsilon then
        [VFloat x.(1); VFloat x.(2); VInt 2]
      else if abs_float (x.(0) -. 1.) < epsilon then
        [VFloat x.(1); VFloat x.(2); VInt 3]
      else if abs_float (x.(1) -. 1.) < epsilon then
        [VFloat x.(2); VFloat x.(0); VInt 4]
      else (* if abs_float x.(1) < epsilon then *)
        [VFloat x.(2); VFloat x.(0); VInt 5]
  | SCylind _ ->
      if abs_float x.(1) < epsilon then
        [VFloat ((x.(2) +. 1.) *. 0.5); VFloat ((x.(0) +. 1.) *. 0.5); VInt 2]
      else if abs_float (x.(1) -. 1.) < epsilon then
        [VFloat ((x.(2) +. 1.) *. 0.5); VFloat ((x.(0) +. 1.) *. 0.5); VInt 1]
      else
        let u = angle x.(0) x.(2) in
        [VFloat x.(1); VFloat u; VInt 0]
  | SCone _ ->
      let v = x.(1) in
      if abs_float v < epsilon then
        [VFloat v; VFloat 0.; VInt 0]
      else if abs_float (x.(1) -. 1.) < epsilon then
        [VFloat ((x.(2) +. 1.) *. 0.5); VFloat ((x.(0) +. 1.) *. 0.5); VInt 1]
      else
        let u = angle x.(0) (x.(2) /. v) in
        [VFloat v; VFloat u; VInt 0]
  | SPlane _ ->
      [VFloat x.(2); VFloat x.(0); VInt 0]

let normal kind m x' x =
  match kind with
    SSphere (x0, _) ->
      normalize (sub x x0)
  | SEllips ->
      let n = vmul (transpose m) x' in
      n.(3) <- 0.;
      normalize n
  | SCylind n ->
      if abs_float x'.(1) < epsilon || abs_float (x'.(1) -. 1.) < epsilon then
        n
      else
        (* XXX Could be optimized... *)
        let n = vmul (transpose m) [| x'.(0); 0.; x'.(2); 0. |] in
        n.(3) <- 0.;
        normalize n
  | SCone n ->
      if abs_float (x'.(1) -. 1.) < epsilon then
        n
      else
        let n = vmul (transpose m) [| x'.(0); -. x'.(1); x'.(2); 0. |] in
        n.(3) <- 0.;
        normalize n
  | SCube (nx, ny, nz) ->
      if
        abs_float x'.(2) < epsilon || abs_float (x'.(2) -. 1.) < epsilon
      then
        nz
      else if
        abs_float x'.(0) < epsilon || abs_float (x'.(0) -. 1.) < epsilon
      then
        nx
      else
        ny
  | SPlane (_, n) ->
      n

let apply_surface_fun f v =
  match !apply f v with
    VFloat n :: VFloat ks :: VFloat kd ::
     VPoint (VFloat cr, VFloat cg, VFloat cb):: tl ->
       (n, ks, kd, cr, cg, cb)
  | _ ->
      failwith "A surface function returns some incorrect values"

let rec trace desc depth cr cg cb orig dir =
  let dir = normalize dir in
  match filter_inter_list (find_all orig dir desc.scene) with
    [] ->
      black
  | (t, o, _, _) :: _ ->
      trace_2 desc depth cr cg cb orig dir t o

and trace_2 desc depth cr cg cb orig dir t obj =
  let x = add_scaled orig t dir in
  match obj with
    SObj (kind, f, m) ->
      let x' = vmul m x in
      let (n, ks, kd, cr', cg', cb') =
        match !f with
          Unopt g ->
            (* First we check whether the function would fail *)
            let res = apply_surface_fun g (texture_coord kind x') in
            (* Then, we check whether it is a constant function *)
            begin try
              ignore (apply_surface_fun g [VInt 0; VInt 0; VFloat 0.]);
              f := Cst res
            with Stuck_computation _ | Stuck_computation' ->
              f := Opt (!inline_closure g)
            end;
            res
        | Opt g ->
            apply_surface_fun g (texture_coord kind x')
        | Cst res ->
            res
      in
      let cr = cr *. cr' in
      let cg = cg *. cg' in
      let cb = cb *. cb' in
      let nm = normal kind m x' x in
      let p = prod dir nm in
      let nm = if p > 0. then neg nm else nm in
      let p = -. abs_float p in
      (* Ambient composant *)
      let (ar, ag, ab) = desc.amb in
      let r = ref (kd *. ar) in
      let g = ref (kd *. ag) in
      let b = ref (kd *. ab) in
      (* Lights *)
      let lights = desc.lights in
      for i = 0 to Array.length lights - 1 do
        begin match lights.(i) with
          Light (ldir, (lr, lg, lb)) ->
            let p' = prod ldir nm in
            if p' > 0. && visible desc x ldir false then begin
              let int =
                if ks > epsilon then
                  kd *. p' +.
                  ks *. (prod (normalize (sub ldir dir)) nm) ** n
                else
                  kd *. p'
              in
              r := !r +. int *. lr;
              g := !g +. int *. lg;
              b := !b +. int *. lb
            end
        | PtLight (src, (lr, lg, lb)) ->
            let ldir = sub src x in
            let ldir' = normalize ldir in
            let p' = prod ldir' nm in
            if p' > 0. && visible desc x ldir true then begin
              let int =
                if ks > epsilon then
                  kd *. p' +.
                  ks *. (prod (normalize (sub ldir' dir)) nm) ** n
                else
                  kd *. p'
              in
              let int = 100. *. int /. (99. +. square ldir) in
              r := !r +. int *. lr;
              g := !g +. int *. lg;
              b := !b +. int *. lb
            end
        | StLight (src, maindir, (lr, lg, lb), cutoff, exp) ->
            let ldir = sub src x in
            let ldir' = normalize ldir in
            let p' = prod ldir' nm in
            let p'' = prod ldir' maindir in
            if
              p' > 0. && p'' > cutoff && visible desc x ldir true
            then begin
              let int =
                if ks > epsilon then
                  kd *. p' +.
                  ks *. (prod (normalize (sub ldir' dir)) nm) ** n
                else
                  kd *. p'
              in
              let int =
                100. *. int /. (99. +. square ldir) *.
                (p'' ** exp)
              in
              r := !r +. int *. lr;
              g := !g +. int *. lg;
              b := !b +. int *. lb
            end
        end
      done;
      r := !r *. cr;
      g := !g *. cg;
      b := !b *. cb;

      let cr = ks *. cr in
      let cg = ks *. cg in
      let cb = ks *. cb in

      (* Reflexion *)
      if ks > epsilon  && depth > 0 && 
	(cr >= (mod_float !r color_step +. 0.5 *. color_step) ||
	 cg >= (mod_float !g color_step +. 0.5 *. color_step) ||
	 cb >= (mod_float !b color_step +. 0.5 *. color_step))
      then begin
        let dir' = add_scaled dir (-2. *. p) nm in
        let (r', g', b') = trace desc (depth - 1) cr cg cb x dir' in
        r := !r +. r';
        g := !g +. g';
        b := !b +. b'
      end;
      (!r, !g, !b)
  | _ ->
      assert false

let conv c =
  let i = truncate (c *. 256.) in
  if i < 0 then 0 else
  if i >= 256 then 255 else
  i

let f ~amb ~lights ~obj ~depth ~fov ~wid ~ht ~file =
  let scene = intern_obj Matrix.identity Matrix.identity 1. true obj in
  let scene = if !optimize_scene then optimize scene else scene in
if List.mem "-scene" (Array.to_list Sys.argv) then
Format.eprintf "@[%a@]@." dump_scene scene;
  let img = Ppm.init wid ht in
  let orig = [| 0.; 0.; -1.; 1. |] in
  let width = 2. *. dtan (0.5 *. fov) in
  let delta = width /. float wid in
  let x0 = -. width /. 2. in
  let y0 = delta *. float ht /. 2. in
  let desc = { amb = amb; lights = intern_lights lights; scene = scene } in
  let period = wid * ht / 80 in
  let pixel_count = ref 0 in
  for j = 0 to ht - 1 do
    for i = 0 to wid - 1 do
      incr pixel_count;
      if !pixel_count mod period = 0 then (print_string "."; flush stdout);
      let dir =
        [| x0 +. (float i +. 0.5) *. delta;
           y0 -. (float j +. 0.5) *. delta;
           1.;
           0. |]
      in
      let (r, g, b) = trace desc depth 1.0 1.0 1.0 orig dir in
      Ppm.setp img i j (conv r) (conv g) (conv b)
     done
  done;
  Ppm.dump file img

(**************************)

let close r1 g1 b1 r2 g2 b2 =
  abs (r1 - r2) <= 2 && abs (g1 - g2) <= 2 && abs (b1 - b2) <= 2

let counter = ref 0
let fast ~amb ~lights ~obj ~depth ~fov ~wid ~ht ~file =
  let scene = intern_obj Matrix.identity Matrix.identity 1. true obj in
  let scene = optimize scene in
if List.mem "-scene" (Array.to_list Sys.argv) then
Format.eprintf "@[%a@]@." dump_scene scene;
  let img = Ppm.init wid ht in
  let orig = [| 0.; 0.; -1.; 1. |] in
  let width = 2. *. dtan (0.5 *. fov) in
  let delta = width /. float wid in
  let x0 = -. width /. 2. in
  let y0 = delta *. float ht /. 2. in
  let desc = { amb = amb; lights = intern_lights lights; scene = scene } in
  let direct i j = 
    [| x0 +. (float i +. 0.5) *. delta;
       y0 -. (float j +. 0.5) *. delta;
       1.;
       0. |]
  in
  let period = wid * ht / 80 in
  let pixel_count = ref 0 in
  for j = 0 to (ht - 1) / 2 do
    for i = 0 to (wid - 1) / 2 do
      incr pixel_count;
      if !pixel_count mod period = 0 then (print_string "."; flush stdout);
      let i = i * 2 in
      let j = j * 2 in
      let dir = direct i j in
      let (r, g, b) = trace desc depth 1.0 1.0 1.0 orig dir in
      Ppm.setp img i j (conv r) (conv g) (conv b)
     done
  done;
counter := 0;
  for j = 0 to (ht - 1) / 2 do
    for i = 0 to (wid - 1) / 2 - 1 do
      incr pixel_count;
      if !pixel_count mod period = 0 then (print_string "."; flush stdout);
      let i = i * 2 + 1 in
      let j = j * 2 in
      let r1 = Ppm.get img (i - 1) j 0 in
      let g1 = Ppm.get img (i - 1) j 1 in
      let b1 = Ppm.get img (i - 1) j 2 in
      let r2 = Ppm.get img (i + 1) j 0 in
      let g2 = Ppm.get img (i + 1) j 1 in
      let b2 = Ppm.get img (i + 1) j 2 in
      if close r1 b1 g1 r2 b2 g2 then begin
incr counter;
        Ppm.setp img i j ((r1 + r2) / 2) ((g1 + g2) / 2) ((b1 + b2) / 2)
      end else begin
        let dir = direct i j in
        let (r, g, b) = trace desc depth 1.0 1.0 1.0 orig dir in
        Ppm.setp img i j (conv r) (conv g) (conv b)
      end
    done
  done;
  if wid mod 2 = 0 then begin
    let i = wid - 1 in
    for j = 0 to (ht - 1) / 2 do
      incr pixel_count;
      if !pixel_count mod period = 0 then (print_string "."; flush stdout);
      let j = j * 2 in
      let dir = direct i j in
      let (r, g, b) = trace desc depth 1.0 1.0 1.0 orig dir in
      Ppm.setp img i j (conv r) (conv g) (conv b)
    done
  end;
  for j = 0 to (ht - 1) / 2 - 1 do
    for i = 0 to wid - 1 do
      incr pixel_count;
      if !pixel_count mod period = 0 then (print_string "."; flush stdout);
      let j = j * 2 + 1 in
      let r1 = Ppm.get img i (j - 1) 0 in
      let g1 = Ppm.get img i (j - 1) 1 in
      let b1 = Ppm.get img i (j - 1) 2 in
      let r2 = Ppm.get img i (j + 1) 0 in
      let g2 = Ppm.get img i (j + 1) 1 in
      let b2 = Ppm.get img i (j + 1) 2 in
      if close r1 b1 g1 r2 b2 g2 then begin
incr counter;
        Ppm.setp img i j ((r1 + r2) / 2) ((g1 + g2) / 2) ((b1 + b2) / 2)
      end else begin
        let dir = direct i j in
        let (r, g, b) = trace desc depth 1.0 1.0 1.0 orig dir in
        Ppm.setp img i j (conv r) (conv g) (conv b)
      end
    done
  done;
  if ht mod 2 = 0 then begin
    let j = ht - 1 in
    for i = 0 to wid - 1 do
      incr pixel_count;
      if !pixel_count mod period = 0 then (print_string "."; flush stdout);
      let dir = direct i j in
      let (r, g, b) = trace desc depth 1.0 1.0 1.0 orig dir in
      Ppm.setp img i j (conv r) (conv g) (conv b)
    done
  end;
Format.eprintf "%d / %d (%f %%)@." !counter (wid * ht)
(float !counter /. float (wid * ht) *. 100.);
  Ppm.dump file img

let f ~amb ~lights ~obj ~depth ~fov ~wid ~ht ~file =
(*  if List.mem "-all" (Array.to_list Sys.argv) then *)
    f ~amb ~lights ~obj ~depth ~fov ~wid ~ht ~file
(*  else
    fast ~amb ~lights ~obj ~depth ~fov ~wid ~ht ~file
*)
