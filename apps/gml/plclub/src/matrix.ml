
open Math

type t = float array
type v = float array

(**** Basic matrices ****)

let identity =
  [| 1.; 0.; 0.; 0.;
     0.; 1.; 0.; 0.;
     0.; 0.; 1.; 0.;
     0.; 0.; 0.; 1. |]

let translate ~x ~y ~z =
  [| 1.; 0.; 0.; -. x;
     0.; 1.; 0.; -. y;
     0.; 0.; 1.; -. z;
     0.; 0.; 0.;   1. |]

let unscale ~x ~y ~z =
  [| x;  0.; 0.; 0.;
     0.; y;  0.; 0.;
     0.; 0.; z;  0.;
     0.; 0.; 0.; 1. |]
  
let scale ~x ~y ~z = unscale (1. /. x) (1. /. y) (1. /. z)

let unuscale s = unscale s s s

let uscale s = scale s s s

let rotatex t =
  let co = dcos t in
  let si = dsin t in
  [| 1.;   0.;  0.; 0.;
     0.;   co;  si; 0.;
     0.; -. si; co; 0.;
     0.;   0.;  0.; 1. |]

let rotatey t =
  let co = dcos t in
  let si = dsin t in
  [| co; 0.; -. si; 0.;
     0.; 1.;   0.;  0.;
     si; 0.;   co;  0.;
     0.; 0.;   0.;  1. |]

let rotatez t =
  let co = dcos t in
  let si = dsin t in
  [|   co;  si; 0.; 0.;
     -. si; co; 0.; 0.;
       0.;  0.; 1.; 0.;
       0.;  0.; 0.; 1. |]

(*** Operations on matrices ***)

(*
let get (m : t) i j = m.(i * 4 + j)
let set (m : t) i j v = m.(i * 4 + j) <- v
*)
let get (m : t) i j = Array.unsafe_get m (i * 4 + j)
let set (m : t) i j v = Array.unsafe_set m (i * 4 + j) v

let print o m =
  for i = 0 to 3 do
    for j = 0 to 3 do
      Format.fprintf o "%f " (get m i j)
    done;
    Format.fprintf o "@."
  done

let vprint o v =
  Format.fprintf o "@[<1>[@ ";
  for i = 0 to 3 do
    Format.fprintf o "%f@ " v.(i)
  done;
  Format.fprintf o "]@]"

let mul m m' =
  let m'' = Array.make 16 0. in
  for i = 0 to 3 do
    for j = 0 to 3 do
      let s = ref 0. in
      for k = 0 to 3 do
        s := !s +. get m i k *. get m' k j
      done;
      set m'' i j !s
    done
  done;
  m''

let transpose m =
  let m' = Array.make 16 0. in
  for i = 0 to 3 do
    for j = 0 to 3 do
      set m' i j (get m j i)
    done
  done;
  m'

let vmul m v =
  let x = Array.unsafe_get v 0 in
  let y = Array.unsafe_get v 1 in
  let z = Array.unsafe_get v 2 in
  let t = Array.unsafe_get v 3 in
  [| x *. get m 0 0 +. y *. get m 0 1 +. z *. get m 0 2 +. t *. get m 0 3;
     x *. get m 1 0 +. y *. get m 1 1 +. z *. get m 1 2 +. t *. get m 1 3;
     x *. get m 2 0 +. y *. get m 2 1 +. z *. get m 2 2 +. t *. get m 2 3;
     x *. get m 3 0 +. y *. get m 3 1 +. z *. get m 3 2 +. t *. get m 3 3 |]

let add_scaled x t v =
  [| x.(0) +. t *. v.(0);
     x.(1) +. t *. v.(1);
     x.(2) +. t *. v.(2);
     x.(3) +. t *. v.(3) |]

let add x y =
  [| x.(0) +. y.(0);
     x.(1) +. y.(1);
     x.(2) +. y.(2);
     x.(3) +. y.(3) |]

let sub x y =
  [| Array.unsafe_get x 0 -. Array.unsafe_get y 0;
     Array.unsafe_get x 1 -. Array.unsafe_get y 1;
     Array.unsafe_get x 2 -. Array.unsafe_get y 2;
     Array.unsafe_get x 3 -. Array.unsafe_get y 3 |]

let prod x y =
  Array.unsafe_get x 0 *. Array.unsafe_get y 0 +.
  Array.unsafe_get x 1 *. Array.unsafe_get y 1 +.
  Array.unsafe_get x 2 *. Array.unsafe_get y 2 +.
  Array.unsafe_get x 3 *. Array.unsafe_get y 3

let square x =
  let vx = Array.unsafe_get x 0 in
  let vy = Array.unsafe_get x 1 in
  let vz = Array.unsafe_get x 2 in
  let vt = Array.unsafe_get x 3 in
  vx *. vx +.
  vy *. vy +.
  vz *. vz +.
  vt *. vt

let normalize x =
  let nx = sqrt (prod x x) in
  [| Array.unsafe_get x 0 /. nx;
     Array.unsafe_get x 1 /. nx;
     Array.unsafe_get x 2 /. nx;
     Array.unsafe_get x 3 /. nx |]

let neg x =
  [| -. x.(0); -. x.(1); -. x.(2); -. x.(3) |]

(*
mul
  [| 0.; 0.; 0.; 0.;
     0.; 0.; 0.; 0.;
     0.; 1.; 0.; 0.;
     0.; 0.; 0.; 0. |]
  [| 0.; 0.; 0.; 0.;
     0.; 0.; 1.; 0.;
     0.; 0.; 0.; 0.;
     0.; 0.; 0.; 0. |];;

[|0.000000; 0.000000; 0.000000; 0.000000;
  0.000000; 0.000000; 0.000000; 0.000000;
  0.000000; 0.000000; 1.000000; 0.000000;
  0.000000; 0.000000; 0.000000; 0.000000|]
*)
