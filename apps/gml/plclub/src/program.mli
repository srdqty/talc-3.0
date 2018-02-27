
(**** Basic types: programs, values, ... ****)

type k =
    Acos | Addi | Addf | Apply | Asin | Clampf | Cone | Cos | Cube
  | Cylinder | Difference | Divi | Divf | Eqi | Eqf | Floor | Frac
  | Get | Getx | Gety | Getz | If | Intersect | Length | Lessi | Lessf
  | Light | Modi | Muli | Mulf | Negi | Negf | Plane | Point
  | Pointlight | Real | Render | Rotatex | Rotatey | Rotatez | Scale
  | Sin | Sphere | Spotlight | Sqrt | Subi | Subf | Translate | Union
  | Uscale

(* Program tokens *)
type t =
    Fun of t list
  | Arr of t list
  | Ident of string
  | Binder of string
  | Int of int
  | Float of float
  | Bool of bool
  | String of string
  | Prim of k

(* internal representation of program tokens *)
type t' =
    Fun' of t' list
  | Arr' of t' list
  | Ident' of int (* index to environment stack *)
  | Binder'
(*
  | Int' of int
  | Float' of float
  | Bool' of bool
  | String' of string
*)
  | Prim' of k
  | Val' of v (* inlined value *)

(* Values *)
and v =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VStr of string
  | VClos of v list * t' list
  | VFun of (v list -> v list) (* XXX for the compiler *)
  | VArr of v array
  | VPoint of v * v * v (* XXX Maybe these should be floats? *)
  | VObj of obj
  | VLight of v * v
  | VPtLight of v * v
  | VStLight of v * v * v * v * v

and obj =
    OObj of kind * closure ref
  | OTransform of
      obj *
      Matrix.t *     (* World to object *)
      Matrix.t *     (* Object to world *)
      float *        (* Scale factor *)
      bool           (* Isometry? *)
  | OUnion of obj * obj
  | OInter of obj * obj
  | ODiff of obj * obj

and kind =
    OSphere
  | OCube
  | OCylind
  | OCone
  | OPlane

and closure =
    Unopt of v (* Unoptimized function *)
  | Opt of v
  | Cst of (float * float * float * float * float * float)

(* Translation of an identifier *)
val translate : string -> t

(* Get the name of an identifier *)
val name : t' -> string

exception Stuck_computation of v list * v list * t' list
exception Stuck_computation' (* for compiler *)
