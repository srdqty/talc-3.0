type k =
    Acos | Addi | Addf | Apply | Asin | Clampf | Cone | Cos | Cube
  | Cylinder | Difference | Divi | Divf | Eqi | Eqf | Floor | Frac
  | Get | Getx | Gety | Getz | If | Intersect | Length | Lessi | Lessf
  | Light | Modi | Muli | Mulf | Negi | Negf | Plane | Point
  | Pointlight | Real | Render | Rotatex | Rotatey | Rotatez | Scale
  | Sin | Sphere | Spotlight | Sqrt | Subi | Subf | Translate | Union
  | Uscale

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

and v =
  | VInt of int
  | VFloat of float
  | VBool of bool
  | VStr of string
  | VClos of v list * t' list
  | VFun of (v list -> v list) (* XXX for the compiler *)
  | VArr of v array
  | VPoint of v * v * v
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
    Unopt of v
  | Opt of v
  | Cst of (float * float * float * float * float * float)

let create_hashtables size init =
  let tbl = Hashtbl.create size in
  let tbl' = Hashtbl.create size in
  List.iter (fun (key, data) -> Hashtbl.add tbl key data) init;
  List.iter (fun (data, key) -> Hashtbl.add tbl' key data) init;
  tbl, tbl'

let (keywords, keyword_name) =
  create_hashtables 101
(* Booleans are either the literal true or the literal false. *)
    [ "true", Bool true;
      "false", Bool false;
(* Operators (see appendix) *)
      "acos", Prim Acos;
      "addi", Prim Addi;
      "addf", Prim Addf;
      "apply", Prim Apply;
      "asin", Prim Asin;
      "clampf", Prim Clampf;
      "cone", Prim Cone;
      "cos", Prim Cos;
      "cube", Prim Cube;
      "cylinder", Prim Cylinder;
      "difference", Prim Difference;
      "divi", Prim Divi;
      "divf", Prim Divf;
      "eqi", Prim Eqi;
      "eqf", Prim Eqf;
      "floor", Prim Floor;
      "frac", Prim Frac;
      "get", Prim Get;
      "getx", Prim Getx;
      "gety", Prim Gety;
      "getz", Prim Getz;
      "if", Prim If;
      "intersect", Prim Intersect;
      "length", Prim Length;
      "lessi", Prim Lessi;
      "lessf", Prim Lessf;
      "light", Prim Light;
      "modi", Prim Modi;
      "muli", Prim Muli;
      "mulf", Prim Mulf;
      "negi", Prim Negi;
      "negf", Prim Negf;
      "plane", Prim Plane;
      "point", Prim Point;
      "pointlight", Prim Pointlight;
      "real", Prim Real;
      "render", Prim Render;
      "rotatex", Prim Rotatex;
      "rotatey", Prim Rotatey;
      "rotatez", Prim Rotatez;
      "scale", Prim Scale;
      "sin", Prim Sin;
      "sphere", Prim Sphere;
      "spotlight", Prim Spotlight;
      "sqrt", Prim Sqrt;
      "subi", Prim Subi;
      "subf", Prim Subf;
      "translate", Prim Translate;
      "union", Prim Union;
      "uscale", Prim Uscale ]

let translate i =
  try Hashtbl.find keywords i with Not_found -> Ident i

let name token =
  Hashtbl.find keyword_name
    (match token with
      Prim' k -> Prim k
    | _       -> raise Not_found)

exception Stuck_computation of v list * v list * t' list
exception Stuck_computation' (* for compiler *)
