open Program

let rtd = 180. /. acos (-1.)
let dtr = acos (-1.) /. 180.
let deg x = rtd *. x
let rad x = dtr *. x
let zero = VFloat 0.
let one = VFloat 1.

let rec lookup env s =
  match env with
    [] -> failwith ("Unbound variable \"" ^ s ^ "\"")
  | s' :: env' when s = s' -> 0
  | _ :: env' -> 1 + (lookup env' s)

(* XXX embed values *)
let rec conv absenv p =
  match p with
    [] -> []
  | Float x :: Float y :: Float z :: Prim Point :: r ->
      Val' (VPoint (VFloat x, VFloat y, VFloat z)) :: conv absenv r
  | t :: r ->
      (match t with
	Fun p' -> Fun' (conv absenv p') :: conv absenv r
      |	Arr p' -> Arr' (conv absenv p') :: conv absenv r
      |	Ident s -> Ident' (lookup absenv s) :: conv absenv r
      |	Binder s -> Binder' :: conv (s :: absenv) r
      |	Int i -> Val' (VInt i) :: conv absenv r
      |	Float f -> Val' (VFloat f) :: conv absenv r
      |	Bool b -> Val' (VBool b) :: conv absenv r
      |	String s -> Val' (VStr s) :: conv absenv r
      |	Prim k -> Prim' k :: conv absenv r)

let rec inline offset env p =
  match p with
    [] -> []
  | t :: r ->
      (match t with
	Fun' p' -> Fun' (inline offset env p') :: inline offset env r
      |	Arr' p' -> Arr' (inline offset env p') :: inline offset env r
      |	Ident' i when i >= offset ->
	  Val' (List.nth env (i - offset)) :: inline offset env r
      |	Binder' -> Binder' :: inline (1 + offset) env r
      |	Prim' _ | Val' _ | Ident' _ -> t :: inline offset env r)

let inline_closure = function
    (VClos (env, p)) -> VClos ([], inline 0 env p)
  | _ -> failwith "a surface function was actually not a function"

let _ = Render.inline_closure := inline_closure

let rec eval env st p =
  match st, p with
(* inlined value *)
    _, Val' v :: r -> eval env (v :: st) r
(* Rule 1 *)
(*
  | _, Bool' b :: r   -> eval env (VBool b :: st) r
  | _, Int' i :: r    -> eval env (VInt i :: st) r
  | _, Float' f :: r  -> eval env (VFloat f :: st) r
  | _, String' s :: r -> eval env (VStr s :: st) r
*)
(* Rule 2 *)
  | v::st', Binder' :: r -> eval (v :: env) st' r
(* Rule 3 *)
  | _, Ident' i :: r ->
      let v = List.nth env i in
      eval env (v :: st) r
(* Rule 4 *)
  | _, Fun' f :: r -> eval env (VClos (env, f) :: st) r
(* Rule 5 *)
  | VClos (env', f) :: st', Prim' Apply :: r ->
      eval env (eval env' st' f) r
(* Rule 6 *)
  | _, Arr' a :: r ->
      eval env (VArr (Array.of_list (List.rev (eval env [] a))) :: st) r
(* Rules 7 and 8 *)
  | VClos _ :: VClos (env', iftrue) :: VBool true :: st', Prim' If :: r ->
      eval env (eval env' st' iftrue) r
  | VClos (env', iffalse) :: VClos _ :: VBool false :: st', Prim' If :: r ->
      eval env (eval env' st' iffalse) r
(* Operations on numbers *)
  | VInt n2 :: VInt n1 :: st', Prim' Addi :: r -> eval env (VInt (n1 + n2) :: st') r
  | VFloat f2 :: VFloat f1 :: st', Prim' Addf :: r ->
      eval env (VFloat (f1 +. f2) :: st') r
  | VFloat f :: st', Prim' Acos :: r -> eval env (VFloat (deg (acos f)) :: st') r
  | VFloat f :: st', Prim' Asin :: r -> eval env (VFloat (deg (asin f)) :: st') r
  | VFloat f as vf:: st', Prim' Clampf :: r ->
      let f' = if f < 0. then zero else if f > 1. then one else vf in
      eval env (f' :: st') r
  | VFloat f :: st', Prim' Cos :: r -> eval env (VFloat (cos (rad f)) :: st') r
  | VInt n2 :: VInt n1 :: st', Prim' Divi :: r -> eval env (VInt (n1 / n2) :: st') r
  | VFloat f2 :: VFloat f1 :: st', Prim' Divf :: r ->
      eval env (VFloat (f1 /. f2) :: st') r
  | VInt n2 :: VInt n1 :: st', Prim' Eqi :: r -> eval env (VBool (n1 = n2) :: st') r
  | VFloat f2 :: VFloat f1 :: st', Prim' Eqf :: r ->
      eval env (VBool (f1 = f2) :: st') r
  | VFloat f :: st', Prim' Floor :: r ->
      eval env (VInt (truncate (floor f)) :: st') r
  | VFloat f :: st', Prim' Frac :: r -> eval env (VFloat (mod_float f 1.0) :: st') r
  | VInt n2 :: VInt n1 :: st', Prim' Lessi :: r ->
      eval env (VBool (n1 < n2) :: st') r
  | VFloat f2 :: VFloat f1 :: st', Prim' Lessf :: r ->
      eval env (VBool (f1 < f2) :: st') r
  | VInt n2 :: VInt n1 :: st', Prim' Modi :: r ->
      eval env (VInt (n1 mod n2) :: st') r
  | VInt n2 :: VInt n1 :: st', Prim' Muli :: r -> eval env (VInt (n1 * n2) :: st') r
  | VFloat f2 :: VFloat f1 :: st', Prim' Mulf :: r ->
      eval env (VFloat (f1 *. f2) :: st') r
  | VInt n :: st', Prim' Negi :: r -> eval env (VInt (- n) :: st') r
  | VFloat f :: st', Prim' Negf :: r -> eval env (VFloat (-. f) :: st') r
  | VInt n :: st', Prim' Real :: r -> eval env (VFloat (float n) :: st') r
  | VFloat f :: st', Prim' Sin :: r -> eval env (VFloat (sin (rad f)) :: st') r
  | VFloat f :: st', Prim' Sqrt :: r -> eval env (VFloat (sqrt f) :: st') r
  | VInt n2 :: VInt n1 :: st', Prim' Subi :: r -> eval env (VInt (n1 - n2) :: st') r
  | VFloat f2 :: VFloat f1 :: st', Prim' Subf :: r ->
      eval env (VFloat (f1 -. f2) :: st') r
(* Operations on points *)
  | VPoint (x, _, _) :: st', Prim' Getx :: r  -> eval env (x :: st') r
  | VPoint (_, y, _) :: st', Prim' Gety :: r  -> eval env (y :: st') r
  | VPoint (_, _, z) :: st', Prim' Getz :: r  -> eval env (z :: st') r
  | (VFloat _ as z) :: (VFloat _ as y) :: (VFloat _ as x) :: st', Prim' Point :: r ->
      eval env (VPoint (x, y, z) :: st') r
  | VInt i :: VArr a :: st', Prim' Get :: r ->
      (* if compiled with "-unsafe" *)
      if i < 0 || i >= Array.length a
      then failwith "illegal access beyond array boundary"
      else eval env (a.(i) :: st') r
  | VArr a :: st', Prim' Length :: r ->
      eval env (VInt (Array.length a) :: st') r
(* Geometric primitives *)
  | VClos _ as f :: st', Prim' Sphere :: r   ->
      eval env (VObj (OObj (OSphere, ref (Unopt f))) :: st') r
  | VClos _ as f :: st', Prim' Cube :: r     ->
      eval env (VObj (OObj (OCube, ref (Unopt f)))   :: st') r
  | VClos _ as f :: st', Prim' Cylinder :: r ->
      eval env (VObj (OObj (OCylind, ref (Unopt f))) :: st') r
  | VClos _ as f :: st', Prim' Cone :: r     ->
      eval env (VObj (OObj (OCone, ref (Unopt f)))   :: st') r
  | VClos _ as f :: st', Prim' Plane :: r    ->
      eval env (VObj (OObj (OPlane, ref (Unopt f)))  :: st') r
(* Transformations *)
  | VFloat z :: VFloat y :: VFloat x :: VObj o :: st', Prim' Translate :: r ->
      eval env
        (VObj (OTransform (o,
                           Matrix.translate x y z,
                           Matrix.translate (-. x) (-. y) (-. z),
                           1., true)) :: st')
        r
  | VFloat z :: VFloat y :: VFloat x :: VObj o :: st', Prim' Scale :: r ->
      eval env
        (VObj (OTransform (o,
                           Matrix.scale x y z,
                           Matrix.unscale x y z,
                           max (abs_float x) (max (abs_float y) (abs_float z)),
                           false)) :: st')
        r
  | VFloat s :: VObj o :: st', Prim' Uscale :: r ->
      eval env
        (VObj (OTransform (o, Matrix.uscale s, Matrix.unuscale s,
                           abs_float s, true)) :: st')
        r
  | VFloat t :: VObj o :: st', Prim' Rotatex :: r ->
      eval env
        (VObj (OTransform (o, Matrix.rotatex t, Matrix.rotatex (-. t),
                           1., true)) :: st')
        r
  | VFloat t :: VObj o :: st', Prim' Rotatey :: r ->
      eval env
        (VObj (OTransform (o, Matrix.rotatey t, Matrix.rotatey (-. t),
                           1., true)) :: st')
        r
  | VFloat t :: VObj o :: st', Prim' Rotatez :: r ->
      eval env
        (VObj (OTransform (o, Matrix.rotatez t, Matrix.rotatez (-. t),
                           1., true)) :: st')
        r
(* Lights *)
  | (VPoint _ as color) :: (VPoint _ as dir) :: st', Prim' Light :: r ->
      eval env (VLight (dir, color) :: st') r
  | (VPoint _ as color) :: (VPoint _ as pos) :: st', Prim' Pointlight :: r ->
      eval env (VPtLight (pos, color) :: st') r
  | (VFloat _ as expon) :: (VFloat _ as cutoff) :: (VPoint _ as color) ::
    (VPoint _ as at) :: (VPoint _ as pos) :: st', Prim' Spotlight :: r ->
      eval env (VStLight (pos, at, color, cutoff, expon) :: st') r
(* Constructive geometry *)
  | (VObj o2) :: (VObj o1) :: st', Prim' Union :: r ->
      eval env (VObj (OUnion (o1, o2)) :: st') r
  | (VObj o2) :: (VObj o1) :: st', Prim' Intersect :: r ->
      eval env (VObj (OInter (o1, o2)) :: st') r
  | (VObj o2) :: (VObj o1) :: st', Prim' Difference :: r ->
      eval env (VObj (ODiff (o1, o2)) :: st') r
(* Rendering *)
  | VStr file :: VInt ht :: VInt wid :: VFloat fov :: VInt depth ::
    VObj obj :: VArr lights :: VPoint (VFloat ax, VFloat ay, VFloat az) ::
    st', Prim' Render :: r ->
(*
amb the intensity of ambient light (a point). 
lights is an array of lights used to illuminate the scene. 
obj is the scene to render. 
depth is an integer limit on the recursive depth of the ray tracing. 
fov is the horizontal field of view in degrees (a real number). 
wid is the width of the rendered image in pixels (an integer). 
ht is the height of the rendered image in pixels (an integer). 
file is a string specifying output file for the rendered image. 
*)
      Render.f (ax, ay, az) lights obj depth fov  wid ht file;
      eval env st' r
(* Termination *)
  | _, [] ->
      st
(* Failure *)
  | _ ->
      raise (Stuck_computation (env, st, p))

let apply f st =
  match f with
    VClos (env, p) ->
      eval env st p
  | _ ->
      assert false

let _ = Render.apply := apply

let f p =
  try
    let st = eval [] [] (conv [] p) in
    if st <> [] then
      Format.eprintf "@[<2>Stack:@ %a@]@." Dump.stack st
  with
    Stuck_computation (env, st, p) ->
      Format.eprintf "Execution failed@.";
      Dump.f env st p;
      exit 1
