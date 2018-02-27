open Program

let conv_ident s =
  let s = String.copy s in
  for i = 0 to String.length s - 1 do
    if String.get s i = '-'
    then String.set s i '\''
    else ()
  done;
  s

let rec f = function
  | [] -> "st"
  | (t :: r) ->
      (match t with
      |	Fun p -> "let st = VFun(fun st ->\n" ^ f p ^ ") :: st in\n" ^ f r
      |	Arr p -> "let st = VArr(Array.of_list (List.rev (let st = [] in " ^ f p ^ "))) :: st in\n" ^ f r
      |	Ident s -> "let st = gml_" ^ conv_ident s ^ " :: st in\n" ^ f r
      |	Binder s ->
	  "(match st with v :: st -> (\n" ^
	  "let gml_" ^ conv_ident s ^ " = v in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Int i -> "let st = VInt(" ^ string_of_int i ^ ") :: st in\n" ^ f r
      | Float x -> "let st = VFloat(" ^ Printf.sprintf "%.20f" x (* XXX: string_of_float prints "1.0" as "1" *) ^ ") :: st in\n" ^ f r
      | Bool b -> "let st = VBool(" ^ string_of_bool b ^ ") :: st in\n" ^ f r
      | String s -> "let st = VStr(\"" ^ s ^ "\") :: st in\n" ^ f r
      |	Prim Apply ->
	  "(match st with VFun(f) :: st -> (\n" ^
	  "let st = f st in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      |	Prim If ->
	  "(match st with VFun(ffalse) :: VFun(ftrue) :: VBool(b) :: st -> (\n" ^
	  "let st = if b then ftrue st else ffalse st in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
(* Operations on numbers *)
      | Prim Addi ->
	  "(match st with VInt n2 :: VInt n1 :: st -> (\n" ^
	  "let st = (VInt (n1 + n2) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Addf ->
	  "(match st with VFloat f2 :: VFloat f1 :: st -> (\n" ^
	  "let st = (VFloat (f1 +. f2) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Acos ->
	  "(match st with VFloat f :: st -> (\n" ^
	  "let st = (VFloat (deg (acos f)) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Asin ->
	  "(match st with VFloat f :: st -> (\n" ^
	  "let st = (VFloat (deg (asin f)) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Clampf ->
	  "(match st with VFloat f as vf :: st -> (\n" ^
	  "let f' = if f < 0. then zero else if f > 1. then one else vf in\n" ^
	  "let st = (f' :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Cos ->
	  "(match st with VFloat f :: st -> (\n" ^
	  "let st = (VFloat (cos (rad f)) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Divi ->
	  "(match st with VInt n2 :: VInt n1 :: st -> (\n" ^
	  "let st = (VInt (n1 / n2) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Divf ->
	  "(match st with VFloat f2 :: VFloat f1 :: st -> (\n" ^
	  "let st = (VFloat (f1 /. f2) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Eqi ->
	  "(match st with VInt n2 :: VInt n1 :: st -> (\n" ^
	  "let st = (VBool (n1 = n2) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Eqf ->
	  "(match st with VFloat f2 :: VFloat f1 :: st -> (\n" ^
	  "let st = (VBool (f1 = f2) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Floor ->
	  "(match st with VFloat f :: st -> (\n" ^
	  "let st = (VInt (truncate (floor f)) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Frac ->
	  "(match st with VFloat f :: st -> (\n" ^
	  "let st = (VFloat (mod_float f 1.0) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Lessi ->
	  "(match st with VInt n2 :: VInt n1 :: st -> (\n" ^
	  "let st = (VBool (n1 < n2) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Lessf ->
	  "(match st with VFloat f2 :: VFloat f1 :: st -> (\n" ^
	  "let st = (VBool (f1 < f2) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Modi ->
	  "(match st with VInt n2 :: VInt n1 :: st -> (\n" ^
	  "let st = (VInt (n1 mod n2) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Muli ->
	  "(match st with VInt n2 :: VInt n1 :: st -> (\n" ^
	  "let st = (VInt (n1 * n2) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Mulf ->
	  "(match st with VFloat f2 :: VFloat f1 :: st -> (\n" ^
	  "let st = (VFloat (f1 *. f2) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Negi ->
	  "(match st with VInt n :: st -> (\n" ^
	  "let st = (VInt (- n) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Negf ->
	  "(match st with VFloat f :: st -> (\n" ^
	  "let st = (VFloat (-. f) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Real ->
	  "(match st with VInt n :: st -> (\n" ^
	  "let st = (VFloat (float n) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Sin ->
	  "(match st with VFloat f :: st -> (\n" ^
	  "let st = (VFloat (sin (rad f)) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Sqrt ->
	  "(match st with VFloat f :: st -> (\n" ^
	  "let st = (VFloat (sqrt f) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Subi ->
	  "(match st with VInt n2 :: VInt n1 :: st -> (\n" ^
	  "let st = (VInt (n1 - n2) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Subf ->
	  "(match st with VFloat f2 :: VFloat f1 :: st -> (\n" ^
	  "let st = (VFloat (f1 -. f2) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
(* Operations on points *)
      | Prim Getx ->
	  "(match st with VPoint (x, _, _) :: st  -> (\n" ^
	  "let st = (x :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Gety ->
	  "(match st with VPoint (_, y, _) :: st  -> (\n" ^
	  "let st = (y :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Getz ->
	  "(match st with VPoint (_, _, z) :: st  -> (\n" ^
	  "let st = (z :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Point ->
	  "(match st with (VFloat _ as z) :: (VFloat _ as y) :: (VFloat _ as x) :: st -> (\n" ^
	  "let st = (VPoint (x, y, z) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Get ->
	  "(match st with VInt i :: VArr a :: st -> (\n" ^
	  "if i < 0 || i >= Array.length a\n" ^
	  "then failwith \"illegal access beyond array boundary\"\n" ^
	  "else let st = (a.(i) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Length ->
	  "(match st with VArr a :: st -> (\n" ^
	  "let st = (VInt (Array.length a) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
(* Geometric primitives *)
      | Prim Sphere ->
	  "(match st with VFun _ as f :: st   -> (\n" ^
	  "let st = (VObj (OObj (OSphere, ref (Unopt f))) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Cube ->
	  "(match st with VFun _ as f :: st     -> (\n" ^
	  "let st = (VObj (OObj (OCube, ref (Unopt f)))   :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Cylinder ->
	  "(match st with VFun _ as f :: st -> (\n" ^
	  "let st = (VObj (OObj (OCylind, ref (Unopt f))) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Cone ->
	  "(match st with VFun _ as f :: st     -> (\n" ^
	  "let st = (VObj (OObj (OCone, ref (Unopt f)))   :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Plane ->
	  "(match st with VFun _ as f :: st    -> (\n" ^
	  "let st = (VObj (OObj (OPlane, ref (Unopt f)))  :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
(* Transformations *)
      | Prim Translate ->
	  "(match st with VFloat z :: VFloat y :: VFloat x :: VObj o :: st -> (\n" ^
	  "let st = (VObj (OTransform (o, Matrix.translate x y z, Matrix.translate (-. x) (-. y) (-. z), 1., true)) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Scale ->
	  "(match st with VFloat z :: VFloat y :: VFloat x :: VObj o :: st -> (\n" ^
	  "let st = (VObj (OTransform (o, Matrix.scale x y z, Matrix.unscale x y z, max (abs_float x) (max (abs_float y) (abs_float z)), false)) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Uscale ->
	  "(match st with VFloat s :: VObj o :: st -> (\n" ^
	  "let st = (VObj (OTransform (o, Matrix.uscale s, Matrix.unuscale s, abs_float s, true)) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Rotatex ->
	  "(match st with VFloat t :: VObj o :: st -> (\n" ^
	  "let st = (VObj (OTransform (o, Matrix.rotatex t, Matrix.rotatex (-. t), 1., true)) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Rotatey ->
	  "(match st with VFloat t :: VObj o :: st -> (\n" ^
	  "let st = (VObj (OTransform (o, Matrix.rotatey t, Matrix.rotatey (-. t), 1., true)) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Rotatez ->
	  "(match st with VFloat t :: VObj o :: st -> (\n" ^
	  "let st = (VObj (OTransform (o, Matrix.rotatez t, Matrix.rotatez (-. t), 1., true)) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
(* Lights *)
      | Prim Light ->
	  "(match st with (VPoint _ as color) :: (VPoint _ as dir) :: st -> (\n" ^
	  "let st = (VLight (dir, color) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Pointlight ->
	  "(match st with (VPoint _ as color) :: (VPoint _ as pos) :: st -> (\n" ^
	  "let st = (VPtLight (pos, color) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Spotlight ->
	  "(match st with (VFloat _ as expon) :: (VFloat _ as cutoff) :: (VPoint _ as color) :: (VPoint _ as at) :: (VPoint _ as pos) :: st -> (\n" ^
	  "let st = (VStLight (pos, at, color, cutoff, expon) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
(* Constructive geometry *)
      | Prim Union ->
	  "(match st with (VObj o2) :: (VObj o1) :: st -> (\n" ^
	  "let st = (VObj (OUnion (o1, o2)) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Intersect ->
	  "(match st with (VObj o2) :: (VObj o1) :: st -> (\n" ^
	  "let st = (VObj (OInter (o1, o2)) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Difference ->
	  "(match st with (VObj o2) :: (VObj o1) :: st -> (\n" ^
	  "let st = (VObj (ODiff (o1, o2)) :: st) in\n" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      | Prim Render ->
	  "(match st with VStr file :: VInt ht :: VInt wid :: VFloat fov :: VInt depth :: VObj obj :: VArr lights :: VPoint (VFloat ax, VFloat ay, VFloat az) :: st -> (\n" ^
	  "Render.f (ax, ay, az) lights obj depth fov wid ht file;" ^
	  f r ^ ") | _ -> raise Stuck_computation')\n"
      )

let f p =
  "(*** BEGIN ***)\n" ^
  "prerr_endline \"begin_compiled\"\n;;\n" ^
  "open Program;;\n" ^
  "Render.inline_closure := fun v -> v;;\n" ^
  "Render.apply := function VFun f -> (fun st -> f st) | _ -> assert false;;\n" ^
  "let rtd = 180. /. acos (-1.) in\n" ^
  "let dtr = acos (-1.) /. 180. in\n" ^
  "let deg x = rtd *. x in\n" ^
  "let rad x = dtr *. x in\n" ^
  "let zero = VFloat 0. in\n" ^
  "let one = VFloat 1. in\n" ^
  "let st = [] in\n" ^
  f p ^
  ";;\nprerr_endline \"end_compiled\"\n" ^
  "(*** END ***)\n"
