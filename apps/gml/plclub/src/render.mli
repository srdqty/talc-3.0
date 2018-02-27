
val apply : (Program.v -> Program.v list -> Program.v list) ref
val inline_closure : (Program.v -> Program.v) ref

val optimize_scene : bool ref

val f :
  amb:(float * float * float) -> lights: Program.v array ->
  obj:Program.obj -> depth:int -> fov:float -> wid:int -> ht:int ->
  file:string -> unit
