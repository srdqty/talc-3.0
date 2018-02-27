
(**** Matrix arithmetic ****)

type t = float array (* 4-dimension matrix *)
type v = float array (* 4-dimension vector *)

(* Basic matrices *)
val identity : t
val translate : x:float -> y:float -> z:float -> t
val scale : x:float -> y:float -> z:float -> t
val uscale : float -> t
val unscale : x:float -> y:float -> z:float -> t
val unuscale : float -> t
val rotatex : float -> t
val rotatey : float -> t
val rotatez : float -> t

(* Operations on matrices *)
val mul : t -> t -> t
val vmul : t -> v -> v
val transpose : t -> t

val print : Format.formatter -> t -> unit
val vprint : Format.formatter -> v -> unit

val add_scaled : v -> float -> v -> v
val add : v -> v -> v
val sub : v -> v -> v
val prod : v -> v -> float
val square : v -> float
val normalize : v -> v
val neg : v -> v
