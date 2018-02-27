open Bigarray

type pixmap

val init : width:int -> height:int -> pixmap
val dump : string -> pixmap -> unit
val load : string -> pixmap

val width : pixmap -> int
val height : pixmap -> int

val get : pixmap -> int -> int -> int -> int
val set : pixmap -> int -> int -> int -> int -> unit
val setp : pixmap -> int -> int -> int -> int -> int -> unit
