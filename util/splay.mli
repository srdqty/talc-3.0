(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dave Walker, Karl Crary,            *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

type ('a, 'b) splaytree =
   LEAF
 | NODE of ('a * 'b * ('a, 'b) splaytree * ('a, 'b) splaytree) ref
;;

val splay : ('a -> int) -> ('a, 'b) splaytree -> bool
val splay2 : ('a -> int) -> ('a, 'b) splaytree -> 'b option
;;
