(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dave Walker, Steve Zdancewic        *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Sets *)

type 'a set ;;

val empty : ('a -> 'a -> int) -> 'a set
val singleton : ('a -> 'a -> int) -> 'a -> 'a set
val cardinality : 'a set -> int
val member : 'a set -> 'a -> bool
val insert : 'a set -> 'a -> 'a set
val union : 'a set -> 'a set -> 'a set
val delete : 'a set -> 'a -> 'a set
val elements : 'a set -> 'a list
val is_empty : 'a set -> bool
val fold : ('a -> 'b -> 'b) -> 'a set -> 'b -> 'b
val app : ('a -> 'b) -> 'a set -> unit
val intersect : 'a set -> 'a set -> 'a set
val from_list : ('a -> 'a -> int) -> 'a list -> 'a set
val subset : 'a set -> 'a set -> bool
val diff : 'a set -> 'a set -> 'a set
val equals : 'a set -> 'a set -> bool
val choose : 'a set -> 'a
val print :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a set -> unit

(* EOF: set.mli *)

