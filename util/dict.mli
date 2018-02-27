(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dave Walker, Karl Crary,            *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

type ('a, 'b) dict ;;

exception Present;;
exception Absent;;
val empty : ('a -> 'a -> int) -> ('a, 'b) dict
val is_empty : ('a, 'b) dict -> bool
val member : ('a, 'b) dict -> 'a -> bool
val insert : ('a, 'b) dict -> 'a -> 'b -> ('a, 'b) dict
val inserts : ('a, 'b) dict -> ('a * 'b) list -> ('a, 'b) dict
val insert_new : ('a, 'b) dict -> 'a -> 'b -> ('a, 'b) dict
val singleton : ('a -> 'a -> int) -> 'a -> 'b -> ('a, 'b) dict
val lookup : ('a, 'b) dict -> 'a -> 'b
val delete : ('a, 'b) dict -> 'a -> ('a, 'b) dict
val delete_present : ('a, 'b) dict -> 'a -> ('a, 'b) dict
val fold_dict : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) dict -> 'c -> 'c
val app_dict : ('a -> 'b -> 'c) -> ('a, 'b) dict -> unit
val map_dict : ('b -> 'c) -> ('a, 'b) dict -> ('a, 'c) dict
val choose : ('a, 'b) dict -> 'a * 'b
val update : ('a, 'b) dict -> ('a, 'b) dict -> ('a, 'b) dict
val print :
    (Format.formatter -> 'a -> unit) -> (Format.formatter -> 'b -> unit) ->
      Format.formatter -> ('a,'b) dict -> unit

(* EOF: dict.mli *)
