(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dave Walker, Dan Grossman           *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Some generic stuff *)

type 'a vector = 'a array
val vector_exists : ('a -> bool) -> 'a array -> bool
val vector_fold : ('a -> 'b -> 'a) -> 'a -> 'b array -> 'a
val vector_fold_rev : ('a -> 'b -> 'b) -> 'a array -> 'b -> 'b
val vector_append : 'a vector -> 'a list -> 'a list
val compare_strings : 'a -> 'a -> int
val revappend : 'a list -> 'a list -> 'a list
val foldmap_left : ('a -> 'b -> 'c * 'a) -> 'a -> 'b list -> 'c list * 'a
val fold_left3 :
  ('a -> 'b -> 'c -> 'd -> 'a) -> 'a -> 'b list -> 'c list -> 'd list -> 'a
val fold_right3 :
  ('a -> 'b -> 'c -> 'd -> 'd) -> 'a list -> 'b list -> 'c list -> 'd -> 'd
val first : ('a -> 'b option) -> 'a list -> 'b option
val nfirst : ('a -> int -> 'b option) -> 'a list -> 'b option
val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
val pair : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
val id : 'a -> 'a
val replicate : 'a -> int -> 'a list
val flist : (int -> 'a) -> int -> 'a list
val flist2 : (int -> 'a * 'b) -> int -> 'a list * 'b list
val flist3 : (int -> 'a * 'b * 'c) -> int -> 'a list * 'b list * 'c list
val iter : ('a -> 'a) -> 'a -> int -> 'a
val itern : ('a -> int -> 'a) -> 'a -> int -> 'a
val rev_itern : ('a -> int -> 'a) -> 'a -> int -> 'a
val filter : ('a -> bool) -> 'a list -> 'a list
val filter_map : ('a -> 'b option) -> 'a list -> 'b list
exception RemoveNth
val remove_nth : int -> 'a list -> 'a * 'a list
val app_num : (int -> 'a) -> int -> unit
val (/+) : int -> int -> int

