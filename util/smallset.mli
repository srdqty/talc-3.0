
(* For very small sets. *)

module type OrderedType =
  sig
    type t
    val compare: t -> t -> int
  end

module type S =
  sig
    type elt
    type t
    val empty : t
    val singleton : elt -> t
    val cardinality : t -> int
    val member : t -> elt -> bool
    val insert : t -> elt -> t
    val union : t -> t -> t
    val delete : t -> elt -> t
    val elements : t -> elt list
    val is_empty : t -> bool
    val fold : (elt -> 'b -> 'b) -> t -> 'b -> 'b
    val fold_left : ('a -> elt -> 'a) -> 'a -> t -> 'a
    val iter : (elt -> unit) -> t -> unit
    val intersect : t -> t -> t
    val from_list : elt list -> t
    val subset : t -> t -> bool
    val diff : t -> t -> t
    val equals : t -> t -> bool
    val choose : t -> elt
    val split : t -> t -> (t*t*t)
    val partition : (elt->bool) -> t -> (t * t)
    val such_that : (elt -> bool) -> t -> t
    val for_all : (elt->bool) -> t -> bool
    val to_set : (elt->elt option) -> t -> t
  end

module Make(Ord:OrderedType): (S with type elt = Ord.t)
