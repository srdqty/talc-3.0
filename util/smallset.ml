
(* A set implementation for very small sets. *)

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
    val partition : (elt -> bool) -> t -> (t*t)
    val such_that : (elt -> bool) -> t -> t
    val for_all : (elt -> bool) -> t -> bool
    val to_set : (elt -> elt option) -> t -> t
  end

module Make(Ord:OrderedType): (S with type elt = Ord.t) =
  struct
    
    type elt = Ord.t
(* We maintain the list in sorted ascending order. *)
    type t = elt list
	  
	  
    let empty = ([]:t)
    let singleton e = ([e]:t)
    let cardinality (l:t) = List.length l
	
    let cmp : elt -> elt -> int = Ord.compare
    let less e1 e2 = (cmp e1 e2 < 0)
    let greater e1 e2 = (cmp e1 e2 > 0)
    let equal e1 e2 = (cmp e1 e2 = 0)
	
    let member es e =
      let rec aux ls =
	match ls with
	  hd::tl ->
	    let c = cmp hd e in
	    if c=0 then true
	    else if c>0 then false
	    else aux tl
	| [] -> false
      in
      aux es
	
    let insert es e =
      let rec aux ls = 
	match ls with
	  hd :: tl -> 
	    if equal hd e then ls
	    else if greater hd e then e :: ls
	    else hd :: aux tl
	| [] -> [e]
      in
      aux es
	
    let delete es e =
      let rec aux ls = 
	match ls with
	  [] -> []
	| hd::tl when equal hd e -> tl
	| hd::tl -> hd :: (aux tl)
      in
      aux es
	
	
    let elements es = es 
	
    let from_list es = List.fold_left insert [] es
	
	
    let is_empty es = (es = [])
	
    let fold = List.fold_right
    let fold_left = List.fold_left

    let iter = List.iter
	
	
    let rec getGE e l =
      match l with
	[] -> []
      | hd::tl -> 
	  if less hd e then getGE e tl
	  else l
	      
	      
    let union es1 es2 =
      begin
	let rec aux l1 l2 =
	  match l1,l2 with
	    (l1',[]) -> l1'
	  | ([],l2') -> l2'
	  | (hd1::tl1,hd2::tl2) ->
	      let c = cmp hd1 hd2 in
	      if c=0 then hd1 :: (aux tl1 tl2)
	      else if c<0 then hd1 :: (aux tl1 l2)
	      else hd2 :: (aux l1 tl2)
	in
	aux es1 es2
      end
	
    let intersect es1 es2 =
      begin
	let rec aux l1 l2 =
	  match l1,l2 with
	    (_,[]) -> []
	  | ([],_) -> []
	  | (hd1::tl1,hd2::tl2) ->
	      let c = cmp hd1 hd2 in
	      if c=0 then hd1 :: (aux tl1 tl2)
	      else if c<0 then aux (getGE hd2 tl1) l2
	      else aux l1 (getGE hd1 tl2)
	in
	aux es1 es2
      end
	
    let diff es1 es2 =
      let rec aux l1 l2 =
	match l1,l2 with
	  ([],_) -> []
	| (l1',[]) -> l1'
	| (hd1::tl1,hd2::tl2) ->
	    let c = cmp hd1 hd2 in
	    if c=0 then aux tl1 tl2
	    else if c<0 then hd1 :: (aux tl1 l2)
	    else aux l1 (getGE hd1 tl2)
      in
      aux es1 es2
      
      
    let subset es1 es2 = 
      let rec aux l1 l2 =
	match l1,l2 with
	  ([],l2') -> true
	| (_,[]) -> false
	| (hd1::tl1,hd2::tl2) ->
	    let c = cmp hd1 hd2 in
	    if c=0 then aux tl1 tl2
	    else if c<0 then aux (getGE hd2 tl1) l2
	    else aux l1 (getGE hd1 tl2)
      in
      aux es1 es2
	
    let equals es1 es2 = 
      let rec aux l1 l2 =
	match l1,l2 with
	  ([],[]) -> true
	| (hd1::tl1,hd2::tl2) when equal hd1 hd2 -> aux tl1 tl2
	| _ -> false
      in
      aux es1 es2

    let choose es =
      begin match es with
	hd::tl -> hd
      |	[] -> failwith "Empty set."
      end

    let split es1 es2 = 
      (* split A B return (A\B,A intersect B, B\A) *)
      let rec aux l1 l2 =
	match l1,l2 with
	  l1',[] -> (l1',[],[])
	| [],l2' -> ([],[],l2')
	| (hd1::tl1,hd2::tl2) ->
	    let c = cmp hd1 hd2 in
	    if c = 0 then 
	      let (ab,iab,ba) = aux tl1 tl2 in
	      (ab,hd1::iab,ba)
	    else if c<0 then 
	      let (ab,iab,ba) = aux tl1 l2 in
	      (hd1::ab,iab,ba)
	    else 
	      let (ab,iab,ba) = aux l1 tl2 in
	      (ab,iab,hd2::ba)	 
      in
      aux es1 es2
	      
      let partition p es =
	let rec aux ls =
	  match ls with
	    [] -> ([],[])
	  | hd::tl -> 
	      let (ptrue,pfalse) = aux tl in
	      if p hd then (hd :: ptrue,pfalse) else (ptrue,hd :: pfalse)
	in
	aux es

    let such_that p es =
      let rec aux ls =
	match ls with
	  [] -> []
	| hd::tl -> if p hd then hd :: (aux tl) else aux tl
      in
      aux es

    let to_set f es = 
      let rec aux ls = 
	match ls with 
	  [] -> []
	| hd :: tl -> 
	    (match f hd with Some hd' -> hd' :: (aux tl) | None -> aux tl)
      in from_list (aux es)

    let for_all = List.for_all

  end



(* Test Code. *)
(*
module SS = Make(struct type t = int let compare = compare end)

let t1 = SS.from_list [7;6;5;8];;
let t2 = SS.from_list [2;4;3;6];;
let t3 = SS.intersect t1 t2;;
let t4 = SS.union t1 t2;;

let t5 = SS.diff t1 t2
let (t6,t7,t8) = SS.split t1 t2

let pr t = 
  print_string "[";
  SS.iter (fun i -> print_int i; print_string " ") t;
  print_string "]\n"

let main () =
  print_string "t1 = ";
  pr t1;
  print_string "t2 = ";
  pr t2;
  print_string "t1 /\\ t2 = ";
  pr t3;
  print_string "t1 \\/ t2 = ";
  pr t4;
  print_string "t1 \ t2 = ";
  pr t5;

 print_string "t1 \ t2  = ";
  pr t6;
 print_string "t1 /\\ t2 = ";
  pr t7;
 print_string "t2 \ t1 = ";
  pr t8;
  ()
;;

main ()
*)

