(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dave Walker, Karl Crary,            *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

type ('a, 'b) splaytree =
   LEAF
 | NODE of ('a * 'b * ('a, 'b) splaytree * ('a, 'b) splaytree) ref
;;

type direction = LEFT | RIGHT ;;

let rotate_left node =
    match !node with
      key,data,NODE(node2),right -> 
          (match !node2 with
            left_key,left_data,left_left,left_right ->
              node := (left_key,left_data,left_left,
		       NODE(ref(key,data,left_right,right))))
    | _ -> invalid_arg "Splay.rotate_left"
;;

(*
let rotate_left (ref (key, data, NODE (ref (left_key, left_data, left_left, left_right)), right) as node) =  (* inex ok *)
   node := (left_key, left_data, left_left, NODE (ref (key, data, left_right, right)))
;;
*)

let rotate_right node =
    match !node with
      key,data,left,NODE(node2) ->
        (match !node2 with
          right_key,right_data,right_left,right_right ->
	    node := (right_key,right_data,
		     NODE(ref(key,data,left,right_left)),right_right))
    | _ -> invalid_arg "Splay.rotate_right"
;;
			       
(*
let rotate_right (ref (key, data, left, NODE (ref (right_key, right_data, right_left, right_right))) as node) =  (* inex ok *)
   node := (right_key, right_data, NODE (ref (key, data, left, right_left)), right_right)
;;
*)

let rec lift = function
   [] -> ()
 | [(LEFT, parent)] ->
      rotate_left parent
 | [(RIGHT, parent)] ->
      rotate_right parent
 | (LEFT, parent) :: (LEFT, grandparent) :: ancestors ->
      (
      rotate_left grandparent;
      rotate_left grandparent;  (* parent has moved into grandparent's position *)
      lift ancestors
      )
 | (RIGHT, parent) :: (RIGHT, grandparent) :: ancestors ->
      (
      rotate_right grandparent;
      rotate_right grandparent;  (* parent has moved into grandparent's position *)
      lift ancestors
      )
 | (LEFT, parent) :: (RIGHT, grandparent) :: ancestors ->
      (
      rotate_left parent;
      rotate_right grandparent;
      lift ancestors
      )
 | (RIGHT, parent) :: (LEFT, grandparent) :: ancestors ->
      (
      rotate_right parent;
      rotate_left grandparent;
      lift ancestors
      )
;;

let splay reln tree =
   let rec aux path = 
     function (NODE node) ->
       (match !node with 
         (key, data, left, right) ->
           let comp = reln key
           in
           if comp = 0 then
             (
             lift path;
             true
               )
           else if comp < 0 then
               (* left *)
             aux ((LEFT, node) :: path) left
           else
               (* right *)
             aux ((RIGHT, node) :: path) right)
       | LEAF ->
           (match path with
             [] -> false
           | _ :: path' ->
               (
               lift path';
               false
                 ))
   in
   aux [] tree
;;

let splay2 reln tree =
   let rec aux node (* path *) =
     match node with
       (NODE node) ->
	 (match !node with 
           (key, data, left, right) ->
             let comp = reln key
             in
             if comp < 0 then
               (* left *)
               aux (* ((LEFT, node) :: path) *) left
             else if comp > 0 then
               (* right *)
               aux (* ((RIGHT, node) :: path) *) right
             else Some data)
             (* (
               	lift path; 
                true
               ) *)
     | LEAF -> None (* false *)
(*         (match path with
           [] -> false
           | _ :: path' ->
           (
           lift path'; 
           false
           )) *)
   in
   aux (* [] *) tree
;;
