(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dave Walker, Karl Crary,            *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

open Utilities ;;
open Splay ;;

exception Absent ;;
exception Present ;;

type ('a, 'b) dict = {reln : 'a -> 'a -> int; tree : ('a, 'b) splaytree} ;;

let empty r = {reln = r; tree = LEAF} ;;
let is_empty {reln = r;tree = t} =
  match t with
    LEAF -> true
  | _ -> false

let choose {reln=r;tree=t} =
  match t with
    LEAF -> raise Absent
  | NODE theref ->
      (match !theref with
	(a, b, _, _) -> (a, b))
;;

let member {reln = r; tree = t} key =
   splay (r key) t
;;

let insert {reln = r; tree = t} key data =
   if splay (r key) t then
      (match t with
          NODE theref -> 
	    (match !theref with
              (_, _, left, right) ->
                 {reln = r; tree = NODE (ref (key, data, left, right))})
        | LEAF -> failwith "insert")
   else
      (match t with
          NODE theref ->
           (match !theref with
            (key', data', left, right) ->
             if r key key' < 0 then
                (* left *)
                {reln = r; tree = NODE (ref (key, data, left, NODE (ref (key', data', LEAF, right))))}
             else
                {reln = r; tree = NODE (ref (key, data, NODE (ref (key', data', left, LEAF)), right))})
        | LEAF ->
             {reln = r; tree = NODE (ref (key, data, LEAF, LEAF))})
;;

let inserts dict items =
   List.fold_right (fun (key, data) dict -> insert dict key data) items dict
;;

let insert_new {reln = r; tree = t} key data =
   if splay (r key) t then
      raise Present
   else
      (match t with
          NODE theref ->
           (match !theref with 
	    (key', data', left, right) ->
             if r key key' < 0 then
                (* left *)
                {reln = r; tree = NODE (ref (key, data, left, NODE (ref (key', data', LEAF, right))))}
             else
                {reln = r; tree = NODE (ref (key, data, NODE (ref (key', data', left, LEAF)), right))})
        | LEAF ->
             {reln = r; tree = NODE (ref (key, data, LEAF, LEAF))})
;;

let singleton r key data =
   {reln = r;
    tree = NODE (ref (key, data, LEAF, LEAF))}
;;

let lookup {reln = r; tree = t} key =
  match splay2 (r key) t with
    None -> raise Absent
  | Some x -> x
(*
   if splay (r key) t then
      (match t with
          NODE n -> (match !n with 
                     (_, data, _, _) -> data)
        | LEAF -> failwith "lookup")
   else
      raise Absent
*)
;;

let delete ({reln = r; tree = t} as st) key =
   if splay (r key) t then
      (match t with
       NODE theref ->
       (match !theref with
          (_, _, LEAF, right) ->
             {reln = r; tree = right}
        | (_, _, left, LEAF) ->
             {reln = r; tree = left}
        | (_, _, left, right) ->
             (
             splay (fun _ -> 1) left;
             match left with
	       NODE theref ->
	         (match !theref with
                  (left_key, left_data, left_left, LEAF) ->
                   { reln = r;
		     tree = NODE (ref (left_key,left_data,left_left,right))}
		 | _ -> failwith "delete")
              | _ -> failwith "delete"
             ))
       | _ -> failwith "delete")
   else
      st
;;

let delete_present {reln = r; tree = t} key =
   if splay (r key) t then
      (match t with
       NODE theref ->  
        (match !theref with
          (_, _, LEAF, right) -> {reln = r; tree = right}
        | (_, _, left, LEAF) -> {reln = r; tree = left}
        | (_, _, left, right) ->
           begin
             splay (fun _ -> 1) left;
             match left with
	        NODE theref2 ->
		  (match !theref2 with
                   (left_key, left_data, left_left, LEAF) ->
                        {reln = r; tree = NODE (ref (left_key, left_data, 
			                             left_left, right))}
		  | _ -> failwith "delete")
              | _ -> failwith "delete"
           end)
        | _ -> failwith "delete")
   else
      raise Absent
;;

(*
let rec tab = function
   0 -> ()
 | n -> (print_string " "; tab (n-1))
;;

let print_tree {reln = _; tree = t} =
   let rec aux indent increment = function
      NODE theref ->
       (match !theref with
        (key, data, left, right) ->
         (
         tab indent;
         print_int key;
         print_newline ();
         aux (indent-increment) (increment/2) left;
         aux (indent+increment) (increment/2) right
         ))
    | LEAF ->
         ()

   in
      aux 100 50 t
;;
*)

let fold_dict f dict root =
   let rec aux input = function
      LEAF -> input
    | NODE theref ->
      (match !theref with
         (key, data, left, right) -> aux (f key data (aux input right)) left)
   in
      aux root (dict.tree)
;;

let app_dict f dict =
   let rec aux = function
      LEAF -> ()
    | NODE theref ->
       (match !theref with
         (key, data, left, right) ->
            begin
              aux left;
              f key data;
              aux right
            end)
   in
      aux (dict.tree)
;;

let map_dict f {reln = r; tree = t} =
   let rec aux = function
      LEAF -> LEAF
    | NODE theref ->
      (match !theref with
        (key, data, left, right) ->
         NODE (ref (key, f data, aux left, aux right)))
   in
      {reln = r; tree = aux t}
;;

let update d1 d2 =
  let aux k v d = insert d k v in
  fold_dict aux d2 d1
;;

open Format;;

let print prn_key prn_data f {tree=t} =
  let rec aux t first =
    match t with
      LEAF -> ()
    | NODE {contents=(k,d,left,right)} ->
	if not first then fprintf f ",@ ";
	fprintf f "@[<hv>"; prn_key f k; fprintf f "=@;<0 2>"; prn_data f d;
	fprintf f "@]"; aux left false; aux right false in
  fprintf f "{@[<hov>"; aux t true; fprintf f "@]}"
;;

(* EOF: dict.ml *)
