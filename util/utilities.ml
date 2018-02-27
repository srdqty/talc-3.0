(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dave Walker,                        *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Some generic stuff *)

type 'a vector = 'a array;;

let vector_exists f v =
  let rec aux f v i l =
    if i>=l then false else f v.(i) or aux f v (i+1) l in
  aux f v 0 (Array.length v)
;;

let vector_fold = Array.fold_left;;

(* Given an array a[0]...a[n] computes f a[0] (f a[1] (...(f a[n] b))) *)
let vector_fold_rev f a b =
  let rec loop acc n =
    if n = -1 then acc else loop (f a.(n) acc) (n-1)
  in
  loop b ((Array.length a) - 1)

(* convert a vector to a list and append it to another list *)
let vector_append vec lis = vector_fold_rev (fun x lis -> x::lis) vec lis
;;

let compare_strings = compare;;

let revappend l1 l2 =
   let rec aux accum = function
      [] -> accum
    | x :: r ->
         aux (x :: accum) r
   in
      aux l2 l1
;;

let rec foldmap_left f a l =
  match l with
    [] -> ([],a)
  | i::l -> let (i,a) = f a i in let (l,a) = foldmap_left f a l in (i::l,a)
;;

let rec fold_left3 f root l1 l2 l3 =
  match l1,l2,l3 with
    [],[],[] -> root
  | (h1::t1),(h2::t2),(h3::t3) -> fold_left3 f (f root h1 h2 h3) t1 t2 t3
  | _,_,_ -> raise (Invalid_argument "Utilities.fold_left3")
;;

let rec fold_right3 f l1 l2 l3 root =
  match l1,l2,l3 with
    [],[],[] -> root
  | (h1::t1),(h2::t2),(h3::t3) ->
      f h1 h2 h3 (fold_right3 f t1 t2 t3 root)
  | _,_,_ -> raise (Invalid_argument "Utilities.fold_right3")
;;

let rec first f = function
   [] -> None
 | x :: rest ->
      (match f x with
          None -> first f rest
        | Some _ as res -> res)
;;

let nfirst f l =
   let rec aux n = function
      [] -> None
    | x :: rest ->
         (match f x n with
             None -> aux (n+1) rest
           | Some _ as res -> res)
   in
      aux 0 l
;;

let compose f g x = f (g x) ;;
let pair f g (x, y) = (f x, g y) ;;
let id x = x ;;

let replicate x n =
   let rec aux l n =
      if n <= 0 then
         l
      else
         aux (x :: l) (n-1)
   in
      aux [] n
;;

let flist f n =
   let rec aux l n =
      if n < 0 then
         l
      else
         aux (f n :: l) (n-1)
   in
      aux [] (n-1)
;;

let flist2 f n =
   let rec aux l1 l2 n =
      if n < 0 then
         (l1, l2)
      else
         let (x1, x2) = f n
         in
            aux (x1 :: l1) (x2 :: l2) (n-1)
   in
      aux [] [] (n-1)
;;

let flist3 f n =
   let rec aux l1 l2 l3 n =
      if n < 0 then
         (l1, l2, l3)
      else
         let (x1, x2, x3) = f n
         in
            aux (x1 :: l1) (x2 :: l2) (x3 :: l3) (n-1)
   in
      aux [] [] [] (n-1)
;;

let rec iter f x n =
   if n <= 0 then
      x
   else
      iter f (f x) (n-1)
;;

let rec itern f x n =
   if n <= 0 then
      x
   else
      itern f (f x (n-1)) (n-1)
;;

let rev_itern f x stop =
   let rec aux x n =
      if n >= stop then
         x
      else
         aux (f x n) (n+1)
   in
      if stop < 0 then
         x
      else
         aux x 0
;;

let filter f l =
   let rec aux = function
      [] -> []
    | h :: t ->
         if f h then
            h :: aux t
         else
            aux t
   in
      aux l
;;

let filter_map f l =
   let rec aux = function
      [] -> []
    | h :: t ->
         (match f h with
             None -> aux t
           | Some h' ->
                h' :: aux t)
   in
      aux l
;;

exception RemoveNth ;;
let rec remove_nth a b = 
 match a,b with
   _, [] -> raise RemoveNth
 | 0, (h :: t) ->
      (h, t)
 | n, (h :: t) ->
      let (x, t') = remove_nth (n-1) t
      in
         (x, h :: t')
;;

let app_num f n =
   let rec aux i =
      if i >= n then
         ()
      else
         (
         f i;
         aux (i+1)
         )
   in
      aux 0
;;

(* infix /+, pronounced "div-ceil":
   a /+ b  =  ceiling (a / b) *)
let (/+) a b = ((a - 1) / b) + 1;;

(* EOF: utilities.ml *)
