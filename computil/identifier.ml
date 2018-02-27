(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dave Walker,                        *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Generic Identifiers
 * To be used for variabels, type constructor variables, labels, etc.
 * Has uniquefication features.
 *)

(* Hacked by Dan to share strings *)

type hash_string = string * int

type identifier = hash_string * int  (* n=-1 means print source only *)

let hash_compare (s1,i1) (s2,i2) =
  if i1 = i2
  then compare s1 s2
  else i1-i2

let memo = ref (Dict.empty hash_compare)
    
let id_make s n =
  let h = (s,Hashtbl.hash s) in
  try (Dict.lookup !memo h,n)
  with Dict.Absent -> (memo := Dict.insert !memo h h; (h,n))

let counter = ref (-1)

let id_new       s         = incr counter; id_make s !counter
let id_unique    ((s,_),_) = id_new s
let id_of_string s         = id_make s (-1)
let id_to_string ((s,_),n) = if n=(-1) then s else s^"$"^(string_of_int n)
let id_to_source ((s,_),_) = s

let id_prn fmt id = Format.pp_print_string fmt (id_to_string id)

(* N.B. order of compares is probably more efficient *)
let id_compare (d1,n1) (d2,n2) =
  if n1=n2 then
    (if d1==d2 then 0 else hash_compare d1 d2)
  else n1-n2

(* EOF: identifier.ml *)
