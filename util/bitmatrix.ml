(**********************************************************************)
(* (c) Greg Morrisett, Steve Zdancewic                                *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* bitmatrix.ml
 *
 * The datatype for extensible bitmatrices.
 *)

open Identifier

type bit_matrix = (identifier, (identifier, unit) Hashtbl.t) Hashtbl.t

let size = 29

let create () =
  Hashtbl.create size

(* Sets an entry to true *)
let set bm id1 id2 = 
  try 
    let ht = Hashtbl.find bm id1 in 
    begin
      Hashtbl.remove ht id2;
      Hashtbl.add ht id2 ();
    end
  with Not_found -> 
    let ht = Hashtbl.create size in
    begin
      Hashtbl.add ht id2 ();
      Hashtbl.add bm id1 ht;
    end

(* Sets an entry to false *)
let clear bm id1 id2 =
  try 
    let ht = Hashtbl.find bm id1 in
    Hashtbl.remove ht id2
  with Not_found -> ()

(* Gets the value of an entry.  Uninitialized default to false. *)
let get bm id1 id2 =
  try
    let ht = Hashtbl.find bm id1 in
    let _ = Hashtbl.find ht id2 in
    true
  with Not_found -> false

(* EOF: bitmatrix.ml *)
