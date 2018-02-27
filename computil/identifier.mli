(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dave Walker,                        *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Generic Identifiers
 * To be used for variabels, type constructor variables, labels, etc.
 * Has uniquefication features.
 *)

(* An identifier consists of a "source" string and a number that can be used
 * to make it unique.  When an identifier is printed the source is printed
 * then a dollar then the number.
 *   id_make s n -> new identifier with s as source n as number, n must be >= 0
 *   id_new s -> new identifier and number that makes it unqiue wrt to all
 *               other calls of id_new & id_unique
 *   id_of_string s -> a identifier that prints as s
 *   id_unique i -> same source as i but number that makes it
 *                  unique wrt to all other calls of id_new & id_unique
 *)

type identifier;;

val id_make : string -> int -> identifier;;
val id_new : string -> identifier;;
val id_of_string : string -> identifier;;
val id_unique : identifier -> identifier;;

(* Get the source, get the print string, and print *)
val id_to_string : identifier -> string;;
val id_to_source : identifier -> string;;
val id_prn : Format.formatter -> identifier -> unit;;

(* For datastructures that require comparision functions *)
val id_compare : identifier -> identifier -> int;;

(* EOF: identifier.mli *)
