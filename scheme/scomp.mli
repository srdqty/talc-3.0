(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Stephanie Weirich,                  *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

val code_gen : Sil.prog -> Tal.tal_imp
val print_comments : bool ref
val string_of_op : Sast.primop -> string
