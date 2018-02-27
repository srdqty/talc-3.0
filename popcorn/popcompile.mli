(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Chris Hawblitzel,                   *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)
(* this flag controls whether or not the file is compiled with extra
 * code for debugging malloc.  In particular, it stores the filename
 * and line number for the module in a global before each call to
 * malloc.
 *)
val debug_malloc : bool ref 

val code_gen : string->Tal.int_ref->Tal.int_ref->bool->
  (Popsyntax.top_decl list * Poptype.global_env)->
    (Tal.tal_pre_mod * Tal.tal_int * Tal.tal_int)

