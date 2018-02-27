(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Chris Hawblitzel, Dan Grossman      *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

val optimize     : bool ref
val allocateregs : bool ref

val code_gen : string->Tal.int_ref->Tal.int_ref->
  (Popsyntax.top_decl list * Poptype.global_env)->
    (Tal.tal_imp * Tal.tal_int * Tal.tal_int)

