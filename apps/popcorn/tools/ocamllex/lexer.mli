(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  Automatique.  Distributed only by permission.                      *)
(*                                                                     *)
(***********************************************************************)

(* $Id: lexer.mli,v 1.1 1999/04/09 14:00:07 danieljg Exp $ *)

val main: Lexing.lexbuf -> Parser.token

exception Lexical_error of string * int * int

val line_num: int ref
val line_start_pos: int ref
