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

(* $Id: output.mli,v 1.1 1999/04/09 14:00:07 danieljg Exp $ *)

(* Output the DFA tables and its entry points *)

val output_lexdef:
      string -> in_channel -> out_channel ->
      Syntax.location ->
      Compact.lex_tables ->
      Lexgen.automata_entry list ->
      Syntax.location ->
      unit

exception Table_overflow
