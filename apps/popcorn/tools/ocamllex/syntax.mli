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

(* $Id: syntax.mli,v 1.1 1999/04/09 14:00:07 danieljg Exp $ *)

(* The shallow abstract syntax *)

type location =
    { start_pos: int;
      end_pos: int;
      start_line: int;
      start_col: int }

type regular_expression =
    Epsilon
  | Characters of int list
  | Sequence of regular_expression * regular_expression
  | Alternative of regular_expression * regular_expression
  | Repetition of regular_expression

type lexer_definition =
    { header: location;
      entrypoints: (string * (regular_expression * location) list) list;
      trailer: location }
