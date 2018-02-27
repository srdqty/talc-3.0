
(* Hacked by Dan to spit out Popcorn. *)

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

(* $Id: output.ml,v 1.3 1999/06/12 17:58:49 danieljg Exp $ *)

(* Output the DFA tables and its entry points *)

open Printf
open Syntax
open Lexgen
open Compact

(* To copy the ML code fragments *)

let copy_buffer = String.create 1024

let copy_chars_unix ic oc start stop =
  let n = ref (stop - start) in
  while !n > 0 do
    let m = input ic copy_buffer 0 (min !n 1024) in
    output oc copy_buffer 0 m;
    n := !n - m
  done

let copy_chars_win32 ic oc start stop =
  for i = start to stop - 1 do
    let c = input_char ic in
    if c <> '\r' then output_char oc c
  done

let copy_chars =
  match Sys.os_type with
    "Win32" -> copy_chars_win32
  | _       -> copy_chars_unix

let copy_chunk sourcefile ic oc loc =
  if loc.start_pos < loc.end_pos then begin
    fprintf oc "#line %d \"%s\"\n" loc.start_line sourcefile;
    for i = 1 to loc.start_col do output_char oc ' ' done;
    seek_in ic loc.start_pos;
    copy_chars ic oc loc.start_pos loc.end_pos
  end

(* To output an array of short ints, encoded as a string *)

let output_byte oc b =
  output_char oc '\\';
  output_char oc (Char.chr(48 + b / 100));
  output_char oc (Char.chr(48 + (b / 10) mod 10));
  output_char oc (Char.chr(48 + b mod 10))

(*let output_array oc v =
  output_string oc "   \"";
  for i = 0 to Array.length v - 1 do
    output_byte oc (v.(i) land 0xFF);
    output_byte oc ((v.(i) asr 8) land 0xFF);
    if i land 7 = 7 then output_string oc "\\\n    "
  done;
  output_string oc "\""*)

let output_array oc v = (* doesn't work for really short arrays *)
  output_string oc "{";
  for i = 0 to Array.length v - 2 do
    output_string oc ((string_of_int v.(i))^",")
  done;
  output_string oc (string_of_int v.(Array.length v - 1));
  output_string oc "}"

(* Output the tables *)

let output_tables oc tbl =
  output_string oc "lex_tables lex_tables = ^lex_tables(\n";
  fprintf oc "  \n%a,\n" output_array tbl.tbl_base;
  fprintf oc "  \n%a,\n" output_array tbl.tbl_backtrk;
  fprintf oc "  \n%a,\n" output_array tbl.tbl_default;
  fprintf oc "  \n%a,\n" output_array tbl.tbl_trans;
  fprintf oc "  \n%a\n" output_array tbl.tbl_check;
  output_string oc ");\n\n"

(* Output the entries *)

let output_entry sourcefile ic oc e =
  fprintf oc "int %s<a>(<a>lexbuf lexbuf) { return %s_rec(lexbuf,%d); }\n"
          e.auto_name e.auto_name e.auto_initial_state;
  fprintf oc "int %s_rec<a>(<a>lexbuf lexbuf, int lexstate) {\n" e.auto_name;
  (* save new state, then switch on it, so default case can use it.
     (int switch can't get the value being switched on) *)
  fprintf oc "  lexstate = lex_engine(lex_tables,lexstate,lexbuf);";
  fprintf oc "  switch lexstate {\n";
  List.iter
    (fun (num, loc) ->
      fprintf oc "\ncase %d: \n" num;
      copy_chunk sourcefile ic oc loc)
    e.auto_actions;
  fprintf oc 
    " default: lexbuf.refill_buff(lexbuf); return %s_rec(lexbuf,lexstate);\n"
    e.auto_name;
  fprintf oc "  }\n raise Error(\"some action didn't return!\");\n}\n"

(* Main output function *)

exception Table_overflow

let output_lexdef sourcefile ic oc header tables entry_points trailer =
  Printf.printf "%d states, %d transitions, table size %d bytes\n"
    (Array.length tables.tbl_base)
    (Array.length tables.tbl_trans)
    (2 * (Array.length tables.tbl_base + Array.length tables.tbl_backtrk +
          Array.length tables.tbl_default + Array.length tables.tbl_trans +
          Array.length tables.tbl_check));
  flush stdout;
  if Array.length tables.tbl_trans > 0x8000 then raise Table_overflow;
  copy_chunk sourcefile ic oc header;
  output_string oc "open Lexing {\n\n";
  output_tables oc tables;
  List.iter (fun e -> output_entry sourcefile ic oc e) entry_points;
  output_string oc "\n}\n";
(*  begin match entry_points with
    [] -> ()
  | entry1 :: entries ->
      output_string oc "let rec "; output_entry sourcefile ic oc entry1;
      List.iter
        (fun e -> output_string oc "and "; output_entry sourcefile ic oc e)
        entries
  end; *)
  copy_chunk sourcefile ic oc trailer
