(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Gcdfe - Generic Compiler Front End Driver
 * Provides some common code for doing parsing & lexing
 *)

open Gcdfec;;

let fe lexfun parsefun filename =
  reset_fe filename;
  try
    let ic = open_in_bin filename in
    try
      let i = parsefun lexfun (Lexing.from_channel ic) in
      if error_p () then raise Exit;
      close_in ic;
      i
    with
      Parsing.Parse_error -> close_in ic; raise Exit
    | x -> close_in ic; raise x
  with
    Sys_error s ->
      Printf.eprintf "%s: %s\n" filename s; flush stderr; raise Exit
;;

(* EOF: gcdfe.ml *)
