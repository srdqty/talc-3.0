(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Stephanie Weirich,                  *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

open Talout;;
open Gcd;;

let set_no_print_comments() = Scomp.print_comments := false;;

let compile_scheme_file filename basename modname =
  let talfile = basename ^ ".tal" in
  let objectfile = basename ^ ".obj" in
  let executable = basename ^ ".exe" in
  Printf.printf "Compiling file: %s\n" basename; flush stdout;
  try
    let syntaxTree = Gcdfe.fe Slex.token Sparse.prog filename in
    if Gcdfec.error_p () then raise Gcdfec.Exit;
    let ilTree = Sil.xprog syntaxTree in
    let tal_imp = Scomp.code_gen ilTree in
    Talout.write_imp modname talfile tal_imp;
    if assemble_p () then begin
      if not (Talout.verify_and_asm talfile (Some objectfile)) then
 	raise Gcdfec.Exit;
      if link_p () then
	let objfiles =
	  [ "sclib"^Talout.object_file_suffix;
	    "stdlib"^Talout.object_file_suffix;
	    "cyclonelib"^Talout.object_file_suffix;
            objectfile(* DJG *)
	  ] in
	add_object_file objectfile; (* DJG *)
      	Talout.link objfiles executable
      else
	true
    end else
      Talout.verify talfile
  with 
    Sys_error str ->
      Printf.eprintf "System error: %s\n" str; flush stderr; false
  | Gcdfec.Exit -> false
;;

let toolname = "scheme";;
set_tooldesc "scheme: JGM's version of scheme compiler";;

let options = std_options @
  ["-C",Arg.Unit set_no_print_comments, "do not print comments in TAL code"]
;;

let file_types = [".s",compile_scheme_file;".ss",compile_scheme_file];;

let middle () =  (* DJG *)
  add_object_file ("sclib"^object_file_suffix);
  add_object_file ("stdlib"^object_file_suffix);
(* Cyclone *)
  add_object_file ("cyclonelib"^object_file_suffix);
(* End Cyclone *)
  true;;

let do_link objfiles libraries executable = true;;

let main () = driver toolname options file_types middle do_link;;

Printexc.catch main () ;;

(* EOF: scheme.ml *)
