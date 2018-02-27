(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Gcd - Generic Compiler Driver
 * Provides some common code for handling file types, compilation phases, etc.
 *)

let errors = ref false;;

(* Printing at various places *)

let printpoints = ref (Set.empty compare);;

let print_at s = printpoints := Set.insert !printpoints s;;

let do_print s prn =
  if Set.member !printpoints s then begin
    Printf.printf "*** Compilation point: %s\n" s;
    prn Format.std_formatter
  end
;;

(* Type checking at various places *)

let tcpoints = ref (Set.empty compare);;

let type_check_at s = tcpoints := Set.insert !tcpoints s;;

let do_type_check s tc =
  if Set.member !tcpoints s then begin
    Printf.printf "*** Type Checking: %s\n" s; flush stdout;
    tc Format.err_formatter; flush stderr;
    Printf.printf "*** Type Checking Complete\n"; flush stdout
  end
;;

(* Compilation phases *)

type comp_phase = FE | TAL | OBJ | EXE;;

let comp_phase = ref EXE;;
let set_elab_only () = comp_phase := FE;;
let set_generate_tal_only () = comp_phase := TAL;;
let set_assemble_only () = comp_phase := OBJ;;
let set_do_link () = comp_phase := EXE;;
let set_no_link () = if !comp_phase=EXE then comp_phase := OBJ;;

let code_generate_p () =
  match !comp_phase with FE -> false | TAL | OBJ | EXE -> true
;;
let assemble_p () =
  match !comp_phase with FE | TAL -> false | OBJ | EXE -> true
;;
let link_p () =
  match !comp_phase with FE | TAL | OBJ -> false | EXE -> true
;;

(* compiler filename basename modulename -> success? *)
type compiler = string -> string -> string -> bool;;
(* suffix,compiler *)
type file_type = string * compiler;;

let file_types = ref [];;
let set_file_types fts = file_types := fts;;

let compile_file filename : unit =
  let basename = Filename.chop_extension filename in
  (* added by Dan: *)
  let dirname = Filename.dirname filename in
  (if compare dirname "" <> 0 then Talout.add_include dirname);
  (* end of addition *)
  let modname = Filename.basename basename in
  let rec find_type types =
    match types with
      [] -> Printf.eprintf "%s: unknown file type\n" filename; flush stderr
    | (s,f)::types ->
	if Filename.check_suffix filename s then begin
	  let suc = f filename basename modname in
	  if not suc then begin errors:=true; set_no_link () end
	end else
	  find_type types in
  find_type !file_types
;;

let object_files = ref [];;
let add_object_file (ofn : string) = object_files := ofn :: !object_files;;
let get_object_files () = List.rev !object_files;;

let libraries = ref [];;
let add_library (libn : string) = libraries := libn :: !libraries;;
let get_libraries () = List.rev !libraries;;

let outputname = ref Talout.default_executable;;
let set_output_name on = outputname := on;;

let compile_object_file filename _ _ = add_object_file filename; true;;
let compile_library filename _ _ = add_library filename; true;;

let std_file_types =
  [ Talout.object_file_suffix,compile_object_file;
    Talout.library_suffix,compile_library
  ]
;;

let tooldesc = ref "";;
let set_tooldesc s = tooldesc := s;;

(* NG: this must be keep up to date.
 * Use x.x for the official release and x.x+ to indicate modifications since
 * the last official release.
 *)
let release = "1.6+";;

let copyright =
  "Copyright\n"^
  "    Greg Morrisett, Neal Glew, David Walker, Stephanie Weirich,\n"^
  "    Steve Zdancewic, Frederick Smith, Richard Samuels, and Dan Grossman\n"^
  "    July, September 1998, and January 1999, all rights reserved."
;;

let version () =
  Printf.printf "%s\nRelease %s\n%s\n\nRuntime directory: %s\n"
    !tooldesc release copyright !Talout.runtime;
  exit 0
;;

let short_options =
  [ "-v",Arg.Unit version,"output version and copyright information";
    "-T",Arg.Unit set_generate_tal_only, "generate TAL, do not assemble";
    "-c",Arg.Unit set_assemble_only,"assemble, do not link";
    "-I",Arg.String Talout.add_include,"add directory to search list";
    "-o",Arg.String set_output_name,
      "output name, default "^Talout.default_executable;
    "--runtime",Arg.String Talout.set_runtime,"set runtime directory";
    "--print-at",Arg.String print_at,"print ir at compilation point";
    "--type-check-at",Arg.String type_check_at,
      "type check ir at compilation point"
  ]
;;

let std_options =
  [ "-v",Arg.Unit version,"output version and copyright information";
    "--version",Arg.Unit version,"output version and copyright information";
    "--elaborate-only",Arg.Unit set_elab_only,"read and type check only";
    "--generate-tal-only",Arg.Unit set_generate_tal_only,
      "generate TAL, do not assemble";
    "-c",Arg.Unit set_assemble_only,"assemble, do not link";
    "-I",Arg.String Talout.add_include,"add directory to search list";
    "--runtime",Arg.String Talout.set_runtime,"set runtime directory";
    "--error-context",Arg.Set Gcdfec.print_context,"print error context";
    "--no-error-context",Arg.Clear Gcdfec.print_context,"print error context";
    "-o",Arg.String set_output_name,
      "output name, default "^Talout.default_executable;
    "--coff",Arg.Unit (fun () -> Talout.objformat:=Talout.COFF),
      "use coff object file format";
    "--elf",Arg.Unit (fun () -> Talout.objformat:=Talout.ELF),
      "use elf object file format";
    "--noisy-sys",Arg.Clear Talout.verbose_sys_commands,
      "show system commands";
    "--quiet-sys",Arg.Set Talout.verbose_sys_commands,
      "do not show system commands";
    "--print-at",Arg.String print_at,"print ir at compilation point";
    "--type-check-at",Arg.String type_check_at,
      "type check ir at compilation point"
  ]
;;

let driver toolname options file_types middle do_link =
  set_file_types file_types;
  Arg.parse options compile_file (toolname^": usage: <options> file ...");
  if not (middle ()) then begin errors:=true; set_no_link () end;
  if link_p () then
    if not (do_link (get_object_files ()) (get_libraries ()) !outputname) then
      errors:=true;
  if !errors then
    exit 255
  else
    exit 0
;;

(* EOF: gcd.ml *)
