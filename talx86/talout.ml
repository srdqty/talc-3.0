(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dan Grossman                        *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* talout.ml
 * Useful functions for programs to write TAL modules out and call an
 * external verifier/assembler/linker.
 *
 *)

(* RLS 4/23/99 - 5/7/99: Added support for GNU linking under Linux. *)

(* Set to true to produce an "annots" file. *)
let do_write_dasm_info = ref false;;

(* set this to register trusted runtime symbols with dynamic loader *)
let register_trusted_syms = ref false;;

(* set this to pass the /DEBUG flag to the linker -- good for debugging *)
let link_debug = ref false;;

(* times the initialization cost of dynamic loading *)
let time_init = ref false;;

type bintool = MS | TALC | GNU;; 
type binformat = COFF | ELF;;

let object_file_suffix,library_suffix,default_executable,
   asm_bintool,link_bintool,cc_bintool,objformat =
  match Sys.os_type with
    "Unix" -> ".o",".a","a.out",ref TALC,ref GNU,ref GNU,ref ELF
  | "Win32" -> ".obj",".lib","a.exe",ref TALC,ref MS,ref MS,ref COFF
  | _ -> Printf.eprintf "Unknown operating system\n"; exit 1
;;

(* Files that make up the TAL runtime *)
let runtime_objfiles = 
  List.map (function lib -> lib^object_file_suffix) ["tal_start"; "tal_util"]
;;
let runtime_libs _ = "gc" :: (if !register_trusted_syms then [ "objlib"; "dynlinklib"] else [])
;;

(* Files that may register trusted symbols with the loader *)
let loader_initfiles = 
  List.map (function lib -> lib^object_file_suffix) 
    ["tal_start"; "cyclonelib"; "stdlib"; "pop_runtime" ]

(* Runtime library directory *)
let runtime =
  ref (try Sys.getenv "TALCLIB" with Not_found -> Filename.current_dir_name)
;;
let set_runtime s = runtime := s;;

(* Modified by Dan to avoid inserting duplicates *)
let includes = ref [];;
let add_include p = 
  if not (List.mem p (!includes))
  then includes := p :: !includes
  else ()

let ri_options () =
  let aux s p = " -I "^p^s in
  " --runtime "^(!runtime)^(List.fold_left aux "" !includes)
;;

let write_options = ref { Talpp.std_options with Talpp.expand_abbrevs = true } (* TEMPORARY; CYCLONE/MASM *)

let write_int modname filename talint =
  Format.set_margin 9999;
  Format.set_max_boxes 9999;
  let oc = open_out filename in
  Format.set_formatter_out_channel oc;
  Talpp.print_tal_int Format.std_formatter
    (!write_options) (* TEMPORARY; CYCLONE/MASM *)
    modname talint;
  Format.print_flush ();
  Format.set_formatter_out_channel stdout;
  close_out oc;
  Format.set_margin 79;
  ()
;;

let write_pre_mod modname filename talmod =
  Format.set_margin 9999;  (* JGM: too large a value causes stack overflow *)
  let oc = open_out filename in
  Format.set_formatter_out_channel oc;
  Talpp.print_tal_pre_mod Format.std_formatter
    (!write_options) (* TEMPORARY; CYCLONE/MASM *)
    modname talmod;
  Format.print_flush ();
  Format.set_formatter_out_channel stdout;
  close_out oc;
  Format.set_margin 79;
  ()
;;

 (* NG: for development purposes this is on by default, probably should be
  *     off for production use.
  *)
let verbose_sys_commands = ref true;;
let sys_command s =
  if !verbose_sys_commands then begin Printf.printf "%s\n" s; flush stdout end;
  (Sys.command s)=0
;;


let verify filename =
  let cmd = "talc.exe"^(ri_options ())^" --elaborate-only "^filename in
  sys_command cmd
;;

exception Exit;;

let do_asm doverify talfn objfile =
  try
    let cmd =
      match !asm_bintool with
      	MS -> (Printf.eprintf "%s: MASM not supproted.\n" talfn; raise Exit)
(* FMS: MASM no longer supported. Commented out.
	  if doverify then
	    if not (verify talfn) then raise Exit;
	  (match !objformat with
	    COFF -> 
	      let aux s p = " /I"^p^s in
	      let includes = aux (List.fold_left aux "" !includes) !runtime in
	      let objfile =
	      	match objfile with None -> "" | Some fn -> " /Fo"^fn in
	      "ml /nologo /coff"^includes^" /Fl /Sc /c"^objfile^" /Ta"^talfn
	  | ELF ->
	      Printf.eprintf "%s: masm cannot output ELF\n" talfn; raise Exit)
*)
      | TALC ->
	  let talc options = "talc.exe -c " ^ talfn ^ " " ^ options in
	  let objfile = match objfile with None -> "" | Some fn -> " -o "^fn in
	  let fmt =
	    match !objformat with COFF -> " --coff" | ELF -> " --elf" in
	  let dasm = (if !do_write_dasm_info then " --dasm-info" else "") in
	  talc ((ri_options ())^dasm^fmt^objfile)
      |	GNU ->
	   Printf.eprintf "%s: GNU assembler not supported\n" talfn;
	   raise Exit    
    in 
    sys_command cmd
  with
    Exit -> false
;;

let asm talfn objfile = do_asm false talfn objfile;;
let verify_and_asm talfn objfile = do_asm true talfn objfile;;


(* find_file_in_paths: Searches for fn in the given paths, in order, 
   and returns the path to fn. 
   Added by RLS to put real file paths into gcc command *)

let rec find_file_in_paths fn paths = 
   if ((String.contains fn '/') ||
       (String.contains fn '\\')) then fn    
   else match paths with
      [] -> fn
    | (path :: tail) -> 
	 if (Sys.file_exists (Filename.concat path fn)) then
	    (Filename.concat path fn)
	 else 
	    (find_file_in_paths fn tail)
;;

(* Used to change all /'s in the given paths to \'s; needed for link
   to work properly *)
let rec convert_paths paths =
  let rec swap_slash s i =
    match (try
      let idx = String.index_from s i '/' in
      s.[idx] <- '\\';
      Some idx
    with Not_found -> None) with
      Some idx -> swap_slash s idx
    | None -> () in
  List.iter (fun p -> swap_slash p 0) paths;
  paths
;;

(* Stuff for the dynamic loader.  The following three functions are
   used to automatically generate and compile a .c file to initialize
   libraries and object files that are linked to create the final 
   executable. *)

(* Generate a string that resembles a C static declaration for
   the provided buffer *)
let make_static_decl =
  let count = ref 0 in
  fun str ofs len ->
    let toString c =
      let cc = Char.code c in
      Printf.sprintf "%d" cc in
    let char_list =
      let l = ref [] in
      for i = ofs to len-1 do
	l := (toString str.[i])::!l;
      done;
      List.rev !l in
    let data = String.concat "," char_list in
    let var = Printf.sprintf "context%d" !count in
    let decl = 
      Printf.sprintf 
	"static char str_%s[%d] = { %s };\nstatic struct string %s = { str_%s, %d };" 
	var len data var var len in
    incr count;
    (var,decl)
;;

let create_loader_init 
    outfilename        (* name of .c file *)
    runtime_filenames  (* names of trusted runtime files *) 
    modules            (* names of modules we'll be linking *)
    interfaces         (* interfaces whose context to export *)
    link_loaderlib =   (* whether we're linking the dynlink library *)

  let outc = open_out outfilename in

  let emit_time_preamble, emit_start_time, emit_end_time, emit_times =
    let noop = ref false in
    let num = ref 0 in
    if !time_init then
      (function n ->
	if n <> 0 then
	  (num := n;
	   Printf.fprintf outc "#include <sys/time.h>\n";
	   Printf.fprintf outc "#include <stdio.h>\n";
	   Printf.fprintf outc "static int delta_sec, delta_usec, i;\n";
	   Printf.fprintf outc "static struct timeval tms, tme;\n";
	   Printf.fprintf outc "static int linksec[%d], linkusec[%d];\n" n n;
	   Printf.fprintf outc "static char *name[%d];\n" n)
	else noop := true),
      (function () -> 
	if not (!noop) then 
	  Printf.fprintf outc "  gettimeofday(&tms,NULL);\n"),
      (fun i nopt ->
	if not (!noop) then
	  (Printf.fprintf outc "  gettimeofday(&tme,NULL);\n";
	   Printf.fprintf outc "  delta_sec = tme.tv_sec - tms.tv_sec;\n";
	   Printf.fprintf outc "  delta_usec = tme.tv_usec - tms.tv_usec;\n";
	   Printf.fprintf outc "  if (delta_usec < 0) { delta_sec--; delta_usec += 1000000; }\n";
	   Printf.fprintf outc "  linksec[%d] += delta_sec;\n  linkusec[%d] += delta_usec;\n" i i;
	   Printf.fprintf outc "  if (linkusec[%d] >= 1000000) { linksec[%d]++; linkusec[%d] -= 1000000; }\n" i i i;
	   match nopt with
	     Some name -> 
	       Printf.fprintf outc "  name[%d] = \"%s\";\n" i name
	   | None -> ())),
      (function _ ->
	if not (!noop) then
	  (Printf.fprintf outc "  for (i=0; i<%d; i++)\n" !num;
	   Printf.fprintf outc "    fprintf (stderr, \"%%s TIME init = %%d.%%06d secs\\n\", name[i], linksec[i], linkusec[i]);\n"))
    else 
      (function _ -> ()), (function () -> ()), 
      (fun _ _ -> ()), (function _ -> ()) in

  emit_time_preamble ((List.length modules) + 
			(match interfaces with Some _ -> 1 | _ -> 0));

  (* extern declarations *)
  Printf.fprintf outc "extern void register_symbol(long,long);\n";
  if link_loaderlib then
    Printf.fprintf outc "extern void caml_startup(char **argv);\n";
  List.iter (function filename -> 
    Printf.fprintf outc 
      "extern void %s_init_loader_syms(void (*f)(long,long));\n" (* *)
      filename) runtime_filenames;
  if modules != [] then
    (Printf.fprintf outc 
       "typedef struct str_internal {int size; char *chars;} *talstring;\n";
     Printf.fprintf outc
      "extern int dlinit(talstring mod_name, void (*initf)(void *lookup, void *add, void *upd, int no_init), int no_init);\n"; (* *)
     let make_pop_string s =
       let len = String.length s in
       Printf.sprintf "static struct str_internal str_%s = { %d, \"%s\" };\n"
	 s len s in
     List.iter (function modname ->
       let string_decl = make_pop_string modname in
       Printf.fprintf outc
	 "%sextern void dyninit_%s(void *lookup, void *add, void *upd, int no_init);\n" string_decl modname) modules);

  (* Binary representation of context *)
  let context_vars =
    (match interfaces with
      Some exps ->
	Printf.fprintf outc "struct string {\n  char *str;\n  int len;\n};\n";
	Printf.fprintf outc "extern void register_context(struct string *contexts[]);\n";
	let (num_ints,varnames) =
	  List.fold_left
	    (fun (num,vars) i ->
	      let (var,decl) = make_static_decl i 0 (String.length i) in
	      Printf.fprintf outc "%s\n" decl;
	      (num+1,var::vars)) (0,[]) exps in
	Printf.fprintf outc "static struct string *contexts[%d];\n" 
	  (num_ints+1);
	varnames
    | None -> []) in

  Printf.fprintf outc "static int retc = 1;\n";
  Printf.fprintf outc "int init_loader(char **argv) {\n";

  emit_start_time ();

  if link_loaderlib then
    Printf.fprintf outc "  caml_startup(argv);\n";
  List.iter (function filename -> 
    Printf.fprintf outc "  %s_init_loader_syms(register_symbol);\n" filename) 
    runtime_filenames;
  (* Init the program type interface *)
  (match interfaces with
    Some _ -> 
      let rec init_contexts vars i =
	(match vars with
	  (var::rest) -> 
	    Printf.fprintf outc "  contexts[%d] = &%s;\n" i var;
	    init_contexts rest (i+1)
	| [] -> 
	    Printf.fprintf outc "  contexts[%d] = 0;\n" i;
	    Printf.fprintf outc "  register_context(contexts);\n") in
      init_contexts context_vars 0
  | None -> ());  

  emit_end_time 0 (Some "context");
  (* Call dlinit twice through without checking the return code;
     this registers all symbols *)
  let x = ref 0 in
  List.iter (function modname -> 
    incr x;
    emit_start_time ();
    Printf.fprintf outc 
      "  dlinit(&str_%s,dyninit_%s,1);\n" modname modname;
    emit_end_time !x (Some modname)) modules;
  x := 0;
  List.iter (function modname -> 
    incr x;
    emit_start_time ();
    Printf.fprintf outc 
      "  dlinit(&str_%s,dyninit_%s,1);\n" modname modname;
    emit_end_time !x None) modules;
  (* Now again with the return code; if failure occurs at this
     point, we have a linking error *)
  (* Printf.fprintf outc "  printf(\"Calling dlinit again\\n\");\n"; *)
  x := 0;
  List.iter (function modname -> 
    incr x;
    emit_start_time ();
    Printf.fprintf outc 
      "  retc &= dlinit(&str_%s,dyninit_%s,0);\n" modname modname;
    emit_end_time !x None) modules;

  emit_times ();

(*  Printf.fprintf outc "  print_table();\n"; *)
  Printf.fprintf outc "  return retc;\n}\n";
  close_out outc

(* return each file in runtime_objfiles and objfiles that is in
   loader_initfiles *)
let get_trusted_objfiles objfiles =
  List.map Filename.basename
    (List.filter
       (fun objfile -> List.mem (Filename.basename objfile) loader_initfiles)
       objfiles)

(* return untrusted object files *)
let get_files_with_init files =
  List.filter
    (fun file -> 
      if not (List.mem file loader_initfiles) then
	let init_name = 
	  let filename = Filename.chop_extension (Filename.basename file) in
	  (match !objformat with
	    ELF -> "dyninit_"
	  | COFF -> "_dyninit_")^filename in
	let cmd = 
	  Printf.sprintf "nm %s | grep %s"
	    file init_name in
	let exitc = Sys.command cmd in
	exitc = 0
      else
	false)
    files

(* replace forward slashes by backslashes to make Win32 happy *)
let fixpath s = 
  let s = String.copy s in
  for i=0 to (String.length s)-1 do
    if (String.get s i) = '/' then String.set s i '\\';
  done; s
;;

let compile_loader_initfile objfiles libfiles interfaces =
(*
  Printf.printf "compile_loader_initfile: \n";
  List.iter (function obj -> Printf.printf "%s\n" obj) objfiles;
  flush stdout;
*)
  (* Create init filename *)
  let loader_initfile = Filename.temp_file "init" ".c" in
  let loader_objfile = 
    (Filename.chop_extension loader_initfile) ^ object_file_suffix in
  (* Get object files that have their own init functions *)
  let modules =
    if !register_trusted_syms then
      List.map 
	(function objfile -> 
	  (Filename.chop_extension (Filename.basename objfile)))
	(get_files_with_init (objfiles@libfiles))
    else [] in
  (* Determine what trusted libraries we are using *)
  let filenames = 
    if !register_trusted_syms then
      List.map (function objfile -> Filename.chop_extension objfile) 
	(get_trusted_objfiles objfiles) 
    else [] (* do not perform initialization *)
  in
  (* Make sure we are linking in the loader library *)
  let link_loaderlib = 
    let loader_obj = ("loader"^object_file_suffix) in
    List.exists (function f -> Filename.basename f = loader_obj) objfiles in

  (* Create the init file *)
  create_loader_init 
    loader_initfile filenames modules interfaces (link_loaderlib);
  (* sys_command ("cat "^loader_initfile); *)
  (* Compile it *)
  let cmd =
    (match !link_bintool with
      GNU -> 
	"gcc -c " ^ loader_initfile ^ " -o " ^ loader_objfile
    | TALC -> 
	failwith "Talout.compile_loader_initfile - unimplemented"
    | MS -> 
	"cl /nologo /c " ^ loader_initfile ^ " /Fo" ^ loader_objfile) in
  sys_command cmd;
  Sys.remove loader_initfile;
  (* Return the name of the object file *)
  loader_objfile

let link objfiles libraries exename interfaces =
  let (cmd,loader_initobjfile) =
    match !link_bintool with
      MS ->
	let paths = 
	  convert_paths ("." :: !runtime :: !includes) in
	let all_objfiles = 
	  List.map (function obj -> find_file_in_paths obj paths)
	    (runtime_objfiles @ objfiles) in
	let all_libfiles =
	  (List.map 
	     (function lib -> find_file_in_paths lib paths) libraries) @ 
	  (List.map 
	     (function lib -> lib^library_suffix) (runtime_libs ())) in	
	let loader_initobjfile = 
	  compile_loader_initfile all_objfiles all_libfiles interfaces in
	let ldbg = if (!link_debug) then "/DEBUG " else "" in
    	("link "^ldbg^"/nologo /nodefaultlib:libc /subsystem:console " ^
    	" /out:" ^ (fixpath exename) ^ " /libpath:" ^ !runtime ^ " " ^
	 loader_initobjfile ^ " " ^
	 (List.fold_right (fun s s' -> (fixpath s)^" "^s') 
	    (all_objfiles @ all_libfiles) ""),
	 loader_initobjfile)

     | TALC -> failwith "Talout.link - TALC linker unimplemented"
     | GNU ->
	 let paths = ("." :: !runtime :: !includes) in
	 let all_objfiles = 
	   List.map (function obj -> find_file_in_paths obj paths)
	     (runtime_objfiles @ objfiles) in
	 let all_libfiles =
	   (List.map 
	      (function lib -> find_file_in_paths lib paths) libraries) @
 	   (List.map 
	      (function lib -> !runtime^"/"^lib^library_suffix) 
	      (runtime_libs ())) in
	 let loader_initobjfile = 
	   compile_loader_initfile all_objfiles all_libfiles interfaces in
	 
	 ("gcc -o " ^ exename ^ " " ^ loader_initobjfile ^ " " ^
	  
	  (List.fold_right (fun s s' -> s^" "^s') 
	     (all_objfiles @ all_libfiles) "") ^
	  
	  "-lm -lcurses",loader_initobjfile)
  in
  let res = sys_command cmd in
  Sys.remove loader_initobjfile;
  res
;;
   
      

(* EOF: talout.ml *)
