(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Chris Hawblitzel, Dan Grossman,     *)
(*     Frederick Smith                                                *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Soon we should probably stop using cl and friends entirely *)

module PS = Popsyntax

open Talout;;
open Gcd;;

type code_gen = CgStack | CgReg

let code_gen            = ref CgStack
let preprocessonly      = ref false
let defines             = ref []

let convert_to_loadable = ref false
let export_to_loadable  = ref false
let export_locals       = ref false
let notice_updates      = ref false
let pretty_print        = ref false

(* Cyclone *)
(* TEMPORARY; CYCLONE/MASM *)
let unsafemasm 		= ref false
(* End Cyclone *)
let verify_tal 		= ref true

(*-------------------------------------------------------------------------*)
let stage s decls f =
  (try f () with 
  | Gcdfec.Exit -> raise Gcdfec.Exit (* Normal-user failure. *)
  | e -> begin
      Printf.eprintf "Compiler Error: %s failed.\n" s; 
      flush stderr;
      if !pretty_print then
	PS.pr_popprogram Format.std_formatter PS.std_opts (List.rev decls);
      raise e
  end)

(*-------------------------------------------------------------------------*)
(* for option --loadable *)
let set_loadable () =
  convert_to_loadable := true;
  export_to_loadable := true

(*-------------------------------------------------------------------------*)
(* for option -D *)
let add_define p = 
  if not (List.mem p (!defines))
  then defines := p :: !defines
  else ()

(*-------------------------------------------------------------------------*)
let execute_command cmd =
  let exitc = Sys.command cmd in
  if not (exitc = 0) then
    failwith 
      (Printf.sprintf "|%s| failed with exit code %d\n"
	 cmd exitc)
      ;;

(*-------------------------------------------------------------------------*)
let abort_on_error () = 
  if Gcdfec.error_p () then raise Gcdfec.Exit

(*-------------------------------------------------------------------------*)
(* Adds the directory of the popcorn library to the list of includes *)
let include_poplibdir () =
  let poplibdir = 
    try Sys.getenv "POPCORNLIB"
    with Not_found -> 
      try Filename.concat 
	  (Sys.getenv "TALCLIB")
	  (Filename.concat ".." (Filename.concat "popcorn" "lib"))
      with Not_found -> Filename.current_dir_name in
  add_include poplibdir

(*-------------------------------------------------------------------------*)
(* preprocesses the given file, returns the name of the resulting file *)
let preprocess_file filename modname =
  include_poplibdir ();
  let preprocfile  = modname ^ ".i" in 
  Sys.command
    (if Sys.os_type = "Unix" 
    then 
      let inc_string = 
	List.fold_left (fun r d -> "-I " ^d ^ " " ^ r) "" !includes in
      let def_string =
	List.fold_left (fun r d -> "-D " ^d ^ " " ^ r) "" !defines in
      Printf.sprintf "gcc -x c -E %s %s %s > %s" 
	inc_string def_string filename preprocfile
    else 
      let inc_string = 
	List.fold_left (fun r d -> "/I" ^d ^ " " ^ r) "" !includes in
      let def_string = 
	List.fold_left (fun r d -> "/D" ^d ^ " " ^ r) "" !defines in
      Printf.sprintf "cl /nologo %s %s /P /TC %s" 
	inc_string def_string filename);
   preprocfile

(*-------------------------------------------------------------------------*)
(* Given a file (and its alternate names), performs pre-processing,
   and returns back the AST *)
let parse_pop_file filename basename modname =
  let preprocfile = preprocess_file filename modname in
  let rm_ppop = 
    let ppop_removed = ref false in
    (function () ->
      if not !ppop_removed then Sys.remove preprocfile; 
      ppop_removed := true) in
  try
    let decls = Gcdfe.fe Poplex.token Popparse.top preprocfile in
    abort_on_error ();
    (* rm_ppop (); *)
    (decls, rm_ppop)
  with e -> 
    rm_ppop (); raise e

(*-------------------------------------------------------------------------*)
(* Given the parsed popcorn AST, compiles the result to TAL code *)
let compile_pop_ast ifc_only decls add_varmap upd_varmap lookup_varmap
    modname basename filename =

  (* filenames *)
  let talfile    = basename ^ ".tal"    in
  let objectfile = basename ^ object_file_suffix in
  let impfile    = modname  ^ "_i.tali" in 
  let expfile    = modname  ^ "_e.tali" in 
  let impfileabs = basename ^ "_i.tali" in 
  let expfileabs = basename ^ "_e.tali" in

  (* do conversion for dynamic loader, if necessary *)
  let decls = 
    if !convert_to_loadable
    || !export_to_loadable 
    || !notice_updates then 
      (let flags = 
	{ Popdyntrans.do_import = !convert_to_loadable || !notice_updates;
	  Popdyntrans.do_export = !export_to_loadable || !convert_to_loadable;
	  Popdyntrans.do_notice_updates = !notice_updates;
	  Popdyntrans.do_static_exports =  not !convert_to_loadable; 
	  Popdyntrans.do_export_local_vars = !export_locals;
	  Popdyntrans.add_varmap = add_varmap;
	  Popdyntrans.upd_varmap = upd_varmap;
	  Popdyntrans.lookup_varmap = lookup_varmap; } in
      stage "Dynamic translation." decls 
	(fun () -> 
	  if ifc_only then
	    (let oldv = !Popdyntrans.count_references in
	    Popdyntrans.count_references := false;
	    let res =
	      Popdyntrans.dyntrans ifc_only decls flags filename modname in
	    Popdyntrans.count_references := oldv;
	    res)
	  else
	    Popdyntrans.dyntrans ifc_only decls flags filename modname))
    else decls in
  abort_on_error ();

  (* typecheck *)
  let popmod = 
    try
      stage "Type-checking" decls 
	(fun () -> Poptype.type_check ifc_only decls) 
    with e -> 
      if !pretty_print then
	(PS.pr_popprogram 
	   Format.std_formatter PS.std_opts (List.rev decls);
	 raise e)
      else
	raise e
  in

  (* pretty-print, if necessary *)
  if !pretty_print then
    (if Gcdfec.error_p () then
      (PS.pr_popprogram 
	 Format.std_formatter PS.std_opts (List.rev decls);
       raise Gcdfec.Exit)
    else  
      (PS.pr_popprogram 
	 Format.std_formatter PS.std_opts (List.rev (fst popmod));
       true))
  else
    (abort_on_error ();

     (* compile to TAL *)
     let (implementation,imports,exports) =
       match !code_gen with
	 CgStack ->
	   stage "Code generation" (fst popmod) 
	     (fun () -> 
	       Popcompile.code_gen 
		 modname 
		 (Tal.Int_filename impfile) 
		 (Tal.Int_filename expfile) 
	         ifc_only 
		 popmod)
       | CgReg ->
	   Printf.eprintf "%s: register allocator currently broken :-(\n"
	     basename; flush stderr; raise Gcdfec.Exit
           (* Popcompilereg.code_gen modname impfile expfile popmod *)
     in
     if ifc_only then
       Talout.write_int modname (basename^".tali") imports
     else
       (Talout.write_int (modname^"_i") impfileabs imports;
	Talout.write_int (modname^"_e") expfileabs exports;
	Talout.write_pre_mod modname talfile implementation);

     if ifc_only then true
     else if !verify_tal & assemble_p() then
       (stage "Assemble and verify\m" []
	  (fun () -> 
	    (Talout.verify_and_asm talfile (Some objectfile)) &
	    (add_object_file objectfile; true)))
     else 
       if not !verify_tal then true 
       else Talout.verify talfile)

(*-------------------------------------------------------------------------*)
(* Entry point -- given the name of the popcorn file to compile,
   compiles it to TAL code *)
let compile_pop_file ifc_only filename basename modname =
  Printf.printf "Compiling file: %s\n" basename; flush stdout;
  try 
    (if !preprocessonly then
      (preprocess_file filename modname;
       true)
    else
      (let (decls,rm_ppop) = parse_pop_file filename basename modname in
      let res = 
	(try
	  compile_pop_ast ifc_only decls [] [] [] modname basename filename
	with e ->
	  rm_ppop (); raise e) in
      rm_ppop ();
      res))
  with Gcdfec.Exit ->
    false

(*-------------------------------------------------------------------------*)
(* For compiling dynamic updates *)
let compile_patch_file filename basename modname =
  Printf.printf "Compiling patch file: %s\n" basename; flush stdout;
  try 
    (let (impl_fname_opt, ifc_fname_opt, svars, rvars) = 
      Popdynpatch.get_patch_file filename in

    let mname, bname, fname = ref "", ref "", ref "" in

    (* interface code file *)
    let (ifc_decls,rm_ppop) =
      match ifc_fname_opt with
	Some ifc_filename ->
	  let ifc_basename = Filename.chop_extension filename in
	  let ifc_modname = Filename.basename ifc_basename in
	  fname := ifc_filename;
	  bname := ifc_basename;
	  mname := ifc_modname;
	  parse_pop_file ifc_filename ifc_basename ifc_modname
      |	None -> ([],function () -> ()) in

    (* new implementation file *)
    let (impl_decls,rm_ppop2) =
      match impl_fname_opt with
	Some impl_filename ->
	  let impl_basename = Filename.chop_extension filename in
	  let impl_modname = Filename.basename impl_basename in
	  fname := impl_filename;
	  bname := impl_basename;
	  mname := impl_modname;
	  parse_pop_file impl_filename impl_basename impl_modname
      |	None -> ([],function () -> ()) in
    
    (* make sure we have at one of the two code files *)
    if !mname = "" then
      failwith "must specify at least one implementation or interface file";

    (* generate the patch code *)
    let (add_varmap,upd_varmap,lookup_varmap,decls) = 
      Popdynpatch.patchtrans impl_decls ifc_decls svars rvars in

(*
    if !pretty_print then
      PS.pr_popprogram Format.std_formatter PS.std_opts (List.rev decls);
*)

    (* compile the patch code *)
    let cleanup () =
      try
	rm_ppop (); rm_ppop2 ()
      with _ ->
	() in
    let res = 
      (try
	compile_pop_ast false decls add_varmap upd_varmap lookup_varmap 
	  !mname !bname !fname
      with e ->
	cleanup (); raise e) in
    cleanup ();
    res)
  with Gcdfec.Exit ->
    false

(*-------------------------------------------------------------------------*)
let middle () =
  add_object_file ("stdlib"^object_file_suffix);
  add_object_file ("pop_runtime"^object_file_suffix);
  add_object_file ("prelude"^object_file_suffix);
(* Cyclone *)
  add_object_file ("cyclonelib"^object_file_suffix);
(* End Cyclone *)
  true
;;

(*-------------------------------------------------------------------------*)
let do_link objfiles libraries executable =
  Talout.link objfiles libraries executable None
;;

let toolname = "popcorn";;
set_tooldesc "popcorn: A safe C subset compiler";;

let options =
  std_options @ 
  ["-xxx",Arg.Clear verify_tal, "generate TAL, do not verify";
    "-E", Arg.Set preprocessonly, "only run pre-processor, creating .i file";
    "-D", Arg.String add_define, "add preprocessor #define";
    "--stack-codegen", Arg.Unit (fun () -> code_gen := CgStack), 
    "use stack-based code generator";
    "--loadable", Arg.Set convert_to_loadable, 
    "compile file to be dynamically loadable";
    "--no-loadable-fn-ptrs", Arg.Clear Popdyntrans.translate_fn_ptrs,
    "compile file to not allow function pointers to be updateable";
    "--loadable-fn-ptrs", Arg.Set Popdyntrans.translate_fn_ptrs,
    "compile file to allow function pointers to be updateable";
    "--count-references", Arg.Set Popdyntrans.count_references,
    "dynamically count the # of references to static and dynamic data";
    "--notice-updates", Arg.Set notice_updates, 
    "compile file to notice dynamic updates";
    "--export-syms", Arg.Set export_to_loadable, 
    "export symbols to dynamically loadable files";
    "--export-locals", Arg.Set export_locals,
    "export local symbols to dynamically loadeded files as well";
    "--pretty-print", Arg.Set pretty_print,
    "pretty print the source file after preprocessing and typechecking";
    "--stack-trace", Arg.Set Popsyntax.stack_traces,
    "generate stack traces for uncaught exceptions."
  ]
;;

let file_types = [".pop",(compile_pop_file false);
		   ".h",(compile_pop_file true);
		  ".patch", compile_patch_file] @ std_file_types;;

let main () = driver toolname options file_types middle do_link;;

Printexc.catch main () ;;

(* EOF: popcorn.ml *)
