(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew                                      *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Modified by Dan to remove statistics *)

open Utilities;;
open Talbe;;
open Gcd;;

module Dasm = Dasm.Chan

let linkc = ref false;;
let progc = ref false;;
let gen_to_file = ref true;;
let include_tali_in_to_file = ref true;;
let output_tal_file = ref "";;
let set_output_tal_file s = output_tal_file := s;;
let print_linked_interface = ref false;;

(* 
 * import*export interfaces for all files 
 *)
let imexs = ref [] ;;

(*-------------------------------------------------------------------------*)
(* list of standard libraries that will be linked in *)
let imports = ref [Tal.Int_filename "tal.tali"];;
let add_std_lib s =
  add_object_file (s^Talout.object_file_suffix);
  imports := (Tal.Int_filename (s^".tali")) :: !imports
;;

(*-------------------------------------------------------------------------*)
let add_trusted add_tali s = (* Add a trusted file that will not be checked! *)
  let add_tali = ref add_tali in
  if Filename.check_suffix s Talout.library_suffix then
    add_library s
  else if Filename.check_suffix s Talout.object_file_suffix then
    add_object_file s
  else if Filename.check_suffix s ".tali" then
    (imports := (Tal.Int_filename s) :: !imports;
     add_tali:=false)
  else
    begin 
      Printf.eprintf "File %s has unrecognized extension. Expected extension %s or %s or .tali \n"
	s Talout.object_file_suffix Talout.library_suffix;
      flush stderr;
      raise Gcdfec.Exit;
    end;
  let basename = Filename.chop_extension s in
  if !add_tali then 
    (imports := (Tal.Int_filename (basename^".tali")) :: !imports);
;;
 
(*-------------------------------------------------------------------------*)
let compile_tal_file filename basename modname =
  try
    let talm = read_pre_mod filename in
    let imex = verify basename talm in
    (* generate the .to file with the binary encoding of the type info *)
    if !gen_to_file then
      begin
	let tobjfile = basename ^ ".to" in
	let toch = 
	  try open_out_bin tobjfile 
	  with Sys_error s ->
	    (Printf.eprintf "failed to open %s: %s\n" tobjfile s;
	     flush stderr; raise Gcdfec.Exit) in
	let close_int_ref intref =
	  match intref with
	    Tal.Int_filename s ->
	      let tali = Talbe.get_tali intref in
	      Tal.Int_data (s,tali)
	  | Tal.Int_data _ -> intref in
	let talm = 
	  if !include_tali_in_to_file then
	    { talm with Tal.import_refs = 
	        Array.map (function ir -> close_int_ref ir) talm.Tal.import_refs;
	      Tal.export_refs = 
	        Array.map (function ir -> close_int_ref ir) talm.Tal.export_refs } 
	  else talm in
	Talbinout.Chan.emit_tal_pre_mod toch talm;
	close_out toch;
      end;
    if assemble_p () then begin
      let objfile = basename ^ Talout.object_file_suffix in 
      if not (asm filename talm imex objfile) then raise Gcdfec.Exit; 
      add_object_file objfile
    end;
    imexs := (modname,imex) :: !imexs;
    (* Gc.print_stat stdout; *)
    true
  with Gcdfec.Exit | Talctxt.Talfail -> false
;;

(*-------------------------------------------------------------------------*)
let verify_to_file to_filename basename modname = 
  try
    let obj_filename = basename ^ Talout.object_file_suffix in
    let talm = Dasm.disassemble (!Talbe.verify_internals) 
	to_filename obj_filename in
    let imex = verify basename talm in
    if !output_tal_file <> "" then 
      Talout.write_pre_mod basename (!output_tal_file) talm;
    add_object_file obj_filename;
    imexs := (modname,imex) :: !imexs;
    (* Gc.print_stat stdout;*)
    true
  with Gcdfec.Exit 
  | Talctxt.Talfail -> false
;;

(*-------------------------------------------------------------------------*)

(* The following groups of routines are used to calculate the program
   interface \Theta to be maintained for dynamic linking.  This interface
   is enforced and maintained in the code for tal_load. *)

(* Generate representations of the contexts of each linked file *)
let make_context it =
   let buf = Buffer.create 100 in 
   Talbinout.Buf.emit_out_tal_int_type buf it;
   Buffer.contents buf

let make_context_data i = 
  let print_ifc imports exports =
    Printf.printf "program type interface will be:\n";
    Printf.printf "Imports\n";
    Talpp.print_tal_int_type Format.std_formatter 
      { Talpp.std_options with Talpp.cons = false } imports;
    Format.pp_print_newline Format.std_formatter ();
    Printf.printf "\nExports\n";
    Talpp.print_tal_int_type Format.std_formatter 
      { Talpp.std_options with Talpp.cons = false } exports;
    Format.pp_print_newline Format.std_formatter ();
    Printf.printf "\n";
    Pervasives.flush stdout in
  match i with (Some (imports, exports)) ->
    ((* print_ifc imports exports;
     Printf.printf "--writing imports\n"; *)
     let i_ctxt_data = make_context imports in
     (* Printf.printf "--writing exports\n"; *)
     let e_ctxt_data = make_context exports in
     let ctxt_data = [ i_ctxt_data ; e_ctxt_data ] in
     (* Printf.printf "--reading imports\n";
     let strchan = Stringchan.from_string (List.hd ctxt_data) in 
     let imports' = Talbinin.Str.read_in_tal_int_type strchan in
     Printf.printf "--reading exports\n";
     let strchan = Stringchan.from_string (List.hd (List.tl ctxt_data)) in
     let exports' = Talbinin.Str.read_in_tal_int_type strchan in
     print_ifc imports' exports'; *)
     ctxt_data)
  | None -> 
      failwith "--register-trusted-symbols :  must also use --verify-program"

(* List of tali files to be included in dynamic program interface.
   add_registered_file is called by user-directive with option
   --register-syms s *)
let dyn_exports = ref []
let add_registered_file s =
  dyn_exports := s :: !dyn_exports

(* List of symbols to be included from standard .tali interfaces *)
let export_syms =
  [ (* tal.tali *) 
    "_tal_exit"; 
    (* pop_runtime.tali *)
    "_pop_exn_handler"; 
    "_pop_exn_handler_verbose"; 
    "___zzzz_global_cstk";
    "___zzzz_active_cstk";
    "_division_by_zero_error";
    "_GC_free";
    "__current_file";
    "__current_line" ]

(* Generate the dynamic program interface, given the static linking
   interface.  Uses the export_syms list and the dyn_exports list
   to modify imex_final. *)
let dyn_imex imex_final =
  (* Dynamic interface is constructed as follows:
     - all constructors imported or exported by the program
     - the values exported by the program from:
       - the registered interfaces
       - the non-registered interfaces that are in the export_syms list
  *)
  let vals_from_imex imex =
    List.filter
      (function (x,_) -> 
	List.mem (Identifier.id_to_string x) export_syms)
      (snd imex).Tal.it_vals in
  let dyn_export_vals =
    List.flatten 
      (let registered_exports =
	(List.map 
	   (fun (name,imex) -> 
	     if List.mem name !dyn_exports then 
	       (snd imex).Tal.it_vals
	     else []) !imexs) in
      let special_exports = vals_from_imex imex_final in
      special_exports :: registered_exports) in
  { Tal.it_cons = (fst imex_final).Tal.it_cons;
    Tal.it_vals = []},
  { Tal.it_cons = (snd imex_final).Tal.it_cons; 
    Tal.it_vals = dyn_export_vals }

(*-------------------------------------------------------------------------*)
let do_link objectfiles libraries executable =
  try
    let runtime_imex = ref None in 
    if !linkc then begin
      try
      	(let imex = 
	  Tallinkchk.verify_link (List.rev (snd(List.split !imexs))) in
      	Printf.printf "LINK Verified\n"; flush stdout;
      	if !print_linked_interface then begin
	  let (it,et) = imex in
	  print_string "TAL Linked Interface"; print_newline ();
	  print_string "Imports"; print_newline ();
	  Talpp.print_tal_int_type Format.std_formatter Talpp.std_options it;
	  Format.print_newline ();
	  print_string "Exports"; print_newline ();
	  Talpp.print_tal_int_type Format.std_formatter Talpp.std_options et;
	  Format.print_newline ()
      	end;
      	if !progc then begin
 	  try
	    (* get interface from the standard lib *)
            let imex_stdlib = Talverify.get_program_interface
		get_tali [||] (Array.of_list (List.rev !imports)) in
            (* make sure program is link compatible with stdlib *)
	    let imex_final = Tallinkchk.verify_link [imex; imex_stdlib] in 
	    (* get interface that complete programs should have *)
	    let imex_program = Talverify.get_program_interface get_tali 
		[||] [|(Tal.Int_filename "tal_prog.tali")|] in
            (* make sure program conforms to that interface *) 
	    let _ = Tallinkchk.verify_program imex_final imex_program in 

	    (* create runtime intferface *)
	    runtime_imex := Some (dyn_imex imex_final);
	    Printf.printf "PROG Verified\n"
	  with
	    Talctxt.Talverify (ctxt,ve) ->
	      print_newline();
	      print_string "PROG verification failed";
	      print_newline ();
	      Talpp.print_Talverify
	      	Format.std_formatter Talpp.std_options (ctxt,ve);
	      Format.print_newline ();
	      raise Talctxt.Talfail
	  | Invalid_argument s -> 
	      print_string("Hit\n"); 
	      raise Talctxt.Talfail
	  | x -> print_string("Fail\n"); raise Talctxt.Talfail
      	end)
      with 
      	Talctxt.Talverify (ctxt,ve) ->
	  print_newline();
	  print_string "LINK verification failed";
	  print_newline ();
	  Talpp.print_Talverify
	    Format.std_formatter Talpp.std_options  (ctxt,ve);
	  Format.print_newline ();
	  raise Talctxt.Talfail
    end;
    (* FMS: Removed.  Why was this here? 
       if libraries<>[] then failwith "Talc.do_link"; *)
    Talout.link objectfiles libraries executable 
      	(if !Talout.register_trusted_syms then 
	   Some (make_context_data !runtime_imex)
      	else None)

  with
    Talctxt.Talfail -> false
;;

let toolname = "talc";;
set_tooldesc "TALC: TAL verifier, assembler, link & program verifier";;

let options =
  std_options @
  [ "-T",Arg.String (add_trusted true),
    "like --trusted but automatically adds interface.";
    "--multiple-errors",Arg.Set multiple_errors,
      "show multiple verifier errors";
    "--single-error",Arg.Clear multiple_errors,
      "show first verify error only";
    "--full-types", Arg.Set Talpp.full_types,
      "Print full types in error messages";
    "--noisy-verify",Arg.Clear silent_verify,
      "indicate verification success";
    "--quiet-verify",Arg.Set silent_verify,
      "silent on successful verification";
    "--print-interfaces",Arg.Set print_interfaces,
      "print module interfaces";
    "--no-interfaces",Arg.Clear print_interfaces,
      "do not print module interfaces";
    "--verify-link",Arg.Set linkc,"verify linking";
    "--print-linked-interface",Arg.Set print_linked_interface,
      "print linked interface";
    "--no-linked-interface",Arg.Clear print_linked_interface,
      "do not print linked interface";
    "--verify-program",Arg.Set progc,"verify program";
    "--std-lib",Arg.String add_std_lib, "add library (an object file)";
    "--trusted",Arg.String (add_trusted false), 
    "add unverified object file, library or interface";
    "--no-gen-to",Arg.Clear gen_to_file,"do not generate a .to file";
    "--generate-tal",Arg.String set_output_tal_file,
      "generate .tal file <filename>, even when given .to";
    "--no-internals",Arg.Clear Talbe.verify_internals,
      "use during link-checking to bypass whole-file verification";
    "--register-trusted-syms",Arg.Set Talout.register_trusted_syms,
      "register trusted symbols with the dynamic loader";
    "--register-syms", Arg.String add_registered_file,
     "add file's symbols to program image implicitly available to loaded code";
    "--link-debug", Arg.Set Talout.link_debug,
      "pass the DEBUG flag to the linker";
    "--no-verify", Arg.Clear Talbe.verify_flag,
    "do not verify anything"
  ]
;;

let file_types = [ (".to",verify_to_file);
		   (".tal",compile_tal_file) ] @ std_file_types;;

let middle () = true;;
let main () = driver toolname options file_types middle do_link;;

Printexc.catch main ();;

(* EOF: talc.ml *)
