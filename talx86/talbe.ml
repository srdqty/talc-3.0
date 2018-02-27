(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* Talbe
 * TAL backend functions.
 *
 *)

open Tal;;
open Talctxt;;

let read_int = Gcdfe.fe Tallex.main Talparser.tal_int;;
let read_pre_mod = Gcdfe.fe Tallex.main Talparser.tal_pre_mod;;

let find_interface s =
  if Sys.file_exists s then
    s
  else
    let rec loop dirs =
      match dirs with
	[] ->
	  let s1 = Filename.concat !Talout.runtime s in
	  if Sys.file_exists s1 then s1 else begin
	    Printf.eprintf "%s: no such interface\n" s; flush stderr;
	    raise Talfail
	  end
      |	dir::dirs ->
	  let s1 = Filename.concat dir s in
	  if Sys.file_exists s1 then s1 else loop dirs in
    loop (List.rev !Talout.includes)
;;

let talintcache = Hashtbl.create 13;;

let rec get_tali intref =
  match intref with
    Int_filename filename ->
      (try 
	Hashtbl.find talintcache filename
      with Not_found ->
	let tali = read_tali filename in
	Hashtbl.add talintcache filename tali;
	tali)
  | Int_data (filename, tali) -> tali
and read_tali filename =
  let fn = find_interface filename in
  try
    let tali = read_int fn in
    if Gcdfec.error_p () then raise Gcdfec.Exit;
    tali
  with
    Gcdfec.Exit -> raise Talfail
;;

let get_mod pre_mod : tal_mod =
   let imps = Array.map get_tali pre_mod.import_refs in
   let exps = Array.map get_tali pre_mod.export_refs in
   { imports = imps;
     exports = exps;
     imp     = pre_mod.pre_imp;
   } 
;;

let multiple_errors = ref false;;
let silent_verify = ref false;;
let print_interfaces = ref false;;
(* controls whether the file is actually type-checked or whether we
 * only process it enough to extract the import/export interface types. *)
let verify_internals = ref true;;
let verify_flag = ref true;;

let print_verify_error ctxt ve =
  let fmt = Format.err_formatter in
  Talpp.print_Talverify fmt Talpp.std_options (ctxt,ve);
  Format.pp_print_newline fmt ()
;;

let errors = ref false;;
let mult_handler c e = errors:=true; print_verify_error c e;;
let sing_handler c e = 
  if !errors then ()  
  else
    begin
      errors:=true;
      print_verify_error c e;
      raise Talfail;
    end

let mult_ctxt = error_handler empty_ctxt mult_handler;;
let sing_ctxt = error_handler empty_ctxt sing_handler;;

let verify talfn tal_pre_mod =
  try
    let tal_mod = get_mod tal_pre_mod in
    let ctxt0 = if !multiple_errors then mult_ctxt else sing_ctxt in
    let imex = (Talverify.get_int_type tal_mod.imports, 
		Talverify.get_int_type tal_mod.exports) in
    errors := false;
    if !verify_flag then
      (try 
	let (ctxt,ctxte) = Talverify.init_ctxt ctxt0 tal_mod in
	if !verify_internals then 
	  (let ctxt = Talverify.verify_imp ctxt imex tal_mod.imp in
	 (* verify exports *)
	  Talverify.verify_exports ctxte ctxt tal_mod.exports);
      with Invalid_argument s-> 
	raise (Failure ("Bug: "^s^" exception raised.")));
    if !errors then raise Talfail;
    if not !silent_verify then
      begin
	if !verify_flag then 
	  (if !verify_internals then
	    begin Printf.printf "%s: TAL verified\n" talfn; flush stdout end
	  else 
	    begin 
	      Printf.printf "%s: interfaces verified\n" talfn; 
	      flush stdout 
	    end)
      end;
    if !print_interfaces then begin
      let (it,et) = imex in
      Printf.printf "%s: import interface:\n" talfn; flush stdout;
      Talpp.print_tal_int_type Format.std_formatter Talpp.std_options it;
      Format.print_newline ();
      Printf.printf "%s: export interface:\n" talfn; flush stdout;
      Talpp.print_tal_int_type Format.std_formatter Talpp.std_options et;
      Format.print_newline ()
    end;
    imex
  with
    Talverify (c,e) -> print_verify_error c e; raise Talfail
  | Failure s ->
      Printf.eprintf "%s: TAL verify failure: %s\n" talfn s; flush stderr;
      raise Talfail
;;

let silent_asm = ref false;;

let asm talfn talmod imex objname =
  let suc =
    match !Talout.asm_bintool with
      Talout.GNU -> 
	Printf.eprintf "GAS support never really done -- we thought about\n";
	Printf.eprintf "it and meant to do it -- honest, but just never\n";
	Printf.eprintf "quite got around to doing it.  Sorry.\n"; false
    | Talout.MS ->
      	(match !Talout.objformat with
	  Talout.COFF -> Talout.asm talfn (Some objname)
      	| Talout.ELF ->
	    Printf.eprintf "%s: masm cannot output ELF\n" talfn; flush stderr;
	    false)
    | Talout.TALC ->
      	try
      	  let objfile = Talasm.assemble talmod.pre_imp imex in
      	  (match !Talout.objformat with
    	    Talout.COFF -> Coff.create_coff talfn objname objfile
      	  | Talout.ELF -> Elf.create_elf talfn objname objfile);
	  true
	with x ->
	  Printf.eprintf "%s: TALC assembler raised %s\n" talfn
	    (Printexc.to_string x);
	  flush stderr;
	  false in
  if not !silent_asm then
    if suc then begin
      Printf.printf "%s: object file created\n" talfn; flush stdout
    end else begin
      Printf.eprintf "%s: object file not created\n" talfn; flush stdout
    end;
  suc
;;

(* EOF: talbe.ml *)
