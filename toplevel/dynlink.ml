(****************************************************************************
  Caml entry point for dynamic linking. 
  Given the binary representation of a type, the contents of a
  tofile, and its associated object file, parse the type, disassemble
  the object file, typecheck it, and compare the product of the types
  of the labels in the object file with the given type.
  
  If they match return a list of all the symbols defined in the file.
  Otherwise, return None.
 *****************************************************************************)

(*** Flags ***)
let do_verify = true
let do_verify_internals = true
let do_timeit = false

(* for timing *)
external gettimeofday : unit -> float = "caml_gettimeofday"

(* Changes:
   RLS 5/26/00: Added register_libs, split register_context into
   register_con_context and register_context, and split verify_tal_file
   into that and verify_tal_file_con. 

   Also added a lot of debug print statements, which are now commented out.*)


      (* disassemble type representation *)      

let deSome x = match x with Some y -> y 
 | None -> failwith "Internal Error - dynlink.ml"
	 
(* 
 * The current import/export interface of the running code
 *)
let runtime_imex  = ref None


(* 
 * Called by the initialization function of the statically linked file 
 * to create the first imex.
 *)

let register_con_context runtime_imports runtime_exports =
   (* merge with existing context *)
   (*   verify that it's consistent with the current context *)
   let imex =
      (match !runtime_imex with
	 Some (curr_imports, curr_exports) ->
	    Tallinkchk.verify_link [ (runtime_imports, runtime_exports);
				      (curr_imports, curr_exports) ]
       | None -> (runtime_imports, runtime_exports)) in
   runtime_imex := Some imex

(*
     let printCtx x = 
       match x with 
	 None -> failwith "internal_error"
       | Some (it,et) ->
	   Printf.printf "Register context:\n";
	   print_string "Imports"; print_newline ();
	   Talpp.print_tal_int_type Format.std_formatter 
	     { Talpp.std_options with Talpp.cons = false } it;
	   Format.print_newline ();
	   print_string "Exports"; print_newline ();
	   Talpp.print_tal_int_type Format.std_formatter 
	     { Talpp.std_options with Talpp.cons = false } et;
	   print_newline ()
     in
     printCtx !runtime_imex;
*)


let register_context (cons_arr : string array) =
   try
     (* disassemble psi representation for import cons *)
      let psirep = cons_arr.(1) in
      let strchan = Stringchan.from_string psirep in 
      let runtime_imports = Talbinin.Str.read_in_tal_int_type strchan in
      
     (* disassemble psi representation for export cons *)
      let psirep = cons_arr.(0) in
      let strchan = Stringchan.from_string psirep in 
      let runtime_exports = Talbinin.Str.read_in_tal_int_type strchan in

      register_con_context runtime_imports runtime_exports;
      0
   with
      e -> (Printf.printf "Caught exception %s in register_context\n"
		 (Printexc.to_string e);
     	      Pervasives.flush stdout;
     	      1 (* error *))
	          


(* register_libs: Allows the caller to specify the names of libraries
   to register symbols from. (RLS 5/26/00)

   'libfiles' should include at least tal.tali, stdlib.tali, and 
   prelude_e.tali. 'libdirs' should contain at least the value of the
   "TALCLIB" environment variable. *)

let register_libs (libdirs : string array) (libfiles : string array) =
   try 
(*
      printf "Dirs: ";
      Array.iter (printf "%s; ") libdirs;

      printf "\nFiles: ";
      Array.iter (printf "%s; ") libfiles;
      printf "\n";
*)

   (* Add library directories. *)
      Array.iter Talout.add_include libdirs;

      Printf.printf "Runtime dirs: ";
      List.iter (Printf.printf "%s; ") !Talout.includes;
      Printf.printf "\n";
      flush stdout;

(*
      Printf.printf "Including files:\n";
      Array.iter 
	 (function x ->
	    Printf.printf "  %s\n" (Talbe.find_interface x))
	 libfiles;
      Printf.printf "\n";
      flush stdout;
*)

   (* Read the libraries' .tali files. *)
      let (libimps, libexps) = 
	Talverify.get_program_interface Talbe.get_tali [||] 
	  (Array.map (function s -> Tal.Int_filename s) libfiles) in

      Printf.printf "Got runtime imexs. Imports:\n";
      Talpp.print_tal_int_type Format.std_formatter Talpp.std_options
	 libimps;
      flush stdout;
      Printf.printf "\nExports:\n";
      Talpp.print_tal_int_type Format.std_formatter Talpp.std_options
	 libexps;
      Printf.printf "\n\n";
      flush stdout;

      
      register_con_context libimps libexps;
      
(*
      Printf.printf "Context registered.\n";
      flush stdout;
*)

   with e ->
      (Printf.printf "register_libs failed\n";
	 (match e with
	    Talctxt.Talverify(ctxt, error) ->
(*	  Talpp.print_verify_error Format.std_formatter 
  Talpp.std_options error; *)
	       Talpp.print_Talverify Format.std_formatter Talpp.std_options
		  (ctxt, error);
(**)
	       Format.pp_print_newline (Format.std_formatter) ();
	       Format.pp_print_flush Format.std_formatter ();
	       Pervasives.flush stdout;
	  | e -> 
	       Printf.printf "Uncaught exception %s\n" (Printexc.to_string e));
      	 Pervasives.flush stdout;)

	 
;;


(* Verifies the .to/.obj file given in tofile/code, and makes sure that it
   implements the interface given in con. (RLS 5/26/00) *)

let verify_tal_file_con con tofile code : string list option =
(*
      Printf.printf "Passed in type:\n"; 
      Talpp.print_con (Format.std_formatter) Talpp.std_options con; 
      Printf.printf "\n\n";

    Printf.printf "About to look at runtime!\n";
    let (it,et) = deSome !runtime_imex in  
    Printf.printf "Current context:\n"; 
    Printf.printf "Imports\n";
    Talpp.print_tal_int_type Format.std_formatter 
       { Talpp.std_options with Talpp.cons = false } it;
    Printf.printf "\nExports\n";
    Talpp.print_tal_int_type Format.std_formatter 
       { Talpp.std_options with Talpp.cons = false } et;
    Printf.printf "\n\n";
*)
    (* disassemble code *)
    let start = if (do_timeit) then gettimeofday () else 0.0 in
    let talm = Dasm.Str.disassemble true tofile code in  
    if (do_timeit) then 
      (let finish = gettimeofday () in
      let delta = finish -. start in
      Printf.eprintf "TIME Disasm = %f secs\n" delta);

    (* verify TAL file in isolation *)
    (* Modified to use Talbe.verify instead.  Does more error-handling,
       but not much else. *)
    let (import, export) =
      let verify_flag = !Talbe.verify_flag in
      let verify_internals = !Talbe.verify_internals in
      let silent_verify = !Talbe.silent_verify in
      Talbe.verify_flag := do_verify;
      Talbe.verify_internals := do_verify_internals;
      Talbe.silent_verify := true;
      let start = if (do_timeit) then gettimeofday () else 0.0 in
      let imex = Talbe.verify tofile talm in
      if (do_timeit) then 
	(let finish = gettimeofday () in
	let delta = finish -. start in
	Printf.eprintf "TIME cchk = %f secs (ver=%b, ver_int=%b)\n" 
	  delta do_verify do_verify_internals);
      Talbe.verify_flag := verify_flag;
      Talbe.verify_internals := verify_internals;
      Talbe.silent_verify := silent_verify;
      imex in
(*
      Talverify.verify_imp true Talbe.get_tali Talctxt.empty_ctxt talm in 
*)

(*
    Printf.printf "Loaded file context:\n";
    print_string "Imports"; print_newline ();
    Talpp.print_tal_int_type Format.std_formatter 
       { Talpp.std_options with Talpp.cons = false } import;
    Format.print_newline ();
    print_string "Exports"; print_newline ();
    Talpp.print_tal_int_type Format.std_formatter 
       { Talpp.std_options with Talpp.cons = false } export;
    Format.print_newline ();
*)
    (* Don't export any values *)
    let start = if (do_timeit) then gettimeofday () else 0.0 in

    let export_cons = {Tal.it_cons = export.Tal.it_cons; Tal.it_vals = []} in 
    
    (* verify that it is link-compatible with the runtime interface and 
       compute the new interface  *)
    let (it, et) as imex = 
      Tallinkchk.verify_link [ (import, export_cons);
			       deSome (!runtime_imex) ] in 

    (* verify that there are no unresolved val imports *)
    let _ = Tallinkchk.verify_program imex 
	({Tal.it_cons =[]; Tal.it_vals=[]}, 
	 {Tal.it_cons=[]; Tal.it_vals=[]}) in 

    if (do_timeit) then 
      (let finish = gettimeofday () in
      let delta = finish -. start in
      Printf.eprintf "TIME lchk = %f secs\n" delta);
  
    let start = if (do_timeit) then gettimeofday () else 0.0 in

    (* verify that the exported symbols have the expected types *)
    let prod = 
      Tal.chptr 
	[] 
	(Some (Tal.cprod (List.map (fun (a,b) -> 
	  Tal.cfield b Tal.ReadWrite) export.Tal.it_vals)))
        None 
    in

(*
    Printf.printf "File contains type:\n";
    Talpp.print_con (Format.std_formatter) Talpp.std_options prod;
    Format.pp_print_newline (Format.std_formatter) ();

    let buf = Buffer.create 16 in
    Talbinout.Buf.emit_out_con buf prod;
    let str = Buffer.contents buf in
    Printf.printf "length = %d\n" (String.length str);
    print_endline (dumpbuf str 0 (String.length str));
    Pervasives.flush stdout;
*)
    (* confirm that the exported symbols match the expected types *)
    Talcon.eqcon (Talctxt.empty_ctxt) con prod;

    if (do_timeit) then 
      (let finish = gettimeofday () in
      let delta = finish -. start in
      Printf.eprintf "TIME ifchk = %f secs\n" delta;
      Pervasives.flush stderr);

    (* ------- All systems go, we can proceed to link ---------- *)
    (* save the new runtime import/export interface *)
    runtime_imex := Some imex;

    (* return the list of labels associated with the passed in
       type representation.  *)
    Some (List.map (fun (a,b) -> 
      Identifier.id_to_source a) export.Tal.it_vals)
;;



let eqconrep rep1 rep2 = 
  try
(*
    Printf.printf "eqconrep: len(rep1) = %d, len(rep2) = %d\n"
      (String.length rep1) (String.length rep2);
*)
    if rep1 = rep2 then true
    else
     ((* disassemble type representation *)
      let strchan = Stringchan.from_string rep1 in 
      let con1 = Talbinin.Str.read_in_con strchan in
    (* disassemble type representation *)
      let strchan = Stringchan.from_string rep2 in 
      let con2 = Talbinin.Str.read_in_con strchan in
      try 
	(Talcon.eqcon (Talctxt.empty_ctxt) con1 con2;
	 true)
      with _ -> 
(*
	Printf.printf "eqcon rep1 = :\n";
	Talpp.print_con (Format.std_formatter) Talpp.std_options con1;
	Format.pp_print_newline (Format.std_formatter) ();
	Printf.printf "eqcon rep2 = :\n";
	Talpp.print_con (Format.std_formatter) Talpp.std_options con2;
	Format.pp_print_newline (Format.std_formatter) ();    
*)
	false)
  with e -> 
    (Printf.printf "eqconrep: Failed to unmarshal type\n%s\n" 
       (Printexc.to_string e);
     Pervasives.flush stdout;
     false)



(* Takes a string denoting typrep, and disassembles it to get the type to
   check against. *)

let verify_tal_file typrep tofile code : string list option =
   try
      let strchan = Stringchan.from_string typrep in 
      let con = Talbinin.Str.read_in_con strchan in

      verify_tal_file_con con tofile code
   with e ->
     (Printf.printf "Verification failed\n";
      (match e with
	Talctxt.Talverify(ctxt, error) ->
(*	  Talpp.print_verify_error Format.std_formatter 
	      Talpp.std_options error; *)
	   Talpp.print_Talverify Format.std_formatter Talpp.std_options
	      (ctxt, error);
(**)
	   Format.pp_print_newline (Format.std_formatter) ();
	   Format.pp_print_flush Format.std_formatter ();
	   Pervasives.flush stdout;
       | e -> 
	    Printf.printf "Uncaught exception %s\n" (Printexc.to_string e));
      	Pervasives.flush stdout;
      	None)


(* Prints out the contents of a type representation; used for debugging *)
let print_conrep rep =
  try
    let strchan = Stringchan.from_string rep in 
    let con = Talbinin.Str.read_in_con strchan in
    Talpp.print_con (Format.std_formatter) Talpp.std_options con;
    Format.pp_print_newline (Format.std_formatter) ()
  with e ->
    (Printf.printf "print_conrep: Failed to unmarshal type\n%s\n" 
       (Printexc.to_string e);
     Pervasives.flush stdout)


(* Allow this code to be called from C *)
let _ = 
   Callback.register "print_conrep" print_conrep;
   Callback.register "verify_tal_file" verify_tal_file;
   Callback.register "eqconrep" eqconrep;
   Callback.register "register_context" register_context;
   Callback.register "register_libs" register_libs
