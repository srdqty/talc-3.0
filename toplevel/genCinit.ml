(* Given an interface file, this program generates C code for the init 
   function registering the value labels present in that interface file.
   Code is emitted to standard out. *)

(*----------------------------------------------------------------------*)
(* FLAGS *)
let output_str_rep = ref true (* output C-def for Popcorn strings *)
let output_rep_rep = ref true (* output C-def for Popcorn arrays *)

(*----------------------------------------------------------------------*)
(* Keeps track of specialized string definitions *)
let lengths_defined = ref [];;
let add_length l =
  let svar = "rep"^(string_of_int l) in
  if (List.mem l !lengths_defined) then
    (svar,"")
  else
    (lengths_defined := l::(!lengths_defined);
     (svar,
      Printf.sprintf "typedef struct { int len; char s[%d]; } %s;\n" l svar))

(*----------------------------------------------------------------------*)
(* Generate a string that resembles a C static declaration for
   the provided buffer *)
let make_static_decl varname str ofs len =
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
  let var = Printf.sprintf "ctx_%s" varname in
  let (svar,svar_decl) = add_length len in
  let decl =
    Printf.sprintf
      "%sstatic %s %s = { %d, { %s }};" svar_decl svar var len data in
  (var,decl)
;;

(*----------------------------------------------------------------------*)
(* Generate a string that resembles a C static declaration for
   the provided buffer
let make_static_decl varname str ofs len =
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
  let var = Printf.sprintf "ctx_%s" varname in
  let decl = 
    Printf.sprintf
      "static char chars_%s[] = { %s };\nstatic struct str_internal %s = { %d, chars_%s };" 
      var data var len var in
  (var,decl)
;;
*)

(*----------------------------------------------------------------------*)
let rec normalize_con c ctxt =
  Talcon.normalize ctxt (Talcon.expand_abbrevs ctxt c)
(*
  let c' = Talcon.normalize ctxt (Talcon.expand_abbrevs ctxt c) in
  (* check if no more expansion needed *)
  let finished = 
    try
      Talcon.eqcon (Talctxt.empty_ctxt) c c';
      true
    with _ -> false in
  (* either done or, iterate again *)
  if finished then c else normalize_con c' ctxt
*)

(*----------------------------------------------------------------------*)
(* Determines whether the given constructor represents a Popcorn function *)
let is_fun_con c ctxt =
  let c = normalize_con c ctxt in
  let rec skip_forall c i =
    match c with
      { Tal.rcon = rc } ->
	(match rc with
	  Tal.Cforall (id, k, c) -> skip_forall c true
	| _ -> (c,i))
  in
  let isCode c =
    match c with
      { Tal.rcon = rc } -> 
	(match rc with
	  Tal.Ccode _ -> true
	| _ -> false)
  in
(*
  Printf.printf "Checking con:\n";
  Talpp.print_con (Format.std_formatter) Talpp.std_options c;
  Format.pp_print_newline (Format.std_formatter) ();
*)
  let (c,skippedForall) = skip_forall c false in
(*
  Printf.printf "Skipped forall: %b, next con =\n" skippedForall;
  Talpp.print_con (Format.std_formatter) Talpp.std_options c;
  Format.pp_print_newline (Format.std_formatter) ();  
*)
  skippedForall && (isCode c)

(*----------------------------------------------------------------------*)
let set_usage_string name =
  (Printf.sprintf 
  "usage : %s [--no-rep-def] [--no-string-def]\n        [--init-code-preamble s] [--init-code-postamble s] tali_file\n"
     name)

let proc_args argv =
  let usage_msg = set_usage_string argv.(0) in

  (* specify here how many non-flagged args we expect *)
  let required_args_done = ref 0 in
  let required_args_expected = 1 in

  let tali_file = ref "" in
  let preamble = ref "" in
  let postamble = ref "" in
  let init_code_preamble s = preamble := s in
  let init_code_postamble s = postamble := s in

  let arg_specs =
    [("--no-rep-def", Arg.Clear output_rep_rep, 
      "Do not output C definition of TAL type representations.");
     ("--no-string-def", Arg.Clear output_str_rep, 
      "Do not output C definition of Popcorn string.");
     ("--init-code-preamble", Arg.String init_code_preamble,
      "Add argument as code at beginning of init function");
     ("--init-code-postamble", Arg.String init_code_postamble,
      "Add argument as code at end of init function") ] 
  in
  let proc_other_arg s = 
    ((match (!required_args_done) with
      0 -> tali_file := s
    | _ -> raise (Arg.Bad("unexpected argument: " ^ s)));
     required_args_done := !required_args_done + 1) in
  (Arg.parse arg_specs proc_other_arg usage_msg;
   if (!required_args_done) < required_args_expected then
     (Printf.printf "%s: missing argument.\n" argv.(0);
      Arg.usage arg_specs usage_msg; exit 1);
   (!tali_file, !preamble, !postamble))
  
(*----------------------------------------------------------------------*)
let main() =
  try
    let (tali_file,init_preamble,init_postamble) = proc_args Sys.argv in
    if not (Sys.file_exists tali_file) then
      (Printf.fprintf stderr "%s: file not found\n" tali_file;
       exit 1);
    
    let outc = Pervasives.stdout in
    let tali = Talbe.read_int tali_file in
    let intname = 
      Filename.chop_extension (Filename.basename tali_file) in
    let ctxt = 
      Array.fold_left
        (fun ctxt (id,con) -> 
          (* XXX call check rather than expand_abbrev *)
	  Talctxt.add_abbrev ctxt id (Talcon.expand_abbrevs ctxt con))
	Talctxt.empty_ctxt tali.Tal.int_abbrevs in
(*
      Array.fold_left 
	(fun ctxt (id,con) -> Talctxt.add_abbrev ctxt id con)
	Talctxt.empty_ctxt tali.Tal.int_abbrevs in
*)
    let buf = Buffer.create 16 in

    let indirect_con con =
      Tal.cprod_b [Tal.cfield con Tal.ReadWrite] in

(*
      (Tal.chptr [] 
	 (Some (Tal.cprod [Tal.cfield con Tal.ReadWrite])) 
	 None in
*)    
    (* For each value label, add a type representation *)
    let val_cons =
      Array.fold_right
	(fun (id,con) results ->
	  (* remove leading _ from id *)
	  let idstrname = 
	    let idstr = Identifier.id_to_string id in
	    if idstr.[0] = '_' then
	      String.sub idstr 1 ((String.length idstr) - 1) 
	    else
	      idstr in
	  (* if the label has a ?, it won't work in C; signal warning *)
	  if not (String.contains idstrname '?') then
	    (* for non-functions, need to indirect the constructor *)
	    (let (con,indirect) =
	      if not (is_fun_con con ctxt) then
		(con,true)
              else
		(con,false) in
	    (* expand type abbreviations *)
	    let con = normalize_con con ctxt in
(*            let con = if !tuple_rep then (indirect_con con) else con in *)
(*
	    Printf.fprintf Pervasives.stderr "after normalizing:\n";
	    Talpp.print_con (Format.err_formatter) Talpp.std_options con;
	    Format.pp_print_newline (Format.err_formatter) ();	  
*)
	    (* generate the type rep *)
	    Buffer.clear buf;
	    Talbinout.Buf.emit_out_con buf con;

	    (idstrname,indirect,Buffer.contents buf)::results)
	  else
	    (Printf.eprintf "Warning: not emitting label %s; contains ?\n"
	       idstrname;
	     results)) tali.Tal.int_vals [] in

    (* Now output the results *)
    Printf.fprintf outc
      "/*** CODE BELOW GENERATED AUTOMATICALLY BY genCinit.exe ****/\n\n";
    if !output_str_rep then
      Printf.fprintf outc 
	"typedef struct str_internal {int size; char *chars;} *string;\n";
    if !output_rep_rep then
      Printf.fprintf outc 
	"typedef struct rep_internal {int size; char chars[1];} *rep;\n";

    (* First output the type representations as static data *)
    let rep_vars =
      List.map
	(function (idstr,indirect,rep) ->
	  let (idrep,decl) = 
	    make_static_decl idstr rep 0 (String.length rep) in
	  Printf.fprintf outc "\n\n/*** %s ***/\n" idstr;
	  Printf.fprintf outc "%s\n" decl;
	  let idlen = String.length idstr in
	  Printf.fprintf outc
	    "static struct str_internal str_%s = { %d, \"%s\" };" 
	    idstr idlen idstr;
	  (idstr,indirect,idrep))
	val_cons in

    (* Now create the init function, registering each of the labels *)
    (* We must not use any registers---the popcorn update function will 
       not properly save callee-saves registers  *)
    Printf.fprintf outc "static void (*upd_sym)(void *clos,string sym,rep r,void *p);\n";
    Printf.fprintf outc "static void (*look_sym)(void *clos,string sym,rep r,void *p);\n";
    Printf.fprintf outc "static void *upd_closure;\n";
    Printf.fprintf outc "static void *look_closure;\n";
    Printf.fprintf outc "static int no_init = 0;\n";
    Printf.fprintf outc "static int __did_update = 0;\n";
    Printf.fprintf outc "void dyninit_%s(void (*l)(void *clos,string sym,rep r,void *p),void *lclos,void (*u)(void *clos,string sym,rep r,void *p),void *uclos,int n) {\n" intname;

    if (init_preamble <> "") then
      Printf.fprintf outc "  %s\n" init_preamble;
    Printf.fprintf outc "  upd_sym = u;\n";
    Printf.fprintf outc "  look_sym = l;\n";
    Printf.fprintf outc "  upd_closure = uclos;\n";
    Printf.fprintf outc "  look_closure = lclos;\n";
    Printf.fprintf outc "  no_init = n;\n";
    
    Printf.fprintf outc "  if (!__did_update) {\n    __did_update = 1;\n";
    List.iter
      (function (idstr,indirect,idrep) ->
	Printf.fprintf outc 
	  "    upd_sym(upd_closure,(string)&str_%s,(rep)&%s,(void *)%c%s);\n" 
	  idstr idrep (if indirect then '&' else ' ') idstr)
      rep_vars;

    Printf.fprintf outc "  }\n";

    if (init_postamble <> "") then
      Printf.fprintf outc "  %s\n" init_postamble;

    Printf.fprintf outc "}\n";

  with e ->
    Printf.printf "Failed with exception %s\n" (Printexc.to_string e)
;;

let _ = main()
;;
      
