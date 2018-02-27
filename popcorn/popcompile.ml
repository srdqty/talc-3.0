(* (c) Greg Morrisett, Neal Glew, Chris Hawblitzel,Frederick Smith    *)
(*     Dan Grossman                                                   *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* We no longer import tal.tali, so you have to extern what you want.
   If we have main, this means we must import tal_exit *)

(* Call map -- here's a top down view of the code generation functions,
 *             where indentation means uses as a helper
 *
 * code_gen  
 *    cg_all
 *       cg_typedecls 
 *       cg_global_decls
 *       cg_exn_decls
 *       cg_fundecl
 *          cg_stmt
 *             cg_exp (also used by cg_bool_exp)
 *                cg_lhs_exp (call cg_exp)
 *                cg_bop (also used by cg_lhs_exp)
 *             cg_bool_exp
 *             cg_fill_holes (also called by cg_exp)
 *    postprocess
 *)  

(* File contents: 
 *  (Note type compilation and env manipulation are now in other modules.)
 *  o Modules and such
 *  o type definitions
 *  o STATE 
 *  o Label names, and main information
 *  o Emit utilities
 *  o Push and pop utilities
 *  o Floating point utilities
 *  o Operator Translation
 *  o Code Generation::Main (bottom up of call map described above)
 *)

let print_comments = ref true
let peephole = true
let debug_malloc = ref false
let dyn_native = ref false

(*********************************Modules and such ****************************)
open Popcomptypes
open Popcompenv
open Numtypes
module T    = Tal
module P    = Popsyntax 
module PT   = Poptype 
module Id   = Identifier
module Peep = Poppeep
module X    = Poperr

type id = Id.identifier

let dc          = T.defcon
let to_i32      = int_to_int32 
let nonsense_id = Id.id_new "nonsense"

let deSome x = match x with Some v -> v | None -> impos "deSome"

exception Break
exception Unimplemented of string

let unimpl s     = raise (Unimplemented ("popcompile: "^s))

let debug s =
  Format.print_string s; Format.print_newline()

(***************************************Types**********************************)

(* Cyclone *)
type code_mark =
    M_Label       of id * T.con option
  | M_ISeq        of T.instruction array
  | M_TypeAbbrev  of id * T.con
      (* Delimiters for templates *)
  | M_TemplateBeg of id * T.con * fenv
  | M_TemplateEnd of T.con
      (* Expression to fill a hole *)
  | M_Fill        of id * P.exp
      (* Jumps from inside templates might need to be patched.
      ** First id is the target, second id is id of the template
      ** containing the jump, if the jump is an inter-template jump. *)
  | M_Jmp   of id * (id * id) option ref * T.con * T.coercion list
  | M_Jcc   of T.condition * id * (id * id) option ref * T.con * T.coercion list
(* End Cyclone *)

type con_or_typ =
    Con of T.con
  | Typ of P.typ

(**************************************** STATE *******************************)
type gen_state = (* Encapsulates the state of the code generator. *)
    { mutable export_cons : T.int_con     list;
      mutable export_vals : (id * T.con)  list;
      mutable import_cons : T.int_con     list;
      mutable import_vals : (id * T.con)  list;
      mutable dyn_rep_dict : (string, id) Dict.dict;   (* hash consing of the static reps of popcorn types *)
      mutable code_marks  : code_mark     list;
      mutable data_blocks : T.data_block  list;
      mutable instrs      : T.instruction list; (* Instr in  current block *)
      mutable label       : id * T.con option;  (* Label for current block *)
      mutable module_string : id (* label for name of module *)
    } 

(* Cyclone *)
(* Eventually put this in gen_state *)
let forward_itjs  = Hashtbl.create 11 (*map labels to inter-template-jump info*)
let tmpl_of_label = Hashtbl.create 11 (*map labels to templates they occur in *)
(* End Cyclone *)

let gen_state = 
  { export_cons     = []; export_vals     = [];
    import_cons     = []; import_vals     = [];
    dyn_rep_dict    = Dict.empty compare;
    code_marks      = []; data_blocks     = [];
    instrs          = []; label = (nonsense_id, None);
    module_string = nonsense_id;
  } 

let add_rep str id = 
   gen_state.dyn_rep_dict <- Dict.insert gen_state.dyn_rep_dict str id
	 
let gen_export_con c = 
  gen_state.export_cons <- (c :: gen_state.export_cons)

let gen_export_val (v1,v2,poptyp) =
  gen_state.export_vals <- ((v1,v2) :: gen_state.export_vals)

let gen_import_con ((i,k,def)as c) = 
  gen_state.import_cons <- (c :: gen_state.import_cons)
	   
let gen_import_val (v1,v2,poptyp) = 
   gen_state.import_vals <- ((v1,v2) :: gen_state.import_vals)

let gen_data_block d = gen_state.data_blocks <- (d :: gen_state.data_blocks)

let in_template() = 
  let rec aux marks depth = 
    match marks with
      [] -> depth <> 0
    | M_TemplateBeg(_,_,_)::tl -> aux tl (depth+1)
    | (M_TemplateEnd _)::tl -> aux tl (depth - 1)
    | _::tl -> aux tl depth
  in aux gen_state.code_marks 0

(* Pushes the block on the current list of code blocks and
 * starts a new block.  Does some simple peephole optimization.
 *)
let flush_code () =
  match gen_state.instrs with
    [] -> ()
  | ilist ->
      let opt = if peephole then Peep.optimize (in_template())
                else (fun x -> x) in
      let iv = Array.of_list(opt(List.rev ilist)) in
      gen_state.code_marks <- ((M_ISeq iv) :: gen_state.code_marks);
      gen_state.instrs     <- []

let gen_code_mark m =
  flush_code();
  gen_state.code_marks <- (m :: gen_state.code_marks)
let gen_set_label  (l,c)     = gen_code_mark(M_Label(l,c)); 
                               gen_state.label <- (l,c)
let gen_code_block (l,c,iv)  = gen_set_label (l,c); gen_code_mark(M_ISeq iv)
let emit           i         = gen_state.instrs <- (i::gen_state.instrs)
let emit_label     l con_opt = flush_code (); gen_set_label(l,con_opt)
let emit_comment   s         = if !print_comments then emit(T.Comment("\t"^s))

(* MWH--add sharing for null strings.  gcc shares all constant strings *)
let null_string_lablref = ref None
let have_null_string_def () =
  match !null_string_lablref with Some n -> true | _ -> false
let add_string s = 
  let slen = String.length s in
  if (slen = 0) && (have_null_string_def ()) then 
    deSome !null_string_lablref
  else
    (let lab = Id.id_new "string"                                  in
    let labref = Id.id_new "string_ref"                            in
    if (slen = 0) then null_string_lablref := Some labref;
    let len = to_i32 slen                                          in
    let l   = if slen = 0 then [] else [T.Dbytes s]                in 
    let char_f = T.cfield (T.pcbytes T.Byte1) T.ReadWrite          in
    gen_data_block(labref,i32_4,None,([T.D4bytes (len,[]); T.Dlabel(lab,[])],
				[T.Pack (T.pcint len,string_con)]));
    gen_data_block(lab,i32_4,
		   Some (T.cprod_b [(T.carray (T.pcint len) char_f)]),
		   (l, [T.Toarray (i32_0,0,char_f)]));
(*  gen_data_block (lab,None,
		  (T.D4bytes (len,[]):: T.Dup ::l,
		   [ T.Pack    (T.pcint len,string_con);
		     T.Toarray (i32_4,0,T.cfield (T.pcbytes T.Byte1)
				  T.ReadWrite)]));
*)
    labref)

let reset_generator module_name = 
  let gs = gen_state in
  gs.export_cons<-[]; gs.export_vals<-[]; gs.import_cons<-[]; 
  gs.import_vals<-[]; gs.code_marks <-[]; gs.data_blocks<-[];
  gs.instrs     <-[]; gs.label<-(nonsense_id, None);
  if (!debug_malloc) then
      gs.module_string <- add_string module_name;
(* Cyclone *)
  Hashtbl.clear forward_itjs;
  Hashtbl.clear tmpl_of_label;
(* End Cyclone *)
  null_string_lablref := None;
  ()
(********************** Label names and main information **********************)

let remove_underscore s =
  if s.[0] = '_' then String.sub s 1 ((String.length s) - 1) else s

let remove_suffix s sfx =
  let s_len = String.length s in
  let sfx_len = String.length sfx in
  if s_len >= sfx_len then
    (let s_sfx = String.sub s (s_len - sfx_len) sfx_len in
    if sfx = s_sfx then
      String.sub s 0 (s_len - sfx_len)
    else
      s)
  else
    s

let pop_exit_label    = Id.id_of_string "_tal_exit"
let null_pointer_label  = tid_internal P.null_pointer_exn
let union_variant_label = tid_internal P.union_variant_exn
let array_bounds_label  = tid_internal P.array_bounds_exn
let exn_handler_label = Id.id_of_string "_pop_exn_handler"
let main_label        = Id.id_of_string "_pop_main"

let main_id = "pop_main"

let bogus_loc = (Gcdfec.seg_of_abs 0 0)
let bogus_typ = P.BooleanType
let bogus_stmt = P.Skip

let pop_exit_con = 
  T.cforall stack1_v T.kstack 
    (T.ccode_l [T.Esp, T.csptr (T.ccons T.cbyte4 (T.cvar stack1_v))])

let jmp_pop_exit () = (* XXX - What about capabilities? *)
  emit (T.Push (T.Immed i32_1,[]));
  emit (T.Mov(T.Reg T.Eax,(T.Addr pop_exit_label,[]))); 
  emit (T.Jmp (T.Reg T.Eax,[T.Tapp (T.StackTail(T.Esp,1))]))
    ;;

let (exn_wrapper_label, exn_wrapper_block) =
  let label = Id.id_of_string "_pop_exn_handler_wrapper" in
  let con = T.ccode_l [(T.Esp, T.csptr T.cempty);
			(T.Eax, exn_con)] in
  let code = [|T.Push (T.Reg T.Eax,[]);
	       T.Call(T.Addr  exn_handler_label,[]);
	       T.Pop (T.Reg T.Eax);
	       T.Push (T.Immed i32_1,[]);
	       T.Jmp (T.Addr pop_exit_label,[T.Tapp (T.Con T.cempty)])
	     |] in
  (label,(label,Some con,code))
  
let tal_main_code_block = 
  let tal_main_label = Id.id_of_string "_tal_main" in
  let b4rw = T.cfield T.cbyte4 T.ReadWrite in
  let tal_main_con =
    T.ccode_l [(T.Esp,T.csptr T.cempty)]
  in
  let tal_main_code = 
    [|T.Push (T.Addr exn_wrapper_label,[]);
      T.Mov (T.Reg T.Ebp, (T.Reg T.Esp,[]));
      T.Call (T.Addr main_label,[
	      T.Tapp (T.Con (T.cempty_cap));
	      T.Tapp (T.Con (T.cempty_cap));
              T.Tapp (T.Con T.cempty);
              T.Tapp (T.Con T.cempty)]);
      T.Pop (T.Reg T.Ebx); 
      T.Push (T.Immed i32_0,[]);
      T.Jmp (T.Addr pop_exit_label,[T.Tapp (T.Con T.cempty)])
    |] 
  in
  (tal_main_label,Some tal_main_con,tal_main_code)

(********** Rewriting Exceptions *******************************************)
(* FMS: We rewrite exceptions so that they carry location information.     *)

(* FMS: add_location is probably slow (reparses the input file each time.)
   For better performance we should cache the line information for a file.
*)
let add_location loc = 
  let loc_str = Gcdfec.string_of_seg loc in
  add_string loc_str
;;

(********** LX runtime type representations (alt from LR stuff) **************)

exception CantRep of P.typ
(* Generate static representations of all of the types mentioned in 
   the environment. Keep a dictionary for hash_consing *)

(* maps the string on a typ to the tal label in the static data that 
   represents that popcorn type. *)
let repbool b =
   let v x = T.cvar (Id.id_of_string x) in 
   if b then v "crtrue" else v "crfalse" 
let repsize s =  
   let v x = T.cvar (Id.id_of_string x) in 
   match s with P.B1 -> v "crb1" | P.B2 -> v "crb2" | P.B4 -> v "crb4" 	       
let rec replistcon typs = 
   let v x = T.cvar (Id.id_of_string x) in 
   match typs with 
      [P.VoidType] -> (v "crnull")
    | [] -> (v "crnull")
    | (hd::tl) -> T.capp (T.capp (v"crcons") (repcon hd)) (replistcon tl) 
and repcon typ = 
   let v x = T.cvar (Id.id_of_string x) in 
   let app f l = List.fold_left T.capp f l in
   match P.compress typ with 
      P.VoidType -> raise (CantRep typ)
    | P.VarType _ -> raise (CantRep typ)
    | P.IntType (b,s) -> 
	app (v "crint") [repbool b; repsize s]
    | P.BooleanType -> v "crbool"
    | P.StringType -> v "crstring"
    | P.CharType -> v "crchar"
(******* unimplemented for floating point types *)
    | P.FloatType | P.DoubleType -> raise (CantRep typ)
    | P.ArrayType (t, eopt) -> (match eopt with Some v ->
	 raise (CantRep typ) 
       | _ -> app (v "crarray") [repcon t])
    | P.FnType (c,vars, typ, typs) when c = P.Cdecl -> 
	 if vars = []  then 
	    (match typ with 
	       P.VoidType -> app (v "crvfn") [replistcon typs]
	     | _ -> app (v "crfn") [repcon typ; replistcon typs])
	 else raise (CantRep typ)
(* Unimplemented for stdcall functions. *)
    | P.FnType (_,_,_,_)  -> raise (CantRep typ)
    | P.TupleType (c,typs) -> (* XXX ignores cap *)
	app (v "crtuple") [replistcon typs]
    | P.NamedType (nref, typs) -> raise (CantRep typ)
    | P.ExnType -> v "crexn"
    | P.Evar _           -> failwith "Popcompile.repcon: impossible"
    | P.MutableTyp _     -> failwith "Popcompile.repcon: impossible"
    | P.UnresolvedTyId _ -> failwith "Popcompile.repcon: impossible"
    | P.RepType _ -> failwith "Popcompile.repcon: impossible"
    | P.ExnconType _ -> failwith "Popcompile.repcon: impossible"

let cg_typ_rep typ = 
  let s i = T.Dlabel (Id.id_of_string (string_of_int i),[]) in 
  let junk = T.D4bytes (to_i32 0,  [T.Subsume T.cbyte4]) in 
  let rec hashcons 
      genstr
      gendatablock = 
    fun arg ->
      let str = genstr arg in 
      try T.Dlabel (Dict.lookup gen_state.dyn_rep_dict str, [])
      with Dict.Absent -> 
	let id = Id.id_new "_tr$" in 
	let db = gendatablock id arg in 
	add_rep str id;
	gen_data_block db;
	T.Dlabel(id, []) in 
  let rec aux typ =
    hashcons P.typ2string 
      (fun id typ -> 
	let data = 
	  match P.compress typ with 
	    P.VoidType -> raise (CantRep typ)
	  | P.VarType _  -> raise (CantRep typ)
	  | P.IntType (b,size) -> [s 1; auxint (b,size)]
	  | P.BooleanType -> [s 2; junk]
	  | P.StringType -> [s 3; junk]
	  | P.CharType -> [s 4; junk]
(******* unimplemented for floating point types *)
          | P.FloatType | P.DoubleType -> raise (CantRep typ)
          | P.RepType _ -> raise (CantRep typ)
          | P.ExnconType _ -> raise (CantRep typ)
	  | P.ArrayType (t, eopt) -> 
	      if not (eopt = None) then raise (CantRep typ) 
	      else [s 5; aux t]
	  | P.FnType (c, vars, typ, typs) when c = P.Cdecl -> 
	      if vars = []  then 
		(match typ with 
		  P.VoidType -> 
		    (match typs with 
		      [P.VoidType] -> [s 7; auxlist []]
		    | _ -> [s 7 ; auxlist typs])
	     	| _ -> [s 6; auxcons (typ, 
				      (match typs with 
					[P.VoidType] ->  []
				      | _ -> typs))])
	      else raise (CantRep typ)
(* Unimplemented for standard call. *)
	  | P.FnType (_) -> raise (CantRep typ)
	  | P.TupleType (c,typs) -> (* XXX ignores cap *) [s 8; auxlist typs]
	  | P.NamedType (nref, typs) -> raise (CantRep typ)
	  | P.ExnType -> [s 0; junk] 
	  | P.Evar _           -> failwith "Popcompile.cg_typ_rep: impossible"
	  | P.MutableTyp _     -> failwith "Popcompile.cg_typ_rep: impossible"
	  | P.UnresolvedTyId _ -> failwith "Popcompile.cg_typ_rep: impossible" 
	in

	let dc = (data, [T.RollTosum (T.capp 
					(T.cvar (Id.id_of_string "reptyp")) 
					(repcon typ))]) in 
	(id, i32_4, None, dc)) typ
  and auxint arg = 
    hashcons (fun (b, size) -> 
      (if b then "t$" else "f$") ^ 
        (match size with P.B1 -> "1" | P.B2 -> "2" | P.B4 -> "4"))
      (fun id (b, size) -> 
	let data =  [if b then s 1 else s 0;
		      match size with 
			P.B1 -> s 0
		      | P.B2 -> s 1
		      | P.B4 -> s 2] in 
	(id, i32_4, Some (T.chptr [] (Some (T.cprod 
		   [T.cfield (T.capp (T.cvar (Id.id_of_string "repbool")) 
				(repbool b)) T.ReadWrite; 
		    T.cfield (T.capp (T.cvar (Id.id_of_string "repsize")) 
				(repsize size)) T.ReadWrite])) None), 
	 (data, [])))
      arg
  and auxcons arg = 
    hashcons (fun (t1,t2) -> 
      let l = (List.map P.typ2string t2) in 
      let l = if l = [] then ["&&"] else l in 
      String.concat "&&" (P.typ2string t1 :: l))
      (fun id (t1,t2) -> 
	let data = [ aux t1; auxlist t2] in
	(id, i32_4, Some (T.chptr [] (Some (T.cprod 
	    [T.cfield (T.capp (T.cvar (Id.id_of_string "reptyp")) 
			 (repcon t1)) T.ReadWrite; 
	     T.cfield (T.capp (T.cvar (Id.id_of_string "reptyplist")) 
			 (replistcon t2)) T.ReadWrite])) None) , (data,[])))
      arg
  and auxlist arg = 
    hashcons (fun typs ->      
      let str = String.concat  "::" ("" :: (List.map P.typ2string typs)) in 
      if str = "" then "[]" else str)
      (fun id typs -> 
	let data = 
	  match typs with 
	    [] -> [s 0; junk]
	  | (hd::tl) -> [s 1; auxcons (hd, tl) ] in 
	let dc = (data, 
		  [T.RollTosum (T.capp (T.cvar (Id.id_of_string "reptyplist"))
				  (replistcon typs))]) in 
	(id, i32_4, None, dc)) arg in 
  match aux typ with 
    T.Dlabel (id, []) -> id
  | _ -> failwith "cg_typ_rep : cant happen"

let make_exp e = 
  {P.raw_exp = e; P.exp_typ = Some P.BooleanType; P.exp_loc = bogus_loc;
   P.exp_un_after = P.mt_varset; }

(******** End  LX runtime type representations *************)
(******** native runtime type representations *************)

(* Use TAL's native representations for the type *)
let compile_native_rep_data =
  let b = Buffer.create 100 in 
  fun con ->
    (Buffer.clear b;
     Talbinout.Buf.emit_out_con b con;
     let str = Buffer.contents b in 
     let len = to_i32 (String.length str) in 
     T.Drep (T.RCon con, ref (Some str)))
  
let cg_native_rep_con = 
  let b = Buffer.create 100 in 
  let fmt,o = Format.std_formatter, Talpp.std_options in 
  function con ->
    (let rec hashcons 
	genstr
	gendatablock = 
      fun arg ->
	let str = genstr arg in 
	try 
	  Dict.lookup gen_state.dyn_rep_dict str
	with Dict.Absent -> 
	  let id = Id.id_new "_tr$" in 
	  let db = gendatablock id arg in 
	  add_rep str id;
	  gen_data_block db;
	  id in 
    let con2rep id con =
      let di = compile_native_rep_data con in
      (id, i32_4, Some (T.cprod_b [(T.cr (T.RCon con))]), ([di],[])) in
    let con2str con =
      (Buffer.clear b;
       Talbinout.Buf.emit_out_con b con;
       Buffer.contents b) in
    hashcons con2str con2rep con)

let cg_native_rep typ = 
  cg_native_rep_con (typ2con typ)

(****** End native runtime type representations ***********)

(* Entry points for type representations *)

let repcon typ = 
  if !dyn_native then typ2con typ
  else repcon typ

let repterm typ =
  if !dyn_native then cg_native_rep typ
  else cg_typ_rep typ

let repterm_tal con =
  if !dyn_native then cg_native_rep_con con
  else failwith "repterm_tal not supported for LX"


(*********************************Emit Utilities*****************************)

let branch_coercion env = 
  (T.Tapp (T.Con cap2_c))::(T.Tapp (T.Con cap1_c))::
  (T.Tapp (T.Con stack2_c))::(T.Tapp (T.Con stack1_c))::
  ((List.rev (List.map
		(fun v -> T.Tapp(T.Con (T.cvar v))) (env_names env))) @
   (List.rev (List.map 
		(fun v -> T.Tapp(T.Con (T.cvar(tid_tyv v)))) (env_tyvars env))))

let exncase_coercion env exnname_arg_con =
  (branch_coercion env) @ [T.Tapp (T.Con exnname_arg_con)]

let fallthru env = 
  let tcons = (List.map (fun v -> T.cvar(tid_tyv v)) (env_tyvars env)) @
    (List.map T.cvar (env_names env)) in
  emit(T.Fallthru (tcons @ [stack1_c;stack2_c
			       ;cap1_c
			       ;cap2_c
                           ]))

let exncase_fallthru env exnname_arg_con =
  let tcons = (List.map (fun v -> T.cvar(tid_tyv v)) (env_tyvars env)) @
    (List.map T.cvar (env_names env)) in
  emit(T.Fallthru (exnname_arg_con::
		   (tcons @ [stack1_c;stack2_c
				;cap1_c
				;cap2_c
                           ])))

let mk_label_con env =
  let exn_part = exn_stack_con (env_stack2 env) (env_cap2 env) in
  let stack_state = 
    let esp_set = T.ms_set_reg (env_regs env) T.Esp 
	(T.csptr (T.cappend (env_stack1 env) exn_part)) in
    T.ms_set_reg esp_set T.Ebp (T.csptr exn_part) in
  let ms = T.ms_set_cap stack_state (env_cap env) in
  close_code (env_names env) (env_tyvars env) (T.ccode_ms ms)

let mk_exncase_label_con env exnname_arg_var =
  T.cforall exnname_arg_var T.kmem (mk_label_con env)

(* In Cyclone jumps between templates have to be patched.
   Therefore we added emit_jmp, emit_jcc, emit_btagi,
   and emit_btagvar. *)
let emit_jmp env label coercion =
(* Cyclone *)
  if not(outermost env)
  then
    (* Make sure we have a coercion (not just an empty coercion)
       in case the jump is an inter-template jump. *)
    let coercion   = branch_coercion env in
    let target_con = mk_label_con    env in
    gen_code_mark(M_Jmp(label,ref None,target_con,coercion))
  else
(* End Cyclone *)
    emit (T.Jmp(T.Addr label,coercion))

let emit_jcc env cc label coercion =
(* Cyclone *)
  if not(outermost env)
  then
    begin 
      let coercion   = branch_coercion env in
      let target_con = mk_label_con    env in
      gen_code_mark(M_Jcc(cc,label,ref None,target_con,coercion))
    end
  else
(* End Cyclone *)
    begin
      emit (T.Jcc(cc,(label,coercion),None))
    end

let emit_btagi name (reg,i,(label,coercion),condition) env =
  begin
    emit (T.Nameobj(name,T.Reg reg));
    emit (T.Cmp((T.Reg reg,[]),(T.Immed i,[])));
(* Cyclone *)
    if not(outermost env)
    then
      let coercion   = branch_coercion env in
      let target_con = mk_label_con    env in
      gen_code_mark(M_Jcc(condition,label,ref None,target_con,coercion))
    else
(* End Cyclone *)
      begin
	emit (T.Jcc(condition,(label,coercion),None));  
      end;
    emit (T.Coerce(T.Reg reg,[T.Forgetname]));
    emit (T.RemoveName name)
  end

let emit_btagvar name (reg,i1,i2,(label,coercion),condition) env =
  begin
    emit (T.Nameobj(name,T.Reg reg));
    emit (T.Mov(T.Reg T.Ecx,(T.Prjr((T.Eax,[]),i1,None),[])));
    emit (T.Cmp((T.Reg T.Ecx,[]),(T.Immed i2,[])));
(* Cyclone *)
    if not(outermost env)
    then
      let coercion   = branch_coercion env in
      let target_con = mk_label_con    env in
      gen_code_mark(M_Jcc(condition,label,ref None,target_con,coercion))
    else
(* End Cyclone *)
      begin emit (T.Jcc(condition,(label,coercion),None)); end;
    emit (T.Coerce(T.Reg reg,[T.Forgetname]));
    emit (T.RemoveName name)
  end;;

let emit_lab env l =  emit_label l (Some (mk_label_con env))
let emit_lab_nocon env l = 
(* Cyclone *) 
  if not(outermost env)
  then (fallthru env; emit_lab env l)
  else
(* End Cyclone *)
    emit_label l None
let emit_exncase env l exnname_arg_var = 
  emit_label l (Some (mk_exncase_label_con env exnname_arg_var))

let handler_code_type env =
  let stack1 = 
    match (env_stack1 env).T.rcon with
      T.Ccons(x,y) -> y
    | _ -> failwith "impossible: Popcompile.handler_code_type"
  in
  let code = 
    T.ccode_l_cap [ T.Eax, exn_con;
		    T.Esp, T.csptr (handler_con 
				      stack1         (env_stack2 env) 
				      (env_cap1 env) (env_cap2 env))]
                  (env_cap env) 
  in
  close_code (env_names env) (env_tyvars env) code

(* FMS: Move to popcomptypes. *) 
let builtin_raise_code_type =
  let exn_stack = exn_stack_con stack2_c cap2_c in
  let state = T.ccode_l_cap [ (T.Esp,T.csptr (T.cappend stack1_c exn_stack));
			      (T.Ebp,T.csptr exn_stack)]
      (T.cjoin [cap1_c; cap2_c])
  in
  close_code [] [] state
;;

let builtin_coercion env = []
(* Coercion for builtin exception is not needed if they're always forward jumps.
   *)
(*
  let bottom = if env_in_try env then T.cempty else stack1_c in
  [ T.Tapp (T.Con (env_cap2 env));
    T.Tapp (T.Con (env_cap1 env));
    T.Tapp (T.StackTail(T.Ebp,1));
    T.Tapp(T.StackSlice(T.Esp, 0, env_s1len env, bottom))]
  *)  

  
(* Cyclone *)
(* FMS: Null checks must be patched specially for cyclone.  The instruction
jcc pop_never_null resolves pop_never_null as a relative address when it
must be absolute for our application. 
*)
let emit_null_check name (reg,i,coercion,condition) env =
  if not (outermost env) then
    begin
      let not_condition = T.negate_condition condition in
      let skip_label = Id.id_new "skip"  in
      (* FMS: Code pilfered from emit_btagi. *)
      emit (T.Nameobj(name,T.Reg reg));
      emit (T.Cmp((T.Reg reg,[]),(T.Immed i,[])));
      (* This relative jump okay since target is always within template. *)
      emit (T.Jcc(not_condition,(skip_label,coercion),None));  
      emit (T.Mov(T.Reg T.Eax,(T.Addr null_pointer_label,
			       builtin_coercion env))); 
      emit (T.Jmp (T.Reg T.Eax,[]));
      emit_label skip_label None; (* REALLY no_con *)
      ()
    end
  else
    emit_btagi name (reg,i,(null_pointer_label,builtin_coercion env),condition) env
;;

(* Takes a packed array in register arr_reg and an index in the
   register index_reg. Unpacks the array. Checks that the index is in
   bounds. On success, the unpacked/flattened array is in the register
   arr_reg.

   Warning: after this the index and the array are unpacked!!
*)
let emit_array_check env arr_reg index_reg =
  let array_size_var = Id.id_new "?sz" in
  let index_var      = Id.id_new "i"   in
  emit(T.Unpack(array_size_var, arr_reg, (T.Reg arr_reg,[])));
  emit(T.Unpack(index_var,      index_reg, (T.Reg index_reg,[])));
  emit(T.Cmp((T.Reg index_reg,[]),(T.Prjr((arr_reg,[]),i32_0,None),[])));
  
  if not (outermost env) then 
    begin 
          (* FMS:  XXX This Jcc is broken for templates!! *)
      let skip_label = Id.id_new "skip" in
      emit (T.Jcc (T.Below, (skip_label,[]), None));
      emit (T.Mov(T.Reg T.Eax,(T.Addr array_bounds_label,
			       builtin_coercion env)));
      emit (T.Jmp (T.Reg T.Eax,[]));
      emit_label skip_label None;
      ()
    end
  else 
    emit(T.Jcc (T.AboveEq,(array_bounds_label,
			   builtin_coercion env),None));
  emit(T.Mov (T.Reg arr_reg,(T.Prjr((arr_reg,[]),i32_4,None),[])));
  ()
  
(* End Cyclone *)

(* Check that r != null, raise null exception if r == null. *)
let check_not_null pos_loc r env = 
  let name = Id.id_new "n" in
  emit_null_check name (r,i32_0,[],T.Eq) env

(********************** Push and pop utilities **************************)

let push_con env reg reg_con = 
  emit (T.Push (T.Reg reg, []));
  env_push_con env reg_con

let push env reg t = push_con env reg (typ2con t) 

let pop reg = emit(T.Pop (T.Reg reg))

let pop_free i = (* pop freeing i words on the stack. *)
  (if i>0 then emit(T.ArithBin(T.Add,T.Reg T.Esp,T.Immed (to_i32 (4*i)))))

(* peek - load the ith word from the top of the stack without popping. *)
let peek reg i = 
  emit(T.Mov (T.Reg reg,(T.Prjr ((T.Esp,[]),to_i32 (4*i),None),[])))

(* Cyclone *)
let abbrev_con con =
  match con.T.rcon with
    T.Cvar _  -> con (* Don't bother to abbreviate a variable *)
  | T.Cprim _ -> con (* or a primitive type *)
  | _ ->
      let abbrev_var = Id.id_new "t"         in 
      let abbrev_con = dc(T.Cvar abbrev_var) in
      gen_code_mark(M_TypeAbbrev(abbrev_var,con));
      abbrev_con

let cg_get_rgn env =
  begin match (snd (cyc_get_cg env)).T.rcon with
    T.Ctrgn(pre,post,t) -> (pre,post,t)
  |  _ -> impos "cg_get_rgn: bad type for cg_region."
  end;;
    
let cg_push env ti ls hs post_con_opt =
  let (pre,post,t) = cg_get_rgn env in
  let con' =  T.ctrgn (pre,post_con_opt,(ti,ls,hs)::t) in
  cyc_set_cg env con'
;;

(* cg_pop_hole: Remove a hole from the type of the current cg region.
   FIX: if a template is dumped twice, there can be two instances of a
   hole; this function currently removes BOTH instances. Rather, we
   should specify both which template copy and which hole we want to
   remove.
   FIX: if we try to remove a hole that does not exist, this function
   won't complain. A complaint would be useful feedback.  *)
let cg_pop_hole env h =
  let (pre,post,t) = cg_get_rgn env in
  let t' =
    List.map (* Not too efficient, oh well... *)
      (fun (ti,ls,hs) ->
        (ti,ls,List.filter (fun (l,c) -> l<>h) hs))
      t in
  let con' = T.ctrgn (pre,post,t') in
  cyc_set_cg env con'

let cg_pop_free env n =
  if n=0 then ( ) else
  let (pre,post,t) = cg_get_rgn env in
  let cg_id = cyc_get_cg_id env in
  let rec aux n l =
      match n,l with
	0,_ -> ()
      |	_,((ti,_,_)::tl) -> (emit(T.CgForget (cg_id,ti)); aux (n-1) tl)
      |	_ -> impos "popcompile:cg_pop_free: n too large."
  in
  aux n t;;
(* End Cyclone *)

(************************* Type Sizes/Floating Point *************************)

(* utilities *)
let is_word  t    = (P.words_in_typ t = 1)
let is_float t    = 
  match P.compress t with P.FloatType|P.DoubleType -> true | _ -> false
let top_of_stack  = T.Prjr ((T.Esp,[]),i32_0,None)
let con_is_word c = (words_in_con c = 1)
let toc_is_word = function
    Typ t -> is_word t
  | Con c -> con_is_word c

(* add static floating point data *)
let add_float f32 = 
  let lab = Id.id_new "flt" in
  gen_data_block (lab,i32_4, Some (T.cprod_b [T.cfield T.pcfloat32 T.Read]),([T.Dfloat32 f32],[]));
  lab

let add_double f64 =
  let lab = Id.id_new "dbl" in
  gen_data_block (lab,i32_8,Some (T.cprod_b [T.cfield T.pcfloat64 T.Read]),([T.Dfloat64 f64],[]));
  lab

(* Identifiers for positive and negative infinity. *)
let neginf = ref None;;
let posinf = ref None;;

(* "the standard location" for x is:
 *   o The top of the floating point stack, if x has type float or double
 *   o Eax, if x has any other type
 *)

(* load a double or a float *)
let load_fp sc  src = emit(T.FPsomeargs (T.Fld, (T.FPgenop (sc,src))))
let load_float  src = load_fp T.Byte4 src
let load_double src = load_fp T.Byte8 src
let load_eax    src = emit (T.Mov (T.Reg T.Eax, (src,[])))
let load_ebx    src = emit (T.Mov (T.Reg T.Ebx, (src,[])))
let load_ecx    src = emit (T.Mov (T.Reg T.Ecx, (src,[])))
let load_edx    src = emit (T.Mov (T.Reg T.Edx, (src,[])))
let load_gc      gc = emit (T.Mov (T.Reg T.Eax, gc))  (* : genop coerce -> unit *)

(* move an object from memory at src to its standard location *)
let load typ src =
  match P.compress typ with
    P.FloatType  -> load_float  src
  | P.DoubleType -> load_double src
  | _ -> load_eax src

(* pop a double or a float off the floating point stack into memory at dest *)
let store_fp  sc dest = emit(T.FPsomeargs (T.Fstp,(T.FPgenop(sc, dest))))
let store_float  dest = store_fp T.Byte4 dest
let store_double dest = store_fp T.Byte8 dest

(* move an object from its standard location to memory at dest *)
let store typ dest =
  match P.compress typ with
    P.FloatType  -> store_float  dest
  | P.DoubleType -> store_double dest
  | _ -> emit (T.Mov(dest,(T.Reg T.Eax,[])))

(* push from standard location onto the stack *)
let push_std typ =
  match P.compress typ with
    P.FloatType ->
      emit (T.ArithBin (T.Sub,T.Reg T.Esp, T.Immed i32_4));
      emit (T.FPsomeargs (T.Fstp, (T.FPgenop (T.Byte4, top_of_stack))))
  | P.DoubleType -> 
      emit (T.ArithBin (T.Sub,T.Reg T.Esp, T.Immed i32_8));
      emit (T.FPsomeargs (T.Fstp, (T.FPgenop (T.Byte8, top_of_stack))))
  | _ -> emit (T.Push (T.Reg T.Eax, []))

(* pop from stack into the standard location *)
let pop_std typ =
  match P.compress typ with
    P.FloatType ->
      emit (T.FPsomeargs (T.Fld, (T.FPgenop (T.Byte4, top_of_stack))));
      emit (T.ArithBin (T.Add,T.Reg T.Esp, T.Immed i32_4))
  | P.DoubleType -> 
      emit (T.FPsomeargs (T.Fld, (T.FPgenop (T.Byte8, top_of_stack))));
      emit (T.ArithBin (T.Add,T.Reg T.Esp, T.Immed i32_8))
  | _ -> emit (T.Pop (T.Reg T.Eax))

(* push from standard location onto the stack, adjusting environment *)
let push_any env typ =
  push_std typ;
  let con = typ2con typ in
  env_push_con env con

(* push a value of typ from the src memory location onto the stack. *)
let push_mem_to_stack src toc =
  if toc_is_word toc then
    emit(T.Push(src,[]))
  else
    begin
      load_double src;
      emit (T.ArithBin (T.Sub,T.Reg T.Esp, T.Immed i32_8));
      store_double top_of_stack
    end

(* pop a value of typ off the stack and into the memory location dest. *)
let pop_stack_to_mem typ dest =
  if is_word typ then
      emit(T.Pop dest)
  else
    begin
      load_double top_of_stack;
      store_double dest;
      pop_free 2
    end

(* memory to memory move (through temporary register reg, if word-sized) *)
let mem_to_mem typ src reg dest =
  if is_word typ then
    begin
      emit(T.Mov(T.Reg reg, (src,[])));
      emit(T.Mov(dest,(T.Reg reg,[])))
    end
  else
    begin
      load_double src;
      store_double dest
    end

let delete_fp_reg typ =
  if is_float typ then
    emit (T.FPsomeargs (T.Ffree, T.FPstack 0))
  else
    ()

(* Convenient prjr function when no coercions are needed. *)
let prjr reg offset = T.Prjr((reg,[]),offset,None)
let prjl0 label = T.Prjl((label,[]),i32_0,None)

(************************* Operator Translation *****************************)
let arith_op p =
  match p with
    P.Plus   -> T.Add | P.Times  -> T.Imul2
  | P.Minus  -> T.Sub | P.Bitand -> T.And 
  | P.Bitor  -> T.Or  | P.Bitxor -> T.Xor
  | _ -> impos "arith_op: Expected arithmetic operator."

let arithsr_op p =
  match p with
    P.Bitlshift -> T.Shl | P.Bitlrshift -> T.Shr | P.Bitarshift -> T.Sar
  | _ -> impos "arithsr_op: Expected arith_sr operator."

let cond_op p = 
  match p with
    P.Gt   -> T.Greater  | P.Lt  -> T.Less  | P.Gte  -> T.GreaterEq
  | P.GtU  -> T.Above    | P.LtU -> T.Below | P.GteU -> T.AboveEq 
  | P.Lte  -> T.LessEq   | P.Eq  -> T.Eq    | P.Neq  -> T.NotEq
  | P.LteU -> T.BelowEq
  | _ -> impos "cond_op : Expected conditional op."

(******************************Debugging GC_malloc***************************)
let current_file_name   = "_current_file"
let current_line_name   = "_current_line"

(* replacement for instruction call to malloc *)
let emit_debug_malloc nameref szb mallocref =
  (* NOTE--no need to save since it will get clobbered by malloc anyway *)
  (* count the number of emitted instructions and move into current_line *)
  let num_instrs = List.length gen_state.instrs in
  emit (T.Mov(T.Prjl (((tid_val current_line_name),[]),i32_0,None),
	      (T.Immed (to_i32 num_instrs),[T.Subsume T.cbyte4])));
  (* copy gs.module_string into current_file *)
  let (label,_) = gen_state.label in
  (* XXX stack-trace stuff already does this; could figure out
     a way to share; could just hash-cons strings in general ... *)
  let labelstr = add_string (Id.id_to_string label) in
  load_eax (T.Addr labelstr);
  store P.StringType (T.Prjl (((tid_val current_file_name),[]),i32_0,None));
  (* call malloc with given arguments *)
  emit (T.Malloc (nameref,szb,mallocref))

let emit_malloc =
  if !debug_malloc then emit_debug_malloc
  else
    (fun nameref szb mallocref ->
      emit (T.Malloc (nameref,szb,mallocref)))

(********** Call stack traces **********************************************)

let cstk_type_label   = tid_type P.cstk_type
let cstk_type_info = info_structdecl P.cstk_type_def
let cstk_type_con = typ2con (P.NamedType (ref P.cstk_type, []))
let global_cstk_label = tid_val  P.global_cstk
let active_cstk_label = tid_val  P.active_cstk
let active_exn_label  = tid_val  (P.active_exn^"_pkt")

let local_cstk_name = "_?local_cstk"

(* Reset the global_cstk based on the local_cstk *)
(* global_cstk = local_cstk *)
(* Clobbers: ECX  *)
let cstk_reset env =
  (* lookup variable local_cstk *)
  let offset = 
    (try env_local_var_offset env local_cstk_name
    with Not_found -> impos "cstk_reset: call_stack variable undefined.")
  in
  let ecx = T.Reg T.Ecx in
  emit (T.Mov (ecx, (prjr T.Esp (to_i32(4*offset)),[])));
  emit (T.Mov (prjl0 global_cstk_label,(T.Reg T.Ecx,[])))
;;

let cstk_fn_entry env fn_name = 
  let fn_nm = add_string fn_name in
  (* Malloc a new list cell. *)
  (* global_cstk = ^ctype_list(fn_name,global_cstk); *)
  let mallocref = T.Mprod [T.Mbytes T.Byte4;T.Mbytes T.Byte4] in 
  let   nameref = Id.id_new "r"                               in
  emit_malloc nameref i32_8 (Some mallocref);
  emit (T.Mov (prjr T.Eax i32_0, (T.Addr fn_nm,[])));
  load_ecx (prjl0 global_cstk_label);
  emit (T.Mov (prjr T.Eax i32_4, (T.Reg T.Ecx,[])));
  emit(T.ForgetUnique nameref);
  emit(T.Coerce(T.Reg T.Eax,(roll_struct cstk_type_info P.cstk_type [])@
		[T.Forgetname]));
  emit(T.Mov(prjl0 global_cstk_label,(T.Reg T.Eax,[])));
  (* local_cstk_name = global_cstk *)
  emit(T.Push (prjl0 global_cstk_label,[]));
  let env = env_add_local_var env local_cstk_name cstk_type_con in
  env
;;

(* Code to be executed immediately prior to raise
   EAX is preserved and assumed to hold the exn to be raised.
   Use conditional move to avoid additional block and jump..
   Clobbers: ECX,EDX *)
let cstk_raise () =
  (* active_cstk = (active_exn == eax) ? active_cstk : global_cstk;
     --encode the above with a conditional move. 
     -- global_cstk and local_cstk are equal in this context.
     *)
  load_ecx (prjl0 active_exn_label); (* prjl probably! *)
  load_edx (prjl0 active_cstk_label);
  emit(T.Cmp ((T.Reg T.Eax,[]), (T.Reg T.Ecx,[])));
  emit(T.Cmovcc (T.NotEq,T.Edx, (prjl0 global_cstk_label,[])));
  emit(T.Mov (prjl0 active_cstk_label,(T.Reg T.Edx,[])));
  emit(T.Mov (prjl0 active_exn_label,(T.Reg T.Eax,[])));
  ()
    ;;

(******************************Code Generation::Main***************************)

let cg_builtin_raise builtin_exn =
(*  let con = builtin_raise_code_type in *)
  let label = tid_internal builtin_exn in
  let pkt = tid_val (builtin_exn^"_pkt") in
  emit_label label None;
(*  emit_label label (Some (con)); *)
  emit (T.Mov(T.Reg T.Eax,(prjl0 pkt,[])));
(* Empty the floating point stack. *)
  emit(T.FPnoargs T.Finit);
  (if !P.stack_traces then cstk_raise ());
  emit (T.Mov (T.Reg T.Esp,(T.Reg T.Ebp,[])));
  emit (T.Pop (T.Reg T.Ebx));
  emit (T.Jmp (T.Reg T.Ebx,[]))
;;

(* Code generate a cast from an expression of type tsrc to type tdest
   Assumes the value starts off in Eax, if it is not a float.
   If a float, assume starts on top of the fp stack.
   Coercions to integers are returned in Eax.
   Coercions to floats are returned on the fp stack.
   MWH 6/16 -- added code to allow casting from a ReadWrite TupleType
     to a ReadOnly one.  Same rules as for integers apply
 *)
let cg_cast tsrc tdest =
  let movpart sign_extend reg_part1 reg_part2 =
    emit (T.Movpart (not sign_extend, T.Reg T.Eax, reg_part1, T.Reg T.Eax,
		     reg_part2))
  in
  let part s = match s with P.B1 -> T.RPl | P.B2 -> T.RPx | P.B4 -> T.RPe in 
  let gen_cast (b,s) (be,se) =
    if s=se then () 
    else if P.size_leq se s 
    then (* s wider se *)
      begin if not be
      then (*   zero-fill *) movpart false (part s) (part se)
      else (* sign-extend *) movpart true  (part s) (part se)
      end
    else  
      let mask s =  
	match s with P.B1 -> 0xFF | P.B2 -> 0xFFFF | P.B4 -> 0xFFFFFFFF(* XXX *)
      in
      if se=P.B4 then () 
      else emit(T.ArithBin(T.And,T.Reg T.Eax, T.Immed (to_i32 (mask se))))
  in
  let cast_tuple tdest tsrc =
    match tdest, tsrc with
      P.TupleType(P.ReadOnly,_),P.TupleType(P.ReadWrite,_) ->
	let c = typ2con tdest in
	emit(T.Coerce(T.Reg T.Eax,[T.Subsume c]))
    | t1, t2 ->
      	let ts1 = P.typ2string t1 in
      	let ts2 = P.typ2string t2 in
      	impos ("Illegal cast from "^ts2^" to "^ts1) in
  let cast_int tdest tsrc =
    match tdest,tsrc with
      P.IntType(b,s)  ,P.IntType(be,se) -> gen_cast (b,s) (be,se)
    | P.CharType      ,P.IntType(be,se) -> gen_cast (false,P.B1) (be,se)
    | P.IntType(b,s)  ,P.CharType       -> gen_cast (b,s) (false,P.B1)
    | t1,t2 when t1=t2 -> () (* XXX - may be wrong *)
    | t1,t2 -> cast_tuple tdest tsrc in
  let b4_to_float () =
    emit (T.Push (T.Reg T.Eax, []));
    emit (T.FPsomeargs (T.Fild, 
	    (T.FPgenop (T.Byte4,T.Prjr ((T.Esp,[]),i32_0,None)))));
    pop_free 1 in
  (* -----------------------------------------------------------------*)
  let tdest,tsrc = P.compress tdest, P.compress tsrc in
  match tdest, tsrc with
    (P.DoubleType | P.FloatType), (P.DoubleType | P.FloatType) -> ()
  | (P.DoubleType | P.FloatType), P.IntType (false,P.B4) ->
      (* An unsigned 32-bit int.  Since intel only provides
       * instructions for loading signed ints into the fp stack, we
       * will cast to long (64-bits) and then load the long. *)
      unimpl "Cast of 32-bit unsigned to float type"
  | (P.DoubleType | P.FloatType), 
      (P.IntType(false,(P.B1|P.B2)) | P.CharType) ->
      (* cast to unsigned B4, then cast to float *)
      cast_int (P.IntType (false,P.B4)) tsrc;
      b4_to_float ()
  | (P.DoubleType | P.FloatType), (P.IntType(true,_)) ->
      cast_int (P.IntType (true,P.B4)) tsrc;
      (* cast to signed B4, then cast to float *)
      b4_to_float ()
  | (P.IntType (_,_) | P.CharType), (P.DoubleType | P.FloatType) ->
      emit (T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed (to_i32 4)));
      emit (T.FPsomeargs (T.Fistp,
	     (T.FPgenop (T.Byte4,T.Prjr ((T.Esp,[]),i32_0,None)))));
      pop T.Eax
  | _,_ -> cast_int tdest tsrc
      
(* Code generate a binary operator. Changed for left to right evaluation order.
 * Pre-conditions:
 *    o Top of stack contains the lhs
 *    o EAX contains the rhs
 * Post-conditions:
 *    o Stack popped one element
 *    o Result in EAX
 * (No return value, caller responsible for updating environment if necessary)
 *
 * Floating point
 * Pre-conditions for floating point arguments:
 *    o Top of gp stack contains lhs
 *    o Top of fp stack contains rhs
 * Post-conditions for floating point results:
 *    o gp stack popped one element
 *    o Result on top of fp stack
 *
 * Dave: need the type of the left-hand side to determine how to load floating
 * point values from the stack
 *
 * Requirement: cg_bop does not use ESI or EDI (assumed by cg_lhs_exp)
 *) 
let cg_bop outermost p lhstyp = (* : P.Primop -> P.typ -> unit *)
  let div_or_mod is_signed is_mod =
    (* Shuffle the places *)
    emit(T.Mov (T.Reg T.Ecx,(T.Reg T.Eax,[])));
    emit(T.Pop(T.Reg T.Eax));
    (* Check for division by zero *)
    let id = Id.id_new "i" in
    emit (T.Unpack (id, T.Ecx, (T.Reg T.Ecx,[])));
    emit (T.Cmp ((T.Reg T.Ecx,[]), (T.Immed i32_0,[])));
    if not outermost then 
      let skip_label = Id.id_new "skip" in
      emit (T.Jcc (T.Above,(skip_label,[]),None));
      emit (T.Mov(T.Reg T.Eax,
		  (T.Addr Objfile.division_by_zero_error_label,[])));
      emit (T.Jmp(T.Reg T.Eax, []));
      emit_label skip_label None;
      ()
    else 
      emit (T.Jcc (T.BelowEq,(Objfile.division_by_zero_error_label,[]),None));
    (* Do the operation *)
    if is_signed
    then begin
      (* Edx = sign extension of Eax *)
      emit(T.Conv T.Cdq);
      emit(T.ArithMD (T.Idiv,T.Reg T.Ecx))
    end
    else begin
      (* Edx = 0 since we are performing unsigned division *)
      emit(T.Mov (T.Reg T.Edx,(T.Immed i32_0,[])));
      emit(T.ArithMD (T.Div,T.Reg T.Ecx))
    end;
    if is_mod then emit(T.Mov (T.Reg T.Eax, (T.Reg T.Edx,[])))
  in
  match p with
    (P.Plus | P.Times | P.Bitand | P.Bitor | P.Bitxor) ->
      (* These are commutative operations, so we can optimize the shuffling *)
      emit(T.Pop(T.Reg T.Ebx));
      emit(T.ArithBin(arith_op p,T.Reg T.Eax,T.Reg T.Ebx))
  | P.Minus -> 
      (* Not commutative *)
      emit(T.ArithBin(arith_op p,T.Prjr((T.Esp,[]),i32_0,None),T.Reg T.Eax));
      emit(T.Pop(T.Reg T.Eax));
  | (P.Bitlshift | P.Bitlrshift | P.Bitarshift) ->
      (* rhs must go into ECX *)
      emit(T.Mov(T.Reg T.Ecx,(T.Reg T.Eax,[])));
      emit(T.Pop(T.Reg T.Eax));
      emit(T.ArithSR(arithsr_op p, T.Reg T.Eax, None));
  | P.TimesU ->
      (* XXX - We should check for overflow. *)
      emit(T.Mov(T.Reg T.Ecx,(T.Reg T.Eax,[])));
      emit(T.Pop(T.Reg T.Eax));
      emit(T.ArithMD (T.Mul,T.Reg T.Ecx))
  | P.Div  -> div_or_mod true  false
  | P.Mod  -> div_or_mod true  true
  | P.DivU -> div_or_mod false false
  | P.ModU -> div_or_mod false true
  | (P.Gt | P.GtU | P.Lt | P.LtU | P.Gte | P.GteU | P.Lte | P.LteU 
     | P.Eq | P.Neq) ->
      emit(T.Mov(T.Reg T.Ecx,(T.Reg T.Eax,[])));
      emit(T.Pop(T.Reg T.Ebx));
      emit(T.Mov(T.Reg T.Eax, bool false));
      emit(T.Cmp ((T.Reg T.Ebx,[]), (T.Reg T.Ecx,[])));
      emit(T.Setcc (cond_op p,T.Reg T.Eax))
  (* floating point operations *)
  | (P.PlusF | P.TimesF | P.MinusF  | P.DivF ) ->
      let fp_op =
  	match p with
	  P.PlusF  -> T.Fadd
  	| P.MinusF -> T.Fsubr
  	| P.TimesF -> T.Fmul
  	| P.DivF   -> T.Fdivr
  	| _        -> impos "fp_arithop 1" in
      let sl,sz = 
	match lhstyp with
	  P.FloatType -> T.Byte4, 1
	| P.DoubleType -> T.Byte8, 2
	| _ -> impos "cg_exp: bad type for arithmetic float operation" in
      emit(T.FPsomeargs (fp_op, T.FPgenop (sl,top_of_stack)));
      pop_free sz
  | (P.Fyl2xF | P.Fyl2xp1F | P.AtanF | P.FremF) ->
      let fp_op = 
	match p with 
	| P.Fyl2xF   -> T.Fyl2x
	| P.Fyl2xp1F -> T.Fyl2xp1
	| P.AtanF    -> T.Fpatan
	| P.FremF    -> T.Fprem1
	| _          -> impos "fp_arithop 2" in
      let (sl,sz) = 
	match lhstyp with
	  P.FloatType -> T.Byte4, 1
	| P.DoubleType -> T.Byte8, 2
	| _ -> impos "cg_exp: bad type for arithmetic float operation" in
      load_fp sl top_of_stack;
      emit(T.FPnoargs fp_op); (* Order on stack may be wrong. *)
      (match fp_op with
	T.Fprem1 -> (* Unlike every other binop this one leaves the second argument on the stack. *)
	  emit(T.FPsomeargs(T.Ffree,T.FPstack 1));
      |	_ -> ());
      pop_free sz      
  | (P.EqF | P.NeqF | P.GtF | P.GteF | P.LtF | P.LteF) ->
      (* The Fcomip instruction sets the following bits in the EFLAGs register
             ZF   PF   CF
	 >   0    0    0
	 <   0    0    1
	 =   1    0    0
	 
	 To check = , check E  (            ZF == 1 )
         To check < , check B  (CF == 1             )
         To check > , check A  (CF == 0 and ZF == 0 )
         To check <=, check BE (CF == 1  or ZF == 1 )
         To check >=, check AE (CF == 0             )
	 *)
      let fp_condop p =
  	match p with
	  P.EqF    -> T.Eq
  	| P.NeqF   -> T.NotEq
  	| P.GtF    -> T.Above
  	| P.GteF   -> T.AboveEq
  	| P.LtF    -> T.Below
  	| P.LteF   -> T.BelowEq
  	| _        -> impos "fp_condop" in
      let sl,sz = 
	match lhstyp with
	  P.FloatType -> T.Byte4, 1
	| P.DoubleType -> T.Byte8, 2
	| _ -> impos "cg_exp: bad type for compare float operation" in
      emit(T.FPsomeargs(T.Fld,T.FPgenop (sl,top_of_stack)));
      emit(T.FPsomeargs(T.Fcomip,T.FPstack2 (true,1)));
      emit(T.FPsomeargs(T.Ffree,T.FPstack 0));
      emit(T.Mov(T.Reg T.Eax,(T.Immed i32_0,[])));
      emit(T.Setcc (fp_condop p,T.Reg T.Eax));
      pop_free sz
  | _ -> impos "cg_exp: Binary Primop"  



(* Figures out the "effects" of an expression or expressions.  Basically,
   an effect is an assignment of some sort or the raising of an exception.
   If an expression doesn't overtly raise an exception then we worry about
   the implicit nullpointer and arraybounds and unionvariant exceptions.
   If we had outofmemory exceptions then we'd worry about these, too. *)

type effects =
    { non_builtin_exn_effects: bool;
      could_raise_null_exn: bool;
      could_raise_bounds_exn: bool;
      could_raise_variant_exn: bool }

let merge_effects efx1 efx2 =
  { non_builtin_exn_effects = 
      efx1.non_builtin_exn_effects || efx2.non_builtin_exn_effects;
    could_raise_null_exn = 
      efx1.could_raise_null_exn || efx2.could_raise_null_exn;
    could_raise_bounds_exn = 
      efx1.could_raise_bounds_exn || efx2.could_raise_bounds_exn; 
    could_raise_variant_exn = 
      efx1.could_raise_variant_exn || efx2.could_raise_variant_exn; }

let rec exp_effects env e =
  let has_null_type e =
    let t = exp2typ e in
    match t with
      P.NamedType (tnr, ts) ->
	(try let si = lookup_struct env !tnr in
	  Popcomptypes.struct_null si
	with Not_found ->
	  (try let ud = lookup_union env !tnr in
	    false (* nullable unions not currently supported *)
	  with Not_found ->
	    (try let ad = lookup_abstype env !tnr in
	      false
	    with Not_found ->
	      (* must be abstract; just assume false since we
	         won't be able to do anything with it anyway *)
	      false)))
    | _ -> false in
  let rec could_be_null e =
    match e.P.raw_exp with
      P.Const c -> (match c with P.Null -> true | _ -> false)
    | P.ConstArray (es, topt) -> false
    | P.Var v -> has_null_type e
    | P.Primop (p,es) -> false
    | P.Conditional (e1,e2,e3) -> (could_be_null e2) || (could_be_null e3)
    | P.AssignOp _ ->
	(* hmm... this is complicated; just punt and say true *)
	true
    | P.FunCall _ -> false
    | P.TypInst (e,ts) -> could_be_null e
    | P.NewStruct _ -> false
    | P.StructMember _ -> has_null_type e
    | P.NewUnion _ -> false
    | P.UnionMember _ -> has_null_type e
    | P.NewTuple _ -> false
    | P.TupleMember _ -> has_null_type e
    | P.NewAbstype _ -> false
    | P.Subscript _ -> has_null_type e
    | P.Codegen _ -> false
    | P.Fill _ -> false
    | P.NewExn _ -> false
    | P.Raise _ -> false
    | P.SeqExp es -> 
	let rec last l =
	  match l with 
	    [] -> failwith "impossible: empty seqExp list"
	  | [x] -> x
	  | (h::t) -> last t in
	could_be_null (last es)
    | P.Nop -> false
    | P.Cast (t,e) -> could_be_null e
    | P.Fun _ -> true
    | P.RepTerm -> false in
  (*-----------------------------------------------------------------*)
  let no_effects = { non_builtin_exn_effects = false;
		     could_raise_null_exn = false;
		     could_raise_bounds_exn = false;
		     could_raise_variant_exn = false; } in
  let has_effects = { non_builtin_exn_effects = true;
		      could_raise_null_exn = false;
		      could_raise_bounds_exn = false;
		      could_raise_variant_exn = false; } in
  match e.P.raw_exp with
    P.Const _ -> no_effects
  | P.ConstArray (es, topt) -> exps_effects env es
  | P.Var _ -> no_effects
  | P.Primop (p,es) -> exps_effects env es
  | P.Conditional (e1,e2,e3) -> exps_effects env [e1;e2;e3]
  | P.AssignOp _ -> has_effects
  | P.FunCall _ -> has_effects (* could check the body and args *)
  | P.TypInst (e,ts) -> exp_effects env e
  | P.NewStruct (tn,tlor,fnoes) ->
      let fexps = List.map (function (fnopt,e) -> e) fnoes in
      exps_effects env fexps
  | P.StructMember (e,fn) -> 
      (* if e could be null, we might raise a nullpointer exception *)
      let efx = exp_effects env e in
      if could_be_null e then
	{ efx with could_raise_null_exn = true }
      else efx
  | P.NewUnion (tn,tlor,fn,eopt) -> 
      (match eopt with
	Some e -> exp_effects env e
      |	None -> no_effects)
  | P.UnionMember (e,fn) ->
      (* could also check for null, but null unions not yet supported *)
      let efx = exp_effects env e in
      { efx with could_raise_variant_exn = true }
  | P.NewTuple es -> exps_effects env es
  | P.TupleMember (e,i) -> exp_effects env e
  | P.NewAbstype (tn,tlor,tlor2,e) -> exp_effects env e
  | P.Subscript (e1,e2) -> 
      (* we could index the array out of bounds *)
      let efx = exps_effects env [e1;e2] in
      { efx with could_raise_bounds_exn = true }
  | P.Codegen _ -> has_effects
  | P.Fill _ -> has_effects
  | P.NewExn _ -> has_effects
  | P.Raise _ -> has_effects
  | P.SeqExp es -> exps_effects env es
  | P.Nop -> no_effects
  | P.Cast (t,e) -> exp_effects env e
  | P.Fun _ -> no_effects
  | P.RepTerm -> no_effects (* allocated in static data *)

and exps_effects env es =
  List.fold_left 
    (fun res e -> 
      let efx = exp_effects env e in
      merge_effects res efx)
    { non_builtin_exn_effects = false;
      could_raise_null_exn = false;
      could_raise_bounds_exn = false;
      could_raise_variant_exn = false; } es

(* gather the effects for these expressions.  If they are otherwise
   effect-free and could only raise one type of builtin exception,
   then let the arguments be evaluated out of order *)
let may_change_exp_order env es =
  let efx = exps_effects env es in
  (not efx.non_builtin_exn_effects) && 
  (not ((efx.could_raise_null_exn && 
	 (efx.could_raise_bounds_exn || efx.could_raise_variant_exn)) ||
	(efx.could_raise_bounds_exn && efx.could_raise_variant_exn)))

(* EAX is "caller"-save. Results are returned in EAX *)
let rec cg_exp_push env e = 
  cg_exp env e; 
  push_any env (exp2typ e)

and cg_funcall env e_whole e typs es = 
  let cons = types2cons typs in
  let fun_typ = (exp2typ e) in (* May need to compress?? *)
  let convention = 
    (match fun_typ with 
      P.FnType (c,_,_,_) -> c | _ -> impos "funcation call to non-function")
  in
  let numargs = (List.length es) in
  let estyps = List.map exp2typ es in
  (* we don't need LEFT to RIGHT evaluation order if
     all of the expressions are effect-free *)
  if may_change_exp_order env (e::es) then
    (let coercion =
      fun_coercion numargs (numargs + (env_s1cons env))
        (if env_in_try env then T.cempty else stack1_c)
        (env_cap1 env)
        (env_cap2 env)
        cons in
    let env' = List.fold_left cg_exp_push env (List.rev es) in
    let args_size = 
      List.fold_left 
	(fun offset etyp -> (P.words_in_typ etyp) + offset)
	0 (List.rev estyps) in
    cg_exp env' e;
    emit(T.Call (T.Reg T.Eax,coercion));
  (* XXX how to incorporate convention here? *)
(*    match convention with
      P.Cdecl -> pop_free (args_size_doubled + return_on_stack)
    |	P.Stdcall -> pop_free (args_size_doubled/2 + return_on_stack)
  end; *)
    pop_free args_size)
  else
    (let numargs = (List.length es) in
      let estyps = List.map exp2typ es in
      let is_known_fun =
	match e.P.raw_exp with
	  P.Var v ->
	    (try (env_local_var_offset env v); false with Not_found -> true)
	| _ -> false
      in
      let env',return_on_stack = 
	if not is_known_fun
        then cg_exp_push env e, 1
        else env, 0 in
      let env' = List.fold_left cg_exp_push env' es in
      let args_size_doubled =
      	List.fold_left 
          (fun offset etyp ->
	    push_mem_to_stack 
	      (T.Prjr((T.Esp,[]),to_i32 (offset * 4),None)) (Typ etyp);
	    2 * P.words_in_typ etyp + offset)
        0 (List.rev estyps) in
      (* the coercion should be in terms of number cons on the stack, not size *)
      let coercion = 
	fun_coercion 
	  numargs (numargs*2 + return_on_stack + (env_s1cons env))
	  (if env_in_try env then T.cempty else stack1_c)
	  (env_cap1 env) (env_cap2 env) cons in
      (if is_known_fun
      then 
        (cg_exp env' e;
	 emit(T.Call (T.Reg T.Eax,coercion)))
      else
        (emit(T.Call (T.Prjr((T.Esp,[]),to_i32(args_size_doubled*4),None),
		      coercion))));
      match convention with
	P.Cdecl -> pop_free (args_size_doubled + return_on_stack)
      |	P.Stdcall -> pop_free (args_size_doubled/2 + return_on_stack));
  (* Dan: We need this with definite assignment because the call may have 
     coerced the handler to a proper supertype which will screw up a merge
     point that has an explicit type.  It is only to appease the verifier.
  *)
  if env_in_try env
  then 
    emit(T.Mov (T.Prjr((T.Ebp,[]),i32_0,None), deSome (env_handler_gop env)))
      
and cg_exp env e = 
  let cg_structmember loc e' n =
    cg_exp env e';
    let t      = exp2typ e'                   in
    let s_info = typ2struct_info env t        in
    let offset = struct_field_offset s_info n in
    emit(T.Coerce(T.Reg T.Eax,[T.Unroll;T.Unroll]));
    if struct_null s_info 
    then check_not_null loc T.Eax 
	(env_set_undefined env e'.P.exp_un_after);
    offset in
  let cg_tuplemember e' i =
    cg_exp env e';
    let rec get_offset j offset typs =
      match typs with
	hd :: tl -> 
	  if j = i then offset 
	  else get_offset (j+1) (offset + P.words_in_typ hd) tl
      | [] -> impos "cg_exp: TupleMember: not enough fields" in
    let typs = 
      match exp2typ e' with
	P.TupleType (_,ts) -> ts
      | t -> 
	  impos ("cg_exp: TupleMember: not tuple type: "^P.typ2string t) in
    let offset = get_offset 1 0 typs in 
    offset in

  match e.P.raw_exp with
    P.Const c ->
	(match c with
	  (* JGM: we need to coerce integers and chars to byte4's explicitly 
	   * so that if they're used during initialization of a heap object, 
	   * we get cbyte4 instead of S(i).  Otherwise, we'd have to coerce 
	   * the struct field to a cbyte4 before forgetting uniqueness. *)
	  P.Int    i -> load_gc (T.Immed i,[T.Subsume T.cbyte4])
	| P.Bool   b -> load_gc (bool b)
	| P.String s -> load_gc (T.Addr (add_string s),[])
	| P.Char   c -> load_gc (T.Immed (to_i32 (Char.code c)),[T.Subsume T.cbyte4])
	| P.Float  f -> load_float (T.Prjl((add_float f,[]),i32_0,None))
	| P.Double d -> load_double (T.Prjl((add_double d,[]),i32_0,None))
	| P.Null     -> load_gc (T.Immed i32_0,[T.RollTosum (exp2con e)]))
  | P.ConstArray (es,ot) -> 
      let typ = 
	match (es,ot) with
	  (_,Some t) -> t 
	| (hd::tl,None) -> exp2typ hd
	| _ -> (impos "cg_exp : ConstArray without type.")       in
      let con       = typ2con typ                                in

      let length    = List.length es                             in
      let length' = to_i32 length                                in 
      let field_size = words_in_con con                          in
      let scale     = 4 * field_size                             in
      let real_size = to_i32 (length * scale)                    in
      let field     = typ2mallocfield typ                        in
      let fields    = Utilities.replicate field length           in
      (* FMS:
	 New array layout is
	 [ size , *array]  ( the ref )
	 [ ....  ]         ( the array )
	 *)
      let mallocarg = T.Mprod fields                              in
      let name     = Id.id_new "m"                                in

      let mallocref = T.Mprod [T.Mbytes T.Byte4;T.Mbytes T.Byte4] in 
      let   nameref = Id.id_new "r"                               in

      let malloc_array () = (* Just malloc the array, not the ref. *)
	emit_malloc name real_size (Some mallocarg)
      in
      let malloc_init_ref () =
	(* Assume initialized array is in ESI *)
	let esi_coerced = (T.Reg T.Esi,
			   [T.Toarray (i32_0,0,T.cfield con T.ReadWrite); 
			     T.Forgetname ])
	in 
	emit_malloc nameref i32_8 (Some mallocref);
	emit (T.Mov (T.Prjr ((T.Eax,[]),i32_0,None),(T.Immed length',[])));
	emit (T.ForgetUnique name);
	emit (T.Mov (T.Prjr ((T.Eax,[]),i32_4,None),esi_coerced));
	(* Initialized but not yet packed ref is in EAX *)
      in
      if no_effect_l es
      then
	(malloc_array ();
	 emit (T.Mov (T.Reg T.Esi,(T.Reg T.Eax,[])));	
	 List.fold_left
	   (fun offset e ->
	     cg_exp env e;
	     store typ (T.Prjr ((T.Esi,[]),i32_4*$offset,None));
	     offset +$ (to_i32 field_size))
	   i32_0 es;
	 malloc_init_ref ();
	 )
      else
	(List.fold_left cg_exp_push env es;
	 malloc_array ();
	 Utilities.app_num 
	   (fun i -> 
	     let offset = to_i32(4*field_size*((length-1)-i)) in
	     emit(T.Pop (T.Prjr((T.Eax,[]),offset,None))))
	   length;
	 emit (T.Mov (T.Reg T.Esi,(T.Reg T.Eax,[]))); 
	 malloc_init_ref ()
	   );
      emit(T.ForgetUnique nameref);
      emit(T.Coerce(T.Reg T.Eax,[T.Pack (T.pcint length',array_con con);
				  T.Forgetname ]))	     
  | P.Var x ->
      (try 
	let offset = env_local_var_offset env x in
	let t = exp2typ e in
	load t (T.Prjr((T.Esp,[]),to_i32(4*offset),None))
      with Not_found -> 
	try 
	  let t   = lookup_global env x in (* lookup its Popcorn type *)
          let is_global_var, n = 
	    match t with
	      P.ExnconType _ -> 
		let loc, c = lookup_exn env x in
		(match loc with
		  ExnconDecl -> false, tid_exn x   (* is TAL exception *)
		| GlobalExncon -> true, tid_val x  (* is exncon glbl var *)
		| _ -> impos "cg_exp : non-glbl excon")
	    | P.FnType (c,_,_,params) ->
		let param_sz = 4 * (P.sumtyps params) in
		(if is_fun_decl env x then
		  false, (tid_fun c param_sz x)
		else
		  true, (tid_fun c param_sz x))
	    | _ -> false, tid_val x                (* is variable *)
	  in 
          let src = 
	    if is_global_var || (needs_indirect t) then 
	      T.Prjl ((n,[]),i32_0,None) 
	    else T.Addr n in
	  load t src
	with Dict.Absent -> impos ("cg_exp: Var without type:" ^ x))
  | P.Primop(P.AddrOf,[e']) ->
      (match e'.P.raw_exp with
	(* XXX tid-val may not work with exncons ... *)
	P.Var v -> 
	  emit (T.Mov (T.Reg T.Eax, (T.Addr (tid_val v), [])))
      | P.StructMember (e',n) ->
	  let offset = cg_structmember e.P.exp_loc e' n in
	  if (offset <> 0) then
	    emit(T.ArithBin(T.Add,T.Reg T.Eax,T.Immed (to_i32 (4*offset))))
      |	P.TupleMember (e',i) ->
	  let offset = cg_tuplemember e' i in
	  if (offset <> 0) then
	    emit(T.ArithBin(T.Add,T.Reg T.Eax,T.Immed (to_i32 (4*offset))))
      |	_ -> impos "& only well-defined on global variables")
  | P.Primop(p,[]) ->
      (match p with
      | (P.PiF | P.Log2_eF | P.Log2_10F | P.Log10_2F | P.Loge_2F) ->
	  let tal_op = 
	    match p with
	    | P.PiF -> T.Fldpi 
	    | P.Log2_eF -> T.Fldl2e
	    | P.Log2_10F -> T.Fldl2t
	    | P.Log10_2F -> T.Fldlg2
	    | P.Loge_2F -> T.Fldln2
	    | _ -> impos "Unexpected operator."
	  in
	  emit (T.FPnoargs tal_op) 
      |	_ -> impos"Bad nullary operator.")
  | P.Primop(p,[e]) ->
      cg_exp env e;
      (match p with
	P.Not ->
	  emit(T.Mov (T.Reg T.Ebx, bool false));
	  emit(T.Cmp ((T.Reg T.Eax,[]), (T.Reg T.Ebx,[])));
	  emit(T.Setcc(T.Eq,T.Reg T.Eax))
      | P.Bitnot -> emit(T.ArithUn (T.Not,T.Reg T.Eax))
      | P.Size -> 
	  let array_size_var = Id.id_new "?sz" in
	  emit (T.Unpack (array_size_var,T.Eax,(T.Reg T.Eax,[])));
	  emit (T.Mov (T.Reg T.Eax,(T.Prjr ((T.Eax,[]),i32_0,None),[])));
	  emit (T.Coerce (T.Reg T.Eax,[T.Subsume T.cbyte4]))
      | P.Ord -> () (* do nothing *)
      | P.Chr -> () (* do nothing *)
      |	P.CosF | P.SinF | P.TanF | P.SqrtF | P.F2xm1F | P.FabsF | P.FroundF->
	  let tal_op = 
	    match p with
	      P.CosF -> T.Fcos | P.SinF -> T.Fsin | P.TanF -> T.Fptan
	    | P.SqrtF -> T.Fsqrt
	    | P.F2xm1F -> T.F2xm1 
	    | P.FabsF -> T.Fabs
	    | P.FroundF -> T.Frndint
	    | _ -> impos "Unexpected floating point operator."
	  in
	  emit (T.FPnoargs tal_op);
	    (* This is only correct if the argument is in range. TAL isn't
	       getting this right either I bet. *)
	  if (tal_op = T.Fptan) then 
	    (emit (T.FPsomeargs (T.Ffree, T.FPstack 0));
	     emit (T.FPnoargs T.Fincstp));
	  ()
      | _ -> impos "cg_exp: Unary op expected.")
  | P.Primop(p,[e1;e2]) ->
      let env' = cg_exp_push env e1 in
      cg_exp env' e2;
      cg_bop (outermost env') p (exp2typ e1)
  | P.Primop _ -> impos "cg_exp: ?? Primop"
  | P.Conditional (e1,e2,e3) ->
      let true_lab  = Id.id_new "condtrue"  in
      let false_lab = Id.id_new "condfalse" in
      let end_lab   = Id.id_new "condend"   in
      cg_bool_exp env e1 (true_lab,[]) (false_lab,[]) true;
      emit_lab_nocon (env_set_undefined env e1.P.exp_un_after) true_lab;
      let env = env_set_undefined env e1.P.exp_un_after in
      cg_exp env e2;
      emit_jmp env end_lab [];
      emit_lab_nocon (env_set_undefined env e1.P.exp_un_after) false_lab;
      cg_exp env e3;
      emit_lab_nocon 
	(env_set_undefined env 
	   (P.Varset.union e2.P.exp_un_after e3.P.exp_un_after))
	end_lab
  | P.AssignOp (e1,po,e2) ->
      cg_lhs_exp env e1 e2 po
  | P.FunCall (e',ts,es) ->
      let ts       = deSome !ts in
      cg_funcall env e e' ts es;
      (if !P.stack_traces then cstk_reset env)
  | P.TypInst({ P.raw_exp = P.RepTerm },[t]) ->
      let id_rep = cg_native_rep t in
      emit (T.Mov (T.Reg T.Eax, (T.Addr id_rep, [])))
  | P.RepTerm ->
      impos "cg_exp: repterm expr mal-formed"
  | P.TypInst(e,ts) ->
      cg_exp env e;
      if ts=[] then ()
      else 
	begin
	  let cs = types2cons ts in
	  let coercions = List.rev (List.map (fun c -> T.Tapp (T.Con c)) cs) in
	  emit(T.Coerce (T.Reg T.Eax,coercions))
	end
  | P.NewStruct(n,ts,es) -> 
      let cs         = List.map typ2con (deSome(!ts))    in
      let s_info     = lookup_struct env n               in
      let name       = Id.id_new "mptr"                  in
      let offsets = struct_field_offsets s_info          in
      let cons    = struct_field_cons s_info             in
      let size = List.fold_left (fun sum c -> sum + words_in_con c) 0 cons in

      (* All fields either contain 
       * Some field_name (meaning that we are assigning to that field of the 
       *   struct) or
       * None (meaning that assignment to the fields is done in the order the 
       *   expressions appear) 
       *)
      let real_offset off fo =
	match fo with
	  None   -> off
	| Some f -> struct_field_offset s_info f
      in

      if no_effect_l (List.map snd es)
      then
	(emit_malloc name (to_i32 (4*size)) None;
	 emit(T.Mov (T.Reg T.Esi,(T.Reg T.Eax,[])));
	 List.iter2
	   (fun (field_name_opt,e) offset ->
	     cg_exp env e;
	     let off = to_i32 (4 * real_offset offset field_name_opt) in
	     store (exp2typ e) (T.Prjr((T.Esi,[]),off,None)))
	   es offsets;
	 emit(T.Mov (T.Reg T.Eax,(T.Reg T.Esi,[]))))
      else
	(List.fold_left cg_exp_push env (List.map snd es);
	 emit_malloc name (to_i32 (4*size)) None;
	 List.iter2 
	   (fun (field_name_opt,e) offset ->
	     let off = to_i32 (4 * real_offset offset field_name_opt) in
	     let dest = T.Prjr((T.Eax,[]),off,None) in
	     pop_stack_to_mem (exp2typ e) dest)
	   (List.rev es) (List.rev offsets);
	 ());
      emit(T.ForgetUnique name);
      emit(T.Coerce(T.Reg T.Eax,(roll_struct s_info n cs)@[T.Forgetname]))

  | P.StructMember (e',n) ->
      let offset = cg_structmember e.P.exp_loc e' n in
      load (exp2typ e) (T.Prjr ((T.Eax,[]),to_i32 (4*offset),None))
	
  | P.NewUnion (nt,ts,f,eo) ->
      let u_info = typ2union_info env (exp2typ e) in
      let cs     = types2cons     (deSome !ts)    in
      (try match eo with
	None    -> (* void_case *)
	  let tag = union_void_tag_assoc u_info f in
	  emit(T.Mov (T.Reg T.Eax, 
		      (T.Immed tag,[T.RollTosum (name_con nt cs)])))
      | Some e' -> (* value_case *)
	  let tag,con   = union_val_tag_assoc u_info f     in
	  let tag_con   = T.csing (T.pcint tag)            in
	  let name      = Id.id_new "mptr"                 in
	  let typ       = exp2typ e'                       in
	  let size      = to_i32 (4 * P.words_in_typ typ)  in
	  if no_effect e' then 
	    (emit_malloc name (i32_4 +$ size) None;
	     emit (T.Mov(T.Reg T.Esi,(T.Reg T.Eax,[])));
	     emit (T.Mov(T.Prjr ((T.Esi,[]),i32_0,None),(T.Immed tag,[])));
	     cg_exp env e';
	     store typ (T.Prjr ((T.Esi,[]),i32_4,None));
	     emit (T.ForgetUnique name);
	     emit (T.Mov(T.Reg T.Eax,(T.Reg T.Esi,
				      [T.RollTosum (name_con nt cs);
					T.Forgetname]))))
	  else
	    (cg_exp_push env e';
	     emit_malloc name (i32_4 +$ size) None;
	     pop_stack_to_mem typ (T.Prjr ((T.Eax,[]),i32_4,None));
	     emit (T.Mov(T.Prjr ((T.Eax,[]),i32_0,None),(T.Immed tag,[])));
	     emit (T.ForgetUnique name);
	     emit (T.Coerce (T.Reg T.Eax,[T.RollTosum (name_con nt cs);
					   T.Forgetname])))
      with Not_found -> impos "cg_exp: NewUnion: No such tag.")
  | P.UnionMember (e',n) ->
	(* generate code for e' *)
	cg_exp env e';
	let t      = exp2typ e'                   in
	let u_info = typ2union_info env t         in
	let tag    = fst(union_val_tag_assoc u_info n) in
	let name   = Id.id_new "uptr" in
	emit(T.Coerce(T.Reg T.Eax,[T.Unroll]));
	let env = env_set_undefined env e'.P.exp_un_after in
	(if ((union_num_voids u_info) > 0)
	then (emit_null_check name
		(T.Eax, T.min_pointer_integer,[],T.Below) env);
	 let name = Id.id_new "uptr" in
	 emit_btagvar name (T.Eax,i32_0,tag,(union_variant_label,
					     builtin_coercion env),
			    T.NotEq) env);
	(* load value out of sum *)
	load (exp2typ e) (T.Prjr((T.Eax,[T.Fromsum]),i32_4,None))
  | P.NewTuple es ->
      let num_fields   = List.length es                in
      let typs         = List.map exp2typ es           in
      let size,offsets = 
	List.fold_left 
	  (fun (total,offsets) typ -> 
	    (total + P.words_in_typ typ, total::offsets))
	  (0, []) typs in
      let offsets = List.rev offsets in
	    
      let margs        = List.map exp2con es           in
      let c = T.cprod_b 
	  (List.map (fun c -> T.cfield c T.ReadWrite) margs) in
      let name = Id.id_new "mptr" in

      if no_effect_l es
      then
	begin
	  emit_malloc name (to_i32 (4*size)) None;
	  emit(T.Mov (T.Reg T.Esi,(T.Reg T.Eax,[])));
	  List.iter2
	    (fun e offset ->
	      cg_exp env e;
	      store (exp2typ e) 
	       	(T.Prjr ((T.Esi,[]),i32_4 *$ (to_i32 offset),None)))
	    es offsets;
	  emit(T.Mov (T.Reg T.Eax,(T.Reg T.Esi,[])))
	end
      else
	begin
	  List.fold_left cg_exp_push env es;
	  emit_malloc name (to_i32 (4*size)) None;
	  List.iter2
	    (fun e offset -> 
	      let off = to_i32 (4*offset) in
	      pop_stack_to_mem (exp2typ e) (T.Prjr((T.Eax,[]),off,None)))
	    (List.rev es) (List.rev offsets)
	end;
      emit(T.ForgetUnique name);
      emit(T.Coerce(T.Reg T.Eax,[T.Forgetname]))

  | P.TupleMember(e',i) ->
      let offset = cg_tuplemember e' i in
      load (exp2typ e) (T.Prjr((T.Eax,[]),to_i32 (4*offset),None))

  | P.NewAbstype(n,all_ts_ref,exist_ts_ref,e) ->
      let abs_info = lookup_abstype env n in
      let all_ts = deSome (!all_ts_ref) in
      let exist_ts = deSome (!exist_ts_ref) in
      let pack_coercions = abs_pack_coercions abs_info all_ts exist_ts in
      cg_exp env e;
      emit(T.Coerce (T.Reg T.Eax,pack_coercions))

  | P.Subscript (e1,e2) ->
      let env' = env in
      let env = cg_exp_push env e1 in
      cg_exp env e2;
      pop T.Ebx;      
      (* array in EBX, index in EAX *)
      emit_array_check env' T.Ebx T.Eax;

      (* do the projection *)
      (match exp2typ e1 with
	P.StringType ->
	  (* move index and zero out EAX to avoid spurious high bits *)
	  emit(T.Mov(T.Reg T.Ecx,(T.Reg T.Eax,[])));
	  emit(T.Mov(T.Reg T.Eax,(T.Immed i32_0,[])));
	  emit(T.Movpart(true, T.Reg T.Eax, T.RPl, 
			 T.Prjr((T.Ebx,[]),i32_0,Some(T.Byte1,T.Ecx)), T.RPl));
      |	P.ArrayType (typ,_) ->
	  let size = 
	    if is_word typ then T.Byte4 else (* assume double *) T.Byte8 in
	  load typ (T.Prjr((T.Ebx,[]),i32_0,Some(size,T.Eax)))
      | _ -> impos "cg_exp: Subscript: not array or string")
		     
  | P.NewExn (x,eopt) -> 
      let exn_id  = tid_exn x in	
      let (loc,con) = lookup_exn env x in
      let con' = T.cfield con T.Read in
      let name = Id.id_new "mptr" in

      let size,typ = 
	(match eopt with 
	  None    -> (* exception has no argument *)
	    emit (T.Mov (T.Reg T.Eax,(T.Immed i32_0,[])));
	    emit (T.Push (T.Reg T.Eax,[]));
	    i32_4, P.IntType(true,P.B4)
	| Some e' -> 
	    cg_exp env e';
	    let typ = exp2typ e' in
	    let size = to_i32 (P.words_in_typ typ * 4) in
	    emit(T.ArithBin (T.Sub,T.Reg T.Esp,T.Immed size));
	    store typ top_of_stack;
	    size, typ) in
      emit_malloc name (i32_8 +$ size) None;
      pop_stack_to_mem typ (T.Prjr ((T.Eax,[]),i32_8,None));

      let loc_info = add_location e.P.exp_loc in
      load_ecx (T.Addr loc_info);
      emit (T.Mov (prjr T.Eax i32_4, (T.Reg T.Ecx,[])));

      let src =
	(match loc with
	  LocalExncon ->
	    (try 
	      let offset = env_local_var_offset env x in
	      T.Prjr((T.Esp,[]),to_i32(4*offset),None)
	    with Not_found -> 
	      impos "cg_exp: exnname not in local env")
	| ExnconDecl ->
	    (T.Addr exn_id)
	| GlobalExncon -> 
	    T.Prjl ((tid_val x,[]),i32_0, None))
      in

      emit(T.Mov (T.Reg T.Ebx,(src,[])));
      emit(T.Mov (T.Prjr ((T.Eax,[]),i32_0,None),(T.Reg T.Ebx,[])));
      emit(T.ForgetUnique name);
      emit(T.Coerce (T.Reg T.Eax,[T.Pack (con',exn_con);T.Forgetname]))
	(* JGM:  probably need more coercions here... *)
    
  | P.Raise e ->
      cg_exp env e;
      (if !P.stack_traces then cstk_raise ());
      emit (T.Mov (T.Reg T.Esp,(T.Reg T.Ebp,[])));
      emit (T.Pop (T.Reg T.Ebx));
      emit (T.Jmp (T.Reg T.Ebx,[]))
  | P.SeqExp es -> List.iter (cg_exp env) es
  | P.Nop -> () 
  | P.Cast (t,e) -> 
      let te = deSome e.P.exp_typ in
      cg_exp env e;
      cg_cast te t
  | P.Fun _ -> impos "inner functions should've been eliminated!"
(* Cyclone *)
    | P.Codegen fd ->
        let env = flush_vis_fenv env in
        let fn_con = abbrev_con(
          typ2con (P.FnType(fd.P.fn_convention,
			    fd.P.fn_tyvars,
                            fd.P.fn_ret_type,
			    List.map snd fd.P.fn_args))) in
	let cg_con = T.ctrgn(fn_con,Some fn_con,[]) in
	let env = cyc_push_cg env cg_con in
	let cg_id = cyc_get_cg_id env in
	let env_tmpl,env_cg = env_codegen_body env fd in
	let pre_con = abbrev_con(mk_label_con env_tmpl) in
        emit(T.CgStart (cg_id,fn_con));
	emit(T.Push (T.Reg T.Eax,[])); (* Push the region on the stack. *)
        gen_code_mark(M_TemplateBeg(Id.id_new "tmpl_start",
                                    pre_con,
                                    get_vis_fenv env));
        cg_fundecl env_tmpl fd;
	(* We need to make sure the last emitted template, 
	   either ends in either an exit, a return, or a raise.
	   Putting a redundant exit doesn't hurt much and is very simple! *)
        jmp_pop_exit ();
        cg_fill_holes env_cg None true;
	pop T.Eax;
        emit(T.CgEnd T.Eax)
    | P.Fill e -> 
	if not (in_frame env)
	then impos "fill can only be used within a codegen or splice";

	let temp_id =
          let rec aux marks depth =
            match marks with
              [] -> impos "can't find template id"
            | M_TemplateBeg(temp_id,_,_)::tl ->
                if depth=0 then temp_id
                else aux tl (depth-1)
            | (M_TemplateEnd _)::tl -> aux tl (depth+1)
            | _::tl -> aux tl depth
          in aux gen_state.code_marks 0 in
        let hole_id = Id.id_new "hole" in
        emit(T.CgHole(T.Eax, temp_id, hole_id));
        gen_code_mark(M_Fill(hole_id,e))
(* End Cyclone *)
(* Changed for left to right evaluation order.
   Now neither expression is evaluated before calling cg_lhs_exp.
   Must obey evaluation order and only evaluate lhs once.
   Must leave EAX with correct value for cascaded assignments.
   We assume cg_bop and cg_cast don't use ESI or EDI
 *)
and cg_lhs_exp env e1 e2 op_opt =(* : env -> exp -> exp -> P.op option -> unit*)
  let raw_exp,arg_cast,result_cast,typlhs_before_cast =
    match e1.P.raw_exp with
      P.Cast (t,e') ->
	let te' = deSome e'.P.exp_typ in
	(e'.P.raw_exp,
	 (fun () -> cg_cast te' t),
	 (fun () -> cg_cast t te'),
	 Some te')
    | rx -> let none () = () in (rx,none,none,None)
  in
  let typ = exp2typ e1 in
  let lhs_typ = match typlhs_before_cast with
    None -> typ | Some t -> t in
  let do_memory_update gop do_upd =
    (* pre-conditions: rhs in EAX, do_upd should expect result in EAX *)
    (* floats: rhs on fp stack, do_upd should expect result on fp stack *)
    match op_opt with
	None -> do_upd()
      |	Some op ->
	  (* inefficient shuffling when there are casts but who cares *)
	  (match typlhs_before_cast with
	    None ->
	      (push_mem_to_stack gop (Typ typ);
	       cg_bop (outermost env) op typ;
	       do_upd())
	  | Some typlhs_before_cast ->
	      (if not (is_float typ) then
	      	emit(T.Mov(T.Reg T.Ebx,(T.Reg T.Eax,[])));
	       load typlhs_before_cast gop;
	       arg_cast();
	       push_std typ;
	       if not (is_float typ) then
	       	 emit(T.Mov(T.Reg T.Eax,(T.Reg T.Ebx,[])));
	       cg_bop (outermost env) op typ;
	       result_cast();
	       do_upd()))
  in
  match raw_exp with
    P.Var x -> 
      (try let gop = 
	(try 
	  (* If deeper than a handler, use EBP to appease the verifier *)
	  let offset = env_local_var_offset env x in
	  if env_in_try env && offset > (env_s1len env)
	  then T.Prjr ((T.Ebp,[]),to_i32 (4*(offset - (env_s1len env))),None)
	  else T.Prjr ((T.Esp,[]),to_i32 (4*offset),                    None)
	with Not_found -> 
	  try 
            let t = lookup_global env x in
            let is_global_var, n = 
	      match t with
		P.ExnconType _ -> 
		  let loc, c = lookup_exn env x in
		  (match loc with
		    ExnconDecl -> false, tid_exn x   (* is TAL exception *)
		  | GlobalExncon -> true, tid_val x  (* is exncon glbl var *)
		  | _ -> impos "cg_exp : non-glbl excon")
	      | P.FnType _ ->
		  (if is_fun_decl env x then
		    false, tid_val x
		  else
		    true, tid_val x)
	      | _ -> false, tid_val x                (* is variable *)
	    in 
            if is_global_var || (needs_indirect t) then
              T.Prjl ((n,[]),i32_0,None)
            else 
	      T.Addr n
	  with Dict.Absent -> impos "cg_exp: Var without type.")
      in
      begin match op_opt with
	None -> 
	  cg_exp env e2;
	  store typ gop
      | Some op ->
	  cg_exp env e1;
	  arg_cast ();
	  let env = push_any env typ in
	  cg_exp env e2;
	  cg_bop (outermost env) op typ;
	  result_cast ();
	  (* Type of lhs and is type before cast.  Cast was inserted for
	     conversion only. *)
	  store lhs_typ gop
      end
      with Break -> ())

  | P.StructMember (e,f) ->
      let struct_info = typ2struct_info env (exp2typ e) in
      let gop =
	let offset = struct_field_offset struct_info f in
	T.Prjr ((T.Esi,[]),to_i32 (4*offset),None) 
      in
      let env' = env in
      let env = cg_exp_push env e in
      let env = env_set_undefined env e.P.exp_un_after in
      cg_exp env e2;
      pop T.Esi;
      emit(T.Coerce(T.Reg T.Esi,[T.Unroll;T.Unroll]));
      if struct_null struct_info then check_not_null e.P.exp_loc T.Esi env';
      (* Address of null-checked struct in ESI, rhs in EAX *)
      (* if rhs is float, then rhs on fpstack *)
      do_memory_update gop (fun () -> store lhs_typ gop)

  | P.Subscript (e_arr,e_sub) ->
      let scale = 
	match exp2typ e_arr with
	  P.StringType -> T.Byte1
	| P.ArrayType (P.DoubleType,_) -> T.Byte8
	| _ -> T.Byte4
      in
      let typ            = exp2typ e2      in
      let env' = env in 
      let env = cg_exp_push env e_arr in
      let env = cg_exp_push env e_sub in
      cg_exp env e2;
      pop T.Edi;
      pop T.Esi;
      (* we need the following push only because the array_bounds_error
       * label isn't sufficiently polymorphic. If we add a "Top" supertype 
       * for the fp stack, then we can take out this push. *)
      (* FMS: push and matching pop below may be needed for float support,
	 but I don't understand why so I just removed them. *)
      (* push_std typ;  *)
      (* array address in ESI, index in EDI, rhs in EAX *)
      (* do the bounds-check *)
      emit_array_check env' T.Esi T.Edi;

      (* do the update *)
      (* pop_std typ; *)
      let gop = T.Prjr((T.Esi,[]),i32_0,Some(scale,T.Edi)) in
      do_memory_update 
	gop
        (fun () -> 
	  match scale with
	    T.Byte1 -> emit(T.Movpart(true,gop,T.RPl, T.Reg T.Eax, T.RPl))
	  | T.Byte4 | T.Byte8 -> store lhs_typ gop
	  | _       -> failwith "Popcompile: unsupported array scale")

  | P.TupleMember (e,i) ->
      let fieldtyps = 
	match exp2typ e with
	  P.TupleType (P.ReadWrite,ts) -> ts
	| P.TupleType _ -> 
	    impos "cg_lhs_exp: TupleMember: read-only tuple type"
	| _ -> impos "cg_lhs_exp: TupleMember: not tuple type" in
      let offset,_ = 
	List.fold_left
	  (fun (offset,j) typ -> 
	    if j = i then (offset,j) else (offset + P.words_in_typ typ, j + 1))
	  (0,1) fieldtyps in
      
      let gop = (T.Prjr ((T.Esi,[]),to_i32 (4*offset),None)) in
      let env = cg_exp_push env e in
      let env = env_set_undefined env e.P.exp_un_after in
      cg_exp env e2;
      pop T.Esi;
      (* Address of Tuple in ESI, rhs in EAX *)
      (* If field is float then rhs in fps *)
      do_memory_update gop (fun () -> store lhs_typ gop)

  | _ -> impos "cg_lhs_exp: Not allowed by type-checker."

(* generates code for a boolean expression that's part of a test --
 * avoids generating the boolean value and instead branches to the
 * appropriate label based on the test.  Special support for && and ||
 * is baked in to generate (surprisingly) decent code.
 *
 * true_l is the label to branch to if the expression evaluates to true,
 * false_l is the label to branch to if the expression evaluates to false.
 * true_is_fallthru = true when the true_l is the next block, whereas
 * true_is_fallthru = false when the false_l is the next block. 
 *)
and cg_bool_exp env e true_l false_l true_is_fallthru = 
  match e.P.raw_exp with
    P.Const (P.Bool b) ->
      if b & (not true_is_fallthru) then emit_jmp env (fst true_l) (snd true_l)
      else if (not b) & true_is_fallthru then 
	emit_jmp env (fst false_l) (snd false_l)
      else ()
  | P.Primop(P.Not,[e]) -> 
      cg_bool_exp env e false_l true_l (not true_is_fallthru)
  | P.Primop(((P.Gt | P.Lt | P.Gte | P.Lte | P.Eq | P.Neq) as p),[e1;e2]) ->
      let env' = cg_exp_push env e1 in
      cg_exp env' e2;
      pop T.Ebx;
      emit(T.Cmp ((T.Reg T.Ebx,[]),(T.Reg T.Eax,[])));
      if true_is_fallthru then
        let (lab,coerce) = false_l in
	emit_jcc env (T.negate_condition(cond_op p)) lab coerce
      else 
        let (lab,coerce) = true_l in
	emit_jcc env (cond_op p) lab coerce
  (* special case for e1 && e2 *)
  | P.Conditional(e1,e2,{P.raw_exp = P.Const(P.Bool false)}) ->
      let true_l2 = Id.id_new "condtrue" in
      cg_bool_exp env e1 (true_l2,[]) false_l true;
      let env = env_set_undefined env e1.P.exp_un_after in
      emit_lab_nocon env true_l2;
      cg_bool_exp env e2 true_l false_l true_is_fallthru;
  (* special case for e1 || e2 *)
  | P.Conditional(e1,{P.raw_exp = P.Const(P.Bool true)},e2) ->
      let false_l2 = Id.id_new "condfalse" in
      cg_bool_exp env e1 true_l (false_l2,[]) false;
      let env = env_set_undefined env e1.P.exp_un_after in
      emit_lab_nocon env false_l2;
      cg_bool_exp env e2 true_l false_l true_is_fallthru;
  | P.Conditional(e1,e2,e3) ->
      let true_l2  = Id.id_new "condtrue"  in
      let false_l2 = Id.id_new "condfalse" in
      let end_l    = Id.id_new "condend"   in
      cg_bool_exp env e1 (true_l2,[]) (false_l2,[]) true;
      let env = env_set_undefined env e1.P.exp_un_after in
      emit_lab_nocon env true_l2;
      cg_bool_exp env e2 true_l false_l true;
      emit_jmp env end_l [];
      emit_lab_nocon env false_l2;
      cg_bool_exp env e3 true_l false_l true;
      emit_lab_nocon 
	(env_set_undefined env 
	   (P.Varset.union e2.P.exp_un_after e3.P.exp_un_after))
	end_l
  | _ ->
      cg_exp env e;
      let env = env_set_undefined env e.P.exp_un_after in
      let (false_lab,false_coercion) = false_l in
      let ( true_lab, true_coercion) =  true_l in
      if true_is_fallthru then
	(emit (T.Cmp((T.Reg T.Eax,[]),(T.Immed i32_0,[])));
	 emit_jcc env T.Eq false_lab false_coercion)
      else 
	(emit (T.Cmp((T.Reg T.Eax,[]),(T.Immed i32_1,[])));
	 emit_jcc env T.Eq true_lab true_coercion)
 
(* Cyclone *)
(****************************************************************)
(* cg_fill_holes is called at the end of a template to generate *)
(* code to dump the template and fill its holes.                *)
(****************************************************************)
and cg_fill_holes cenv0 post_con_opt restore_stack =
(*
  (Printf.eprintf "Entering cg_fill_holes\n"; flush stderr);
*)
    (* Scan back in the TAL code that has been emitted so far,
    ** until we reach the beginning of the template.  Gather
    ** the labels and holes of the template, as well as 
    ** any jumps in the template.  Also gather the environment
    ** in effect just before the template code was emitted. *)
  let rec gather marks labels holes jumps depth =
    match marks with
      [] -> impos "gather"
    | M_Fill(hole_id,e)::tl ->
        if depth=0 then gather tl labels ((hole_id,e)::holes) jumps depth
        else gather tl labels holes jumps depth
    | M_Label(i,Some c)::tl -> 
        if depth=0
        then gather tl ((i,abbrev_con c)::labels) holes jumps depth
        else gather tl labels holes jumps depth
    | M_Label(i,None)::tl -> gather tl labels holes jumps depth
    | (M_Jmp _ as m)::tl ->
        if depth=0 then gather tl labels holes (m::jumps) depth
        else gather tl labels holes jumps depth
    | (M_Jcc _ as m)::tl ->
        if depth=0 then gather tl labels holes (m::jumps) depth
        else gather tl labels holes jumps depth
    | M_TemplateBeg(tmpl_id,pre_con,fenv)::tl ->
        if depth=0 then (tmpl_id,pre_con,labels,holes,jumps,fenv)
        else gather tl labels holes jumps (depth-1)
    | (M_TemplateEnd _)::tl -> gather tl labels holes jumps (depth+1)
    | (M_ISeq _)::tl -> gather tl labels holes jumps depth
    | (M_TypeAbbrev _)::tl -> gather tl labels holes jumps depth
  in
  let (this_tmpl,pre_con,labels,holes,jumps,fenv) =
    gather (gen_state.code_marks) [] [] [] 0 in
  List.iter (fun (l,_) -> Hashtbl.add tmpl_of_label l this_tmpl) labels;
    (* Type variable for the template copy *)
  let tmpl_tv = Id.id_new "tv" in
    (* Find out which jump targets are not defined in the template;
     ** these are inter-template jumps that will have to be patched. *)
  let jumps = 
    List.concat 
      (List.map
	 (fun m ->
	   match m with
	     M_Jmp(t,x,c,coercion) ->
	       if List.exists (fun (i,_) -> t=i) labels then []
               else
                 let hole = Id.id_new "jump_hole" in
                 x := Some(this_tmpl,hole);
                 [(t,c,this_tmpl,hole,
                   fun (a,b,c,d,e,f,g) -> T.CgFillJmp(a,b,c,d,e,f,g))]
           | M_Jcc(cc,t,x,c,coercion) ->
               if List.exists (fun (i,_) -> t=i) labels then []
               else
                 let hole = Id.id_new "jcc_hole" in
                 x := Some(this_tmpl,hole);
                 [(t,c,this_tmpl,hole,
                   fun (a,b,c,d,e,f,g) -> T.CgFillJcc(a,b,c,d,e,f,g))]
           | _ -> impos "cg_fill_holes: mark is not a jump")
         jumps) in
    (* Get a list of all the holes and their cons *)
    let holes2 =
      (List.map (fun (v,e) -> (v,abbrev_con(exp2con e))) holes)
        @ (List.map (fun (_,c,_,h,_) -> (h,c)) jumps) in

    (* Mark the end of the template *)
    let template_con =
      abbrev_con(dc(T.Ctmpl(pre_con,post_con_opt,labels,holes2))) in
    gen_code_mark(M_TemplateEnd template_con);
    (* Restore the stack and cg type from before the last template *)
    let cenv = put_vis_fenv cenv0 fenv in 
    (* Put the cg_region in Ecx *)
    let (cg_offset,_) = cyc_get_cg cenv in
    peek T.Ecx cg_offset;
    (* Dump the template *)
    emit(T.CgDump(T.Ecx, tmpl_tv, T.Ebx, this_tmpl)); 
    let cenv = cg_push cenv tmpl_tv labels holes2 post_con_opt in
    (* Push the pointer to the template copy *)
    emit(T.Push(T.Reg T.Ebx,[]));
    let cenv =
      env_add_local_var cenv (Id.id_to_string this_tmpl) (dc(T.Ctptr tmpl_tv))in
    (* Fill the move holes *)
    let (cg_offset,_) = cyc_get_cg cenv in
    let cenv = 
      List.fold_left
        (fun cenv (hole,e) ->
          cg_exp cenv e; (* Value to fill hole in goes in EAX *)
          peek T.Ebx 0;  (* Get the pointer to the temp. from top of stack *)
	  peek T.Ecx cg_offset; (* Load the cg_region *)
          emit (T.CgFill(T.Ecx, T.Ebx, this_tmpl, hole, T.Eax));
          cg_pop_hole cenv hole)
        cenv holes in

    (*** INVARIANT: T.EBX HOLDS THE POINTER TO THE TEMPLATE COPY ***)
    (***            T.ECX                          CODE GENERATION REGION ***)

    (* Patch forward jumps to this template *)
    let cenv =           
      List.fold_left                (* for every label in this template *)
        (fun cenv (label,_) ->
          List.fold_left            (* for every forward itj to the label *)
            (fun cenv (back_tmpl,back_hole,fill_inst) ->
              let i = try 
		env_local_var_offset cenv (Id.id_to_string back_tmpl)
	      with Not_found -> impos "cyclone: unexpected." in
                (* Can this fail? *)
                (* List.assoc (Id.id_to_string back_tmpl) cenv.local_env in *)
              peek T.Eax i;
              emit(fill_inst(T.Ecx,T.Eax,back_tmpl,back_hole,T.Ebx,this_tmpl,label));
              cg_pop_hole cenv back_hole)
            cenv
            (Hashtbl.find_all forward_itjs label))
        cenv labels in

    (*** STILL INVARIANT: T.EBX HOLDS THE POINTER TO THE TEMPLATE COPY ***)
    (*** DITTO FOR T.ECX !                                             ***)

    (* Patch backward jumps from this template, mark fwd jumps as such *)
    let cenv =  
      List.fold_left
        (fun cenv (t,_,_,hole,fill_inst) ->
          try 
            let back_tmpl = Hashtbl.find tmpl_of_label t in
            let i = env_local_var_offset cenv (Id.id_to_string back_tmpl) in
              (* Can this fail? *)
              (* List.assoc (Id.id_to_string back_tmpl) cenv.local_env in *)
            peek T.Eax i;
            emit(fill_inst(T.Ecx,T.Ebx,this_tmpl,hole,T.Eax,back_tmpl,t));
            cg_pop_hole cenv hole
          with Not_found ->
            (* Not a backward jump, must be a forward jump *)
            Hashtbl.add forward_itjs t (this_tmpl,hole,fill_inst);
            cenv)
        cenv jumps in
    
    (* Finally, restore the stack if asked *)
    if restore_stack then begin
      let n = env_stack_height cenv - env_stack_height cenv0 in
      pop_free n;
      cg_pop_free cenv n
    end;

    (* The returned cenv is not correct if restore_stack=true, because
    ** it doesn't take the pop_free into account.  But it is only used
    ** if restore_stack=false. *)
    cenv
(* End Cyclone *)

and cg_stmt env {P.raw_stmt=s;P.stmt_loc=loc;
		 P.un_before=un_before; P.un_after=un_after; } = 
  (* evaluate to true iff stmt must return *)
  let env = env_set_undefined env un_before in
  let cg_control_flow env n =
    let local_depth = env_local_depth  env   in
    let ebp_opt     = env_next_handler env n in
    (match ebp_opt with 
      None        -> ()
    | Some offset -> peek T.Ebp (local_depth - offset));
    pop_free (local_depth - n) in
  let cg_goto env (label,n) coercion = 
    cg_control_flow env n; emit_jmp env label coercion 
  in
  match s with
    P.Skip  -> false
  | P.Exp e -> 
      cg_exp env e; 
      delete_fp_reg (exp2typ e);
      (match e.P.raw_exp with
	P.Raise _ -> true
      | _         -> false)
  | P.Seq (s1,s2) -> cg_stmt env s1 or cg_stmt env s2
  | P.Return eopt ->
      (match eopt with
	None   -> ()
      | Some e -> cg_exp env e; ());
      cg_control_flow env (env_args_on_stk env); (* pop local vars *)
      (match env_convention env with
      |	P.Cdecl   -> emit (T.Retn None)
      |	P.Stdcall -> 
	  let p = 4 * ((env_args_on_stk env) - 1)in
	  emit (T.Retn (Some (int_to_int32 p))));
      true
  | P.IfThenElse(e,s1,s2) ->
      let true_lab  = Id.id_new "iftrue"  in
      let false_lab = Id.id_new "iffalse" in
      let end_lab   = Id.id_new "ifend"   in
      let env_t = env_set_undefined env s1.P.un_before in
      let env_f = env_set_undefined env s2.P.un_before in
      let env_m = env_set_undefined env un_after       in
      cg_bool_exp env e (true_lab,[]) (false_lab,[]) true;
      emit_lab_nocon env_t true_lab;
      let s1_returns = cg_stmt env_t s1 in
      if not s1_returns then emit_jmp env_m end_lab [];
      emit_lab_nocon env_f false_lab;
      (* FMS: To keep TAL type-checking fast we should really occasionally put
	 in some cons.  It isn't easy though.  But the following should work. *)
      fallthru(env_f);
      emit_lab env_f (Id.id_new "tc_fast");
      let s2_returns = cg_stmt env_f s2 in
      if not (s1_returns & s2_returns)
      then (emit_lab_nocon env_m end_lab; false)
      else true
  | P.While(e,s) ->
      (* This may be wrong when we change the definite assignment rules for
	 loops *)
      let loopbody = Id.id_new "whilebody" in
      let looptest = Id.id_new "whiletest" in
      let loopend  = Id.id_new "whileend"  in
      let env' = env_set_loop_labels env loopend looptest in
      emit_jmp env  looptest [];
      emit_lab (env_set_undefined env' s.P.un_before) loopbody;
      cg_stmt  env' s;
      emit_lab_nocon env' looptest;
      cg_bool_exp env' e 
	(loopbody,branch_coercion env') (loopend,[]) false;
      emit_lab_nocon (env_set_undefined env' un_after) loopend;
      false
  | P.Break xo  -> 
      begin match xo with
	None   -> 
	  let (e,o,env') = env_break_label env in
	  cg_goto env (e,o) []
      | Some l -> 
	  let (s,e,o,env') = lookup_label env l in
	  cg_goto env (e,o) (branch_coercion env')
      end;
      false
  | P.Continue xo -> 
      begin match xo with
	None   -> 
	  let (e,o,env') = env_cont_label env in
	  cg_goto env (e,o) []
      | Some l -> 
	  let (s,e,o,env') = lookup_label env l in
	  cg_goto env (s,o) (branch_coercion env')
      end;
      true
  | P.For(e1,e2,e3,s) ->
      cg_exp env e1;
      let looptest  = Id.id_new "fortest"  in
      let loopbody  = Id.id_new "forbody"  in
      let loopcount = Id.id_new "forcount" in
      let loopend   = Id.id_new "forend"   in
      let env'  = env_set_loop_labels env loopend loopcount in
      let env_b = env_set_undefined env' s.P.un_before      in
      let env_n = env_set_undefined env' e1.P.exp_un_after  in
      let env_t = env_set_undefined env' e1.P.exp_un_after  in (* MAY CHANGE *)
      let env_m = env_set_undefined env' un_after           in
      emit_jmp env  looptest [];
      emit_lab env_b loopbody;
      cg_stmt  env_b s;
      emit_lab_nocon env_n loopcount;
      cg_exp   env_n e3;
      emit_lab_nocon env_t looptest;
      cg_bool_exp env_t e2 (loopbody,branch_coercion env') (loopend,[]) false;
      emit_lab_nocon env_m loopend;
      false
  | P.CharSwitch(e,ss,s) -> (* compile char switch as integer switch *)
      let e' = { P.exp_typ = Some (P.IntType(true,P.B4)); 
		 P.raw_exp = P.Primop(P.Ord,[e]);
		 P.exp_loc = e.P.exp_loc;
		 P.exp_un_after = e.P.exp_un_after;
	       } in
      let ss' = List.map (fun (c,s) -> (to_i32 (Char.code c),s)) ss in
      cg_stmt env {P.raw_stmt=P.IntSwitch(e', ss', s); P.stmt_loc=loc;
		    P.un_before=un_before; P.un_after=un_after}
  | P.IntSwitch(e,ss,s) -> (* just does iterated ifs *)
      cg_exp env e;
      let env  = env_set_undefined env e.P.exp_un_after in
      let env2 = env_add_reg env T.Eax int_con          in
      let end_label = Id.id_new "endswitch"             in
      let end_env = env_set_undefined env un_after      in
      let rec aux r l arms returns = 
	match arms with
	  (i,s)::rest -> 
	      (* now handles '-' character which can't be in a label *)
	    let i' = int32_to_int i in
	    let prefix  = if i' < 0 then "lM" else "l" in
	    let i' = if i' < 0 then -i'  else i' in
	    let l' = Id.id_new (prefix ^ (string_of_int i')) in
	    emit(T.Cmp((T.Reg r,[]),(T.Immed i,[])));
	    emit_jcc env2 T.NotEq l' [];
	    let s_returns = cg_stmt env s in
	    (if not s_returns
            then emit_jmp end_env end_label (branch_coercion env));
	    emit_lab_nocon env2 l';
	    aux r l' rest (s_returns & returns) 
	| [] -> 
	    let s_returns = cg_stmt env s in
	    if not s_returns then fallthru env;
	    (if not (s_returns & returns)  
            then (emit_lab end_env end_label; false)
	    else true) in
      aux T.Eax nonsense_id ss true
  | P.UnionSwitch(e,ss,d) ->
      let r             = T.Eax                          in
      let u_info        = typ2union_info env (exp2typ e) in
      let end_label     = Id.id_new "endswitch"          in
      let default_label = Id.id_new "default"            in
      let name          = Id.id_new "uptr"               in

      (* generate the expression, and update environment in case it inits
	 some variables *)
      cg_exp env e;                 (* gen expression          *)
      emit(T.Coerce(T.Reg r,[T.Unroll])); (* unroll expression       *)
      emit(T.Nameobj(name,T.Reg r)); (* name the value *)
      let env     = env_set_undefined env e.P.exp_un_after in
      let env_end = env_set_undefined env un_after         in

	(* Several auxiliary functions *)
      let rec split cs value_cs void_cs = 
	match cs with
	  [] -> (List.rev value_cs, List.rev void_cs)
	| hd::tl when hd.P.arm_pat = P.No_pat  ->
	    let f = hd.P.arm_field in
	    let s = hd.P.arm_body in
	    let lab = Id.id_new (f ^ "_void")          in
	    let tag = union_void_tag_assoc u_info f    in
	    split tl value_cs ((tag,lab,s) :: void_cs) 
	| hd :: tl -> 
	    let f = hd.P.arm_field in
	    let pat = hd.P.arm_pat in
	    let prim_pat_t p = 
	      match p with
		P.Wild_pat r -> !r
	      | P.Var_pat (_,r) -> !r in
	    let t = 
	      (match pat with
		P.Prim_pat p -> prim_pat_t p
	      | P.Tuple_pat ps -> 
		  P.TupleType(P.ReadOnly,List.map prim_pat_t ps)
	      | _ -> impos "pattern compilation") in
	    let s = hd.P.arm_body in
	    let lab     = Id.id_new (f ^ "_value")     in
	    let tag,con = union_val_tag_assoc u_info f in
	    let con = typ2con t in
	    split tl ((tag,t,con,lab,pat,s)::value_cs) void_cs
      in
	
      let cg_void_branch (tag,lab,_) = 
	emit (T.Cmp((T.Reg r,[]),(T.Immed tag,[])));
	emit (T.Jcc(T.Eq,(lab,[]),None)) in
      let cg_default_void_case tag = 
	emit (T.Cmp((T.Reg r,[]),(T.Immed tag,[])));
	emit (T.Jcc(T.Below,(default_label,[]),None)) in
      let cg_value_branch (tag,_,_,lab,_,_) = 
	emit (T.Cmp((T.Reg T.Ecx,[]),(T.Immed tag,[])));
	emit (T.Jcc(T.Eq,(lab,[]),None)) in

      let cg_void_case (tag,lab,s) = 
	emit_lab_nocon env lab; 
	emit (T.RemoveName name);
	(if not (cg_stmt env s)
	then (emit_jmp env_end end_label (branch_coercion env_end); false)
	else true) in
	
      let cg_value_case (tag,typ,con,lab,pat,s) = 
	let rc = (* Dan: we can avoid making this after rtcg infers labels *)
	  T.cprod [ T.cfield (T.csing (T.pcint tag)) T.Read;
		    T.cfield con T.Read ] in
	let rc   = T.cptr (T.csum [rc])   in
	let env' = env_add_reg env r rc       in
	emit_lab_nocon env' lab;
	(match pat with (*For floating point cannot perform unnecessary load.*)
	  P.Prim_pat(P.Wild_pat _) -> ()
	| _ -> load typ (T.Prjr((r,[T.Fromsum;T.Forgetname]),i32_4,None)));
	emit (T.RemoveName name);
	let (env',num_slots) = 
	  (match pat with
	    P.Prim_pat(P.Wild_pat _) -> 
	      (env,0) (* the Mov into Eax above is not
			 * necessary here but oh well... *)
	  | P.Prim_pat(P.Var_pat (x,_)) -> 
	      push_std typ; 
	      (env_add_local_var env x con, P.words_in_typ typ)
	  | P.Tuple_pat ps ->
	      let rec loop i c ps env = 
		match ps with
		  [] -> (env,c)
		| (P.Wild_pat r)::ps -> 
		    let size = to_i32 (4 * P.words_in_typ (!r)) in
		    loop (i +$ size) c ps env
		| (P.Var_pat(x,r))::ps -> 
		    let typ = !r in
		    let words = P.words_in_typ typ in
		    let size = to_i32 (4 * words) in
		    let con = typ2con typ in
		    push_mem_to_stack (T.Prjr ((T.Eax,[]),i,None)) (Typ typ);
		    (* emit(T.Push(T.Prjr((T.Eax,[]),i,None),[])); *)
		    loop (i+$ size) 
		      (c+words) ps (env_add_local_var env x con) in
	      loop i32_0 0 ps env
	  | P.No_pat -> impos "pattern compilation 2") in
	let s_returns = cg_stmt env' s in
	if not s_returns 
	then (pop_free num_slots; 
	      emit_jmp env_end end_label (branch_coercion env_end)); 
	s_returns in

	(* Reorganize cases, etc. *)
      let (value_cases,void_cases) = split ss [] []                   in
      let value_cases = Sort.list 
	  (fun (tag1,_,_,_,_,_)(tag2,_,_,_,_,_)->(tag1>tag2)) value_cases in
      let void_cases = Sort.list 
	  (fun (tag1,_,_) (tag2,_,_)->(tag1>tag2)) 	    void_cases  in

      let voids_exhaust   = List.length void_cases = union_num_voids u_info in
      let has_value_cases = match value_cases with [] -> false | _ -> true  in
      let has_default     = match d with None -> false | _ -> true          in

	(* Actually generate code *)
      	
      if has_default                (* gen branch instructions *)
      then 
	(List.iter cg_void_branch void_cases;
	 (if (not voids_exhaust) & has_value_cases
	 then 
	   let tag,_,_,_,_,_ = List.hd (List.rev value_cases) in
	   cg_default_void_case tag);
	 if has_value_cases then
	   (emit (T.Mov(T.Reg T.Ecx,(T.Prjr((r,[]),i32_0,None),[])));
	    List.iter cg_value_branch value_cases);
	 emit_jmp env default_label [])
      else
	if has_value_cases
	then
	  (List.iter cg_void_branch  void_cases;
	   if (List.tl value_cases) <> [] then
	     (emit (T.Mov(T.Reg T.Ecx,(T.Prjr((r,[]),i32_0,None),[])));
	      List.iter cg_value_branch (List.tl value_cases)))
	else
	  List.iter cg_void_branch (List.tl void_cases);
	  
      let values_return   =         (* gen value   blocks *)
	List.for_all (fun x -> x) (List.map cg_value_case value_cases) in
      let voids_return    =         (* gen void    blocks *)
	List.for_all (fun x -> x) (List.map cg_void_case void_cases)   in
      let default_returns =         (* gen default block  *)
	match d with
	  None   -> true
	| Some s -> emit_lab_nocon env default_label; 
	    if not (cg_stmt env s) then (fallthru env; false) 
		                   else true                   in
	                              (* finish and return *)
      let returns = voids_return & (values_return & default_returns)   in
      if not returns then emit_lab env_end end_label;
      returns

  | P.ExnSwitch(e,ss,d) -> (* Exception switch *)
      let end_label = Id.id_new "switchend"                                in
      let exnname_arg_var = Id.id_new "c"                                  in
      let exnname_arg_con = exnname_arg_con exnname_arg_var                in
      
      cg_exp env e;
      emit(T.Unpack(exnname_arg_var,T.Eax,(T.Reg T.Eax,[])));
      emit(T.Mov(T.Reg T.Ebx,(T.Prjr ((T.Eax,[]),i32_0,None),[])));

      let env     = env_set_undefined env e.P.exp_un_after               in 
      let env'    = env_add_reg env  T.Eax (exn_body exnname_arg_var)    in
      let env'    = env_add_reg env' T.Ebx (exnname_con exnname_arg_con) in
      let env_end = env_set_undefined env un_after                       in

      let rec cg_arm { P.arm_field = exn; P.arm_pat = pat; P.arm_body = s} =
	let loc, con = lookup_exn env exn in
	let nextlab = Id.id_new "exn_case" in
	let exn_id  = tid_exn exn in

	let src =
	  (* may be a globally defined exn name, or a local variable *)
	  (match loc with
	    LocalExncon ->
	      (try 
		let offset = env_local_var_offset env exn in
		T.Prjr((T.Esp,[]),to_i32(4*offset),None)
	      with Not_found -> 
		impos "cg_exp: exnname not in local env")
	  | ExnconDecl ->
	      (T.Addr exn_id)
	  | GlobalExncon -> T.Prjl ((tid_val exn,[]),i32_0,None))
	in

	emit(T.Cmp((T.Reg T.Ebx,[]), (src,[])));
	emit(T.Jcc(T.NotEq, (nextlab, 
			     exncase_coercion env' exnname_arg_con), 
		   None));
	let env'',count = 
	  match pat with
	    P.No_pat -> (env,0)
	  | P.Prim_pat(P.Wild_pat r) -> (env,0)
	  | P.Prim_pat(P.Var_pat(x,r)) ->
	      (push_mem_to_stack (T.Prjr ((T.Eax,[]),i32_8,None)) (Con con);
	       (env_add_local_var env x con, words_in_con con))
	  | P.Tuple_pat ps ->
	      (emit (T.Mov (T.Reg T.Eax,(T.Prjr((T.Eax,[]),i32_8,None),[])));
	       let rec loop offset count env ps = 
		 match ps with
		   [] -> (env,count)
		 | (P.Wild_pat r)::rest -> 
		     let size = to_i32 (P.words_in_typ (!r)) in
		     loop (offset +$ size) count env rest
		 | (P.Var_pat(x,r))::rest ->
		     (let typ = !r in
		     let words = P.words_in_typ typ in
		     push_mem_to_stack 
		       (T.Prjr ((T.Eax,[]),offset,None)) (Typ typ);
		     loop (offset +$ (to_i32 (4*words))) (count+words)
			(env_add_local_var env x (typ2con typ)) rest)
	       in loop i32_0 0 env ps) in
	let arm_returns = cg_stmt env'' s in
	if not arm_returns then 
	  (pop_free count; 
	   emit_jmp env_end end_label (branch_coercion env_end));
	emit_exncase env' nextlab exnname_arg_var;
	arm_returns in
	
      let env' = env_add_reg env' T.Eax (exn_body exnname_arg_var) in
      let env' = env_add_reg env' T.Ebx (exnname_con exnname_arg_con) in
      exncase_fallthru env' exnname_arg_con;
      emit_exncase env' (Id.id_new "exn_case") exnname_arg_var;
                              (* DJG: Bug fix -- Avoid short-circuiting *)
      let arms_return     = List.for_all (fun x -> x) (List.map cg_arm ss) in
      let default_returns = cg_stmt env (deSome d)        in
      let returns         = arms_return & default_returns in
      if not default_returns then fallthru env;
      if not returns         then emit_lab env_end end_label;
      returns
  | P.Decl(x,t,roe,s) ->
      let size = P.words_in_typ t in
      (match !roe with
	Some e -> 
	  cg_exp env e; 
	  push_std t
      |	None ->
	  (* If the size >2 then the below pushes to cons onto the stack
	     -- junk4 :: junk4, but we rely on the NUMNER of cons when doing
	     type instantiations.  Therefore we must coerce this to a 
	     junk8. *)
	  (emit(T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed (to_i32 (4*size)))));
	  if (size=2) then emit (T.Coerce(T.Reg T.Esp, [T.Slot (i32_0,i32_8)]));
	  ());
      if not (cg_stmt (env_add_local_var env x (typ2con t)) s)
      then (pop_free size; false)
      else (true)
  | P.Label(x,s) ->
      let label_start = Id.id_new (x^"_start") in
      let label_end   = Id.id_new (x^"_end")   in
      let env'        = env_add_label env x label_start label_end in
      fallthru env;
      emit_lab env' label_start;
      let returns = cg_stmt env' s in
      if not returns then 
	(fallthru env'; emit_lab (env_set_undefined env' un_after) label_end);
      returns
  | P.Do(s,e) ->
      let loopstart = Id.id_new "loopstart" in
      let looptest  = Id.id_new "looptest"  in
      let loopend   = Id.id_new "loopend"   in
      fallthru env;
      let env' = env_set_loop_labels env loopend loopstart in
      emit_lab env' loopstart;
      cg_stmt env' s;
      emit_lab_nocon env' looptest; (* MAY CHANGE *)
      cg_bool_exp env' e 
	(loopstart,branch_coercion env') (loopend,[]) false;
      emit_lab_nocon env' loopend; (* MAY CHANGE *)
      false
  | P.TryHandle(s1,x,s2) ->
	let trycatch = Id.id_new "trycatch" in
	let tryend   = Id.id_new "tryend"   in
	let env'     = push_con env T.Ebp (env_handler_con env) in
	let handler_coercion = branch_coercion env' in 
	let handler_gop      = T.Addr trycatch, handler_coercion in
	emit(T.Push handler_gop);
	emit(T.Mov (T.Reg T.Ebp,(T.Reg T.Esp,[])));

	emit_comment("Begin try body.");
	let handler_t = 
	  handler_code_type (env_set_undefined env' un_before) in
	let env'' = env_try_body env' handler_t handler_gop in
	let s1_returns = cg_stmt env'' s1 in
	emit_comment("End try body.");
	if not s1_returns 
	then begin
	  pop_free 1; (* pop handler *)
	  pop T.Ebp;  (* restore EBP *)
	  emit_jmp env tryend (branch_coercion env);
	end;
	emit_label trycatch (Some handler_t);
	pop T.Ebp; (* restore EBP *)
	emit(T.Push (T.Reg T.Eax,[]));
	let env' = env_add_local_var env x exn_con in
	(if !P.stack_traces then cstk_reset env');
	let s2_returns = cg_stmt env' s2 in
	if not s2_returns
	then begin 
	  pop_free 1; 
	  fallthru env;     
	end;
	let s_returns = s1_returns & s2_returns in
	if not s_returns then emit_lab (env_set_undefined env un_after) tryend;
	s_returns
    | P.TryCatchFinally(_,_,_,_) -> 
	impos "TryCatchFinally should've been eliminated"
(* Cyclone *)
    | P.Cut stmt ->
        let env = flush_vis_fenv env in
	if not (in_frame env)
	then impos "cut can only be used within a codegen or splice";

              (* Hack with type abbrevs to avoid MASM string too long *)
	let post_con        = mk_label_con env    in
        let post_con_abbrev = abbrev_con post_con in
              (* Find the next visible frame. *)
              (* This is too convoluted, but requires less modification
                 of the original environment type.  Someday we should just
                 go for it. *)
        let env2 = env_cut_body env in
              (* This is tricky... 
              ** here returns is true if the CUT statement
              ** lays out templates that all return *)
        let returns = PT.doesSpliceRet stmt in
        let post_con_opt = Some post_con_abbrev in
              (* Type of ECG will change as we dump and fill *)
	fallthru env;
        let env3 =
          cg_fill_holes
            env2
            post_con_opt
            false in
              (* WATCH OUT: can the type of ECG change if there are
		 ** splices in stmt?? *)
        cg_stmt env3 stmt;
              (* Note, the post-condition of the last template becomes
                 the pre-condition of the next template *)
        gen_code_mark(M_TemplateBeg(Id.id_new "tmpl_start",
                                    post_con_abbrev,
                                    get_vis_fenv env3));
              (* The TAL parser wants a label at the beginning of each
              ** template; the label serves no other purpose. *)
              (* We can't use the abbreviated post_con here because of
              ** talverify; maybe talverify should be changed. *)
        gen_code_mark(M_Label(Id.id_new "cut",Some post_con));
        returns
    | P.Splice stmt ->
        let env     = flush_vis_fenv  env           in
        let env2    = env_splice_body env           in
	let env2    = env_set_undefined env2 stmt.P.un_before in
        let pre_con = abbrev_con(mk_label_con env2) in
        gen_code_mark(M_TemplateBeg(Id.id_new "tmpl_start",
                                    pre_con,
                                    get_vis_fenv env));
              (* The TAL parser wants a label at the beginning of each
              ** template; the label serves no other purpose. *)
	let splice_id = Id.id_new "splice" in
        emit_lab env2 splice_id;
        let returns      = cg_stmt env2 stmt                      in
        let post_con_opt = if returns then None else Some pre_con in
	begin match post_con_opt with
	  None -> () 
	| Some _ -> fallthru env2
	end;
        cg_fill_holes env post_con_opt true;
        false
    | P.With(x,topr,vs,e,s) ->
	cg_exp env e;
	(* unroll the abstract type *)
	emit(T.Coerce(T.Reg T.Eax,[T.Unroll]));
	(* unpack all of the abstracted type variables *)
	List.iter 
	  (fun v -> (emit(T.Unpack(tid_tyv v,T.Eax,(T.Reg T.Eax,[]))))) 
	  (List.rev vs);
	(* add the abstracted type variables and the value variable to env *)
	emit(T.Push (T.Reg T.Eax,[]));
	let env' = env_add_local_var (env_add_tyvars env vs) x 
	    (typ2con (deSome (!topr))) in
	(* generate the body of the statement *)
	let s_returns = cg_stmt env' s in
	if not s_returns then pop_free 1;
	s_returns
    | P.Rdtsc(e1,e2) ->
	(* For now, both e1 and e2 must be integers. *)
	let v1,v2 = 
	  match e1.P.raw_exp, e2.P.raw_exp with
	  | P.Var v1, P.Var v2 -> v1, v2 
	  | _,_ -> impos "cg_exp: rdtsc arguments are not variables."
	in
	let gop x = (* Modified from cg_lhs_exp. *) 
	  (try 
	  (* If deeper than a handler, use EBP to appease the verifier *)
	    let offset = env_local_var_offset env x in
	    if env_in_try env && offset > (env_s1len env)
	    then T.Prjr ((T.Ebp,[]),to_i32 (4*(offset - (env_s1len env))),None)
	    else T.Prjr ((T.Esp,[]),to_i32 (4*offset),                    None)
	  with Not_found -> 
	    try 
	      let t = lookup_global env x in
	      let n = tid_val x  in 
	      if (needs_indirect t) then T.Prjl ((n,[]),i32_0,None)
	      else T.Addr n
	    with Dict.Absent -> impos "cg_exp: Var without type.")
	in
	emit (T.Rdtsc);
	emit (T.Mov(gop v1,(T.Reg T.Edx,[])));
	emit (T.Mov(gop v2,(T.Reg T.Eax,[])));
	false
(* End Cyclone *)

(* Cyclone *)
(* cg_fundecl has been changed so that whoever calls it is required
   to set up the environment properly.  This is needed by Cyclone. *)
(* End Cyclone *)
and cg_fundecl env fd =
  let name    = fd.P.fn_name              in
  let convention = fd.P.fn_convention     in
  let tyargs  = fd.P.fn_tyvars            in
  let ret_typ = fd.P.fn_ret_type          in
  let body    = fd.P.fn_body              in
  let params  = List.map snd fd.P.fn_args in
  let (con,stack1_type,stack2_type) = fun_con convention tyargs ret_typ params in
  let param_size = 4 * (P.sumtyps params) in
  let lab     = tid_fun convention param_size name in
  (if fd.P.fn_static then
    (* MWH - this seems redundant; is there some side-effect that we need? *)
(*    let (con,_,_) = fun_con tyargs ret_typ params in *)
    gen_export_val (lab,con,P.FnType (convention,tyargs, ret_typ,params)));
  gen_set_label (lab,Some con);
  let env = if !P.stack_traces then cstk_fn_entry env name else env in 
  if (not (cg_stmt env body)) then 
    begin
      if !P.stack_traces then pop_free 1;
      match convention with
	P.Cdecl -> emit(T.Retn None)
      |	P.Stdcall -> emit (T.Retn (Some (int_to_int32 param_size)))
    end;
  flush_code ();
  (name = main_id)

(* cg_typedecls : env -> P.top_decl list -> (env * T.con_block list)
 *  Generates the tal constructors for popcorn type declarations.
 *)
let rec cg_typedecls env tds = 
  let rec aux tds con_blocks structs unions abstypes =
    match tds with
      [] -> (con_blocks,structs,unions,abstypes)
    | ((P.StructDecl sd,_) :: tl) ->
	let label     = tid_type sd.P.st_name                               in
	let mem_label = tid_mem  sd.P.st_name                               in
	let s_info    = info_structdecl sd                                  in
	let c,   k    = struct_t     s_info                                 in
	let memc,memk = struct_mem_t s_info                                 in
	let real_mem  = mem_label, memk, T.ConcCon memc                     in
	let mem       = mem_label, memk, memc                               in
	let int_con   = label,     k,    T.ConcCon c                        in
	let con       = label,     k,    c                                  in
	let abs_con   = label,     k,    T.AbsCon                           in
	let abs_mem   = mem_label, memk, T.AbsCon                           in
	let aux' cons = 
	  aux tl cons ((sd.P.st_name,s_info)::structs) unions abstypes      in
	begin match sd.P.st_scope with
	  P.Public -> 
	    gen_export_con int_con; 
	    gen_export_con real_mem;
	    aux' (con::mem::con_blocks)
	| P.Abstract -> 
	    if struct_null s_info    (* Export null for all option types *)
	    then (gen_export_con int_con; gen_export_con abs_mem)
	    else gen_export_con abs_con;
	    aux' (con::mem::con_blocks)
	| P.Extern -> 
	    gen_import_con int_con; 
	    gen_import_con real_mem; 
	    aux' con_blocks
	| P.Static -> aux' (con::mem::con_blocks) 
	end
    | ((P.UnionDecl ud,_) :: tl) ->
	let label     = tid_type ud.P.un_name                                in
	let u_info    = info_uniondecl ud                                    in
	let c,k       = union_t u_info                                       in
	let real_con  = label, k, T.ConcCon c                                in
	let con       = label, k, c                                          in
	let abs_con   = label, k, T.AbsCon                                   in
	let aux' cons =  
	  aux tl cons structs ((ud.P.un_name,u_info)::unions) abstypes       in
	begin match ud.P.un_scope with
	  P.Public   -> gen_export_con real_con; aux' (con::con_blocks)
	| P.Abstract -> gen_export_con abs_con;  aux' (con::con_blocks)
	| P.Extern   -> gen_import_con real_con; aux' con_blocks
	| P.Static   ->                          aux' (con::con_blocks)
	end
    | ((P.AbsDecl ad,_) :: tl) ->
	let label     = tid_type ad.P.abs_name                               in
	let abs_info  = info_abstype ad                                      in
	let c,k       = abstype_t abs_info                                   in
	let real_con  = label, k, T.ConcCon c                                in
	let con       = label, k, c                                          in
	let abs_con   = label, k, T.AbsCon                                   in
	let aux' cons =  
	  aux tl cons structs unions ((ad.P.abs_name,abs_info)::abstypes)    in
	begin match ad.P.abs_scope with
	  P.Public   -> gen_export_con real_con; aux' (con::con_blocks)
	| P.Abstract -> gen_export_con abs_con;  aux' (con::con_blocks)
	| P.Extern   -> gen_import_con real_con; aux' con_blocks
	| P.Static   ->                          aux' (con::con_blocks)
	end
    (* Externs do not add anything to the environment and result in no
       code -- they are noted in the module imports *)
    | ((P.ExternType (tn,tyvs,opt),_)::tl) ->
	let kind_con  = tyvars_to_kind tyvs T.k4byte             in
	let kind_mem  = tyvars_to_kind tyvs T.kmem               in
	let tylam     = tyvars_to_lam  tyvs                      in
	let label     = tid_type tn                              in
	let mem_label = tid_mem  tn                              in
	let abs_con   = (label,     kind_con, T.AbsCon)          in
	let abs_mem   = (mem_label, kind_mem, T.AbsCon)          in
	let tycons    = tyvars_to_cons tyvs                      in
	let conc_con  = tylam (opt_con (mem_name_con tn tycons)) in
	let con       = (label,kind_con,T.ConcCon conc_con)      in
	(if opt 
	then (gen_import_con con; gen_import_con abs_mem)
	else gen_import_con abs_con);
    	aux tl con_blocks structs unions abstypes
    | ((P.ExternVal (v,t),_) :: tl) ->
	let ptr_to con = T.cprod_b [T.cfield con T.ReadWrite]           in
	let con'       = typ2con t                                      in
	let con        = 
	  if (needs_indirect t) then ptr_to con' else con' in
	let tid = 
	  match t with
	    P.FnType (c,_,_,params) ->
	      let param_sz = 4 * (P.sumtyps params) in
	      (tid_fun c param_sz v)
	  | _ -> tid_val v in
	gen_import_val (tid,con,t);
	aux tl con_blocks structs unions abstypes
    | _ -> impos "cg_typedecl : This is not a type declaration."
  in
  let (con_blocks,structs,unions,abstypes) = aux tds [] [] [] []        in
  let env = env_add_structs env structs in
  let env = env_add_unions  env unions  in
  let env = env_add_abstypes env abstypes in
  (env,con_blocks)

(* cg_global_decls: env -> global_decl list -> data_block list

  Popcompenv.env -> (P.scope * string * typ * P.exp option ref) list ->
  (Popcomptypes.id * T.con option * (T.data_item list * T.coercion list)) list 
*)

(* compile_data: string -> env -> P.exp -> P.typ option ->
                 (T.data_block list) * T.con * T.data_item *)
let rec compile_data n env e cast_topt =
   let return con data blocks =
     let label = Id.id_unique (tid_val n) in
     (((label, i32_4,Some con, data)::blocks), con, T.Dlabel(label,[]))  in
   let con = exp2con e in
      (* must be a "constant" initialization expression *)
   match e.P.raw_exp with
	P.Const(P.Int  i) -> ([], con, T.D4bytes (i,
                                                  [T.Subsume T.cbyte4]))
      | P.Const(P.Char c) -> ([], con, T.D4bytes (to_i32 (Char.code c),
			                          [T.Subsume T.cbyte4]))
      | P.Const(P.Bool b) -> ([], con, T.D4bytes ((if b then i32_1 else i32_0),
			                          [T.Tosum con]))
      | P.Const(P.Null)   -> ([], con, T.D4bytes (i32_0, [T.RollTosum con]))
      | P.Const(P.String s) -> ([], con, T.Dlabel (add_string s,[])) 
      |	P.Const(P.Float f)  -> 
	  (match cast_topt with 
	  | Some P.DoubleType -> 
	      ([], typ2con P.DoubleType, T.Dfloat64 (f32_to_f64 f))
	  | Some t -> impos "cg_global_decsl: Global float cast unimplemented."
	  | _ ->  ([], con, T.Dfloat32 f))
      |	P.Const(P.Double d) -> 
	  (match cast_topt with
	  | Some P.FloatType -> ([],typ2con P.FloatType,
				 T.Dfloat32 (f64_to_f32 d))
	  | Some _ -> impos "cg_global_decl: Global double cast unimplemented."
	  | _ -> ([], con, T.Dfloat64 d))
      | P.ConstArray(es,ot) ->
	  let typ = 
	    match (es,ot) with
	      (_,      Some t) -> t 
	    | (hd::tl, None)   -> exp2typ hd
	    | _  -> impos "cg_global_decls : ConstArray without type." in
	  let len = to_i32 (List.length es)         in
	  (* MWH -- 9/13/00
	     Change the cons the types of the elements in the
	     array to be the array type---this is because during
	     typechecking, only the root array Evar gets resolved,
	     not the Evars of its elements *)
	  List.iter (function e -> e.Popsyntax.exp_typ <- Some typ) es;
	  let (blocks, _, datas) = compile_datas n env es None in
	  let c = typ2con typ                       in
	  
	  let field = T.cfield c T.ReadWrite in
	  let the_array = (datas,[T.Toarray(i32_0,0,field)]) in
	  let arr_label = Id.id_unique (tid_val "array") in 
	  let the_ref = ([T.D4bytes (len,[]); T.Dlabel (arr_label,[])],
			 [T.Pack (T.pcint len,con)]) in
	  let arr_d = (arr_label,i32_4,
		       Some (T.cprod_b [(T.carray (T.pcint len) field)]),
		       the_array) in
	  return con the_ref (arr_d::blocks)
      | P.NewStruct(tn, tsor, named_es) ->
	  let ts     = deSome !tsor                 in
	  let cs     = types2cons ts                in
	  let s_info = lookup_struct env tn         in
	  let es = 
	    let rec loop offset named_es = 
	      match named_es with
		[] -> []
	      |	(None,e)::rest -> 
		  let size = 4 * P.words_in_typ (exp2typ e) in
		  (offset,e)::(loop (offset+size) rest)
	      |	(Some f,e)::rest ->
		  (struct_field_offset s_info f,e)::(loop offset rest)
	    in let pos_es = loop 0 named_es in
	    let cmp (i,_) (j,_) = i <= j in
	    List.map snd (Sort.list cmp pos_es) in
	  let (blocks, _, datas) = compile_datas n env es None in
	  let coerce = roll_struct s_info tn cs     in
	  return con (datas, coerce) blocks 
      | P.NewUnion(tn,tsor,f,eopt) ->
	  let ts     = deSome !tsor        in
	  let cs     = types2cons ts       in
	  let u_info = lookup_union env tn in
	  let coerce = T.RollTosum con     in
	  (match eopt with
	    None   -> 
              let tag = union_void_tag_assoc u_info f in
	      ([],con,T.D4bytes (tag, [coerce]))
	  | Some e ->
	      let blocks, _, data = compile_data n env e None in
	      let tag,_ = union_val_tag_assoc u_info f in
	      return con ([(T.D4bytes (tag,[])); data], [coerce]) blocks)
      | P.NewTuple es ->
	  let (blocks, cons, datas) = compile_datas n env es None in
	  let con = 
	    (match cast_topt with
	      Some t ->
		(match t with
		  P.TupleType (cap,ts) ->
		    typ2con t
		| _ -> impos "non-tuple-typ cast for ^()")
	    | None ->
		T.cprod_b (List.map (fun c -> (T.cfield c T.ReadWrite)) cons))
	  in
	  return con (datas,[]) blocks 
      |	P.NewExn (xname,eopt) ->
	  let exn_id = tid_exn xname in
	  let loc_info_id = add_location e.P.exp_loc in 
          let loc, field_con = lookup_exn env xname in
	  if loc <> ExnconDecl then 
	    impos "non-decl exncon in global intitialization statement";
	  let field_con' = T.cfield field_con T.Read in
	  let name = Id.id_new "exn" in
	  let (blocks, _, datas) =
	    let (b,c,d) =
	      (match eopt with 
		None    -> ([], field_con, T.D4bytes (i32_0, []))
	      | Some e' -> compile_data n env e' None) in
	    (b,c,[(T.Dlabel (exn_id,[]));
		  (T.Dlabel (loc_info_id,[]));
		  d]) in
	  return exn_con (datas,[T.Pack (field_con',exn_con)]) blocks
(* LR *)
      |	P.TypInst ({ P.raw_exp = P.RepTerm }, [t]) ->
	  let d = compile_native_rep_data (typ2con t) in
	  return con ([d],[]) []
(* end LR *)
      | P.Var v ->
	  (* XXX eliminate duplicated code in the next two cases *)
	  (* First check if it's an exception con *)
	  (try 
	    let loc, con = lookup_exn env v in
	    let field_con = T.cfield con T.Read in
	    (match loc with
	      ExnconDecl ->
		([], exnname_con field_con, T.Dlabel (tid_exn v,[])) 
	    | _ -> impos "cg_global_decl: non exception decl in glbl init")
	  with Dict.Absent ->
	    (* see if it's a function decl *)
	    (if is_fun_decl env v then
	      let t = lookup_global env v in
	      ([], typ2con t, T.Dlabel (tid_val v,[]))
	    else
	      impos "cg_global_decl: illegal name in glbl init"))
      |	P.Primop(P.AddrOf,[e]) ->
	  (* Just like the Var case above for exncons and fundecls *)
	  (match e.P.raw_exp with
	    P.Var v ->
	      (* First check if it's an exception con *)
	      (try 
		let loc, con = lookup_exn env v in
		let field_con = T.cfield con T.Read in
		(match loc with
		  ExnconDecl ->
		    ([], exnname_con field_con, T.Dlabel (tid_exn v,[])) 
		| _ -> impos "cg_global_decl: non exception decl in glbl init")
	      with Dict.Absent ->
	        (* see if it's a function decl *)
		(let t = lookup_global env v in
		if is_fun_decl env v then
		  ([], typ2con t, T.Dlabel (tid_val v,[]))
		(* it's a variable, get its address and change its type *)
		else
		  ([], typ2con (P.TupleType (P.ReadWrite,[t])), 
		   T.Dlabel (tid_val v,[]))))
	  | _ -> impos "cg_global_decl: & requires a variable argument")
      |	P.Cast(t,e) ->
	  (* Pass in the expected type to be applied when the expression
	     is processed. *)
	  compile_data n env e (Some t)

      |	_ -> impos "cg_global_decl: Expected a global variable declaration"

and compile_datas n env es cast_tos_opt =
  let aux cast_typ elt (blocks, cons, datas) =  
    let (subBlocks, con, data) = compile_data n env elt cast_typ in
    ((subBlocks@blocks), (con::cons), (data::datas))
  in
  (match cast_tos_opt with
    None -> List.fold_right (aux None) es ([], [], []) 
  | Some topts ->
      List.fold_right2 aux topts es ([], [], []))

let cg_global_decls env globals =
  (* generates a data_block for the given global_decl (tuple arg) and
     concatentates it with the given data_block list *)
  let cg_global dbs (scope,n,typ,eor) =
    let (blocks, con, data) = compile_data n env (deSome !eor) None in
    let con' = T.cprod_b [T.cfield con T.ReadWrite] in
    let nt   = tid_val n                            in
    if scope = P.Public then gen_export_val (nt,con', typ);
    (nt, i32_4,Some con', ([data],[])) :: (blocks@dbs)
  in
  List.fold_left cg_global [] globals

let cg_exn_decls exns = (* : (var * scope * typ) list -> data_block list *)
  let rec aux (v,sc,typ) rest =
    let con       = typ2con (P.ExnconType typ)                   in
    let label     = tid_exn v                                    in
    let data_item = (label, i32_4, Some con, 
		     ([T.D4bytes (i32_0,[T.Subsume T.cbyte4])],[])) in
    match sc with
      P.Static   -> data_item :: rest
    | P.Public   -> gen_export_val (label,con,P.ExnType); data_item :: rest
    | P.Extern   -> gen_import_val (label,con,P.ExnType); rest
    | P.Abstract -> impos "cg_exn_decls: Abstract exception declaration."
  in
  List.fold_right aux exns []

(* cg_all : string -> Popcompenv.env -> P.top_decl list ->
            (Popcomptypes.id * T.kind * Popcomptypes.con) list * bool *)
(* Sort top decls, Generate type decls, Generate functions *)
let cg_all mod_name env topdecls = 
  let (type_decls,fun_decls,global_decls,exn_decls) = 
    let rec sort_decls ds ts fs gs xs = 
      match ds with
	[]                        -> (ts,fs,gs,xs)
      | (P.FunDecl fd,_)::tl      -> sort_decls tl ts (fd::fs) gs xs
      | (P.GlobalDecl g,_)::tl    -> sort_decls tl ts fs (g :: gs) xs
      | (P.ExceptionDecl x,_)::tl -> sort_decls tl ts fs gs (x::xs)
      | (P.OpenDecl(prefix,decls),_)::tl ->
	  let (ts,fs,gs,xs) = sort_decls decls ts fs gs xs in
	  sort_decls tl ts fs gs xs
      (* includes StructDecl, UnionDecl, ExternType, ExternVal, PrefixDecl *)
      | hd::tl                    -> sort_decls tl (hd::ts) fs gs xs in
    sort_decls topdecls [] [] [] []  in
  reset_generator mod_name;

  let (env,con_blocks) = cg_typedecls env type_decls in 

  let data_blocks = 
    (cg_exn_decls exn_decls) @ (cg_global_decls env global_decls) in
  List.iter gen_data_block data_blocks;

  let has_main_list =
    List.map (fun fd -> cg_fundecl (env_fun_start env fd) fd) fun_decls in

  let has_main = List.exists (fun x -> x) has_main_list in
  if has_main then 
    (gen_code_block exn_wrapper_block;
     gen_code_block tal_main_code_block);
  
  (con_blocks,has_main)

(* Note: marks are processed in *REVERSE* order *)
let postprocess marks =
  let rec pp marks cbs istack tmps cuts tcons abbrevs =
    match marks with
      [] ->
        begin
          match istack,cuts with
            [],[] -> (tmps,cbs,abbrevs)
          | _,[]  -> impos "instructions without label"
          | [],_  -> impos "cuts not empty"
          | _     -> impos "instructions without label, cuts not empty"
        end
    | m::ms ->
        let issue x = pp ms cbs ([|x|]::istack) tmps cuts tcons abbrevs in
        match m with
          M_Label(id,con) ->
            let cb = (id,con,Array.concat istack) in
            pp ms (cb::cbs) [] tmps cuts tcons abbrevs
        | M_ISeq i ->
            pp ms cbs (i::istack) tmps cuts tcons abbrevs
        | M_TypeAbbrev(v,c) ->
            pp ms cbs istack tmps cuts tcons ((v,c)::abbrevs)
(* Cyclone *)
        | M_Jmp(label,{contents=None},target_con,coercion) ->
            issue (T.Jmp(T.Addr label,coercion))
        | M_Jmp(_,{contents=Some(temp,hole)},target_con,coercion) ->
            issue (T.CgHoleJmp(temp,(hole,coercion)))
        | M_Jcc(cc,label,{contents=None},target_con,coercion) ->
            issue (T.Jcc(cc,(label,coercion),None))
        | M_Jcc(cc,_,{contents=Some(temp,hole)},target_con,coercion) ->
            issue (T.CgHoleJcc(cc,temp,(hole,coercion),None))
        | M_TemplateEnd c ->
            pp ms [] [] tmps ((cbs,istack)::cuts) (c::tcons) abbrevs
        | M_TemplateBeg(id1,pre_con,_) ->
            begin
              match istack,cuts,tcons with
                [],(cbs0,istack0)::tl,c::tcons ->
                  pp ms cbs0 istack0 ((id1,c,cbs)::tmps) tl tcons abbrevs
              | _,_,_ -> impos "pp M_TemplateBeg"
            end
        | M_Fill _ -> pp ms cbs istack tmps cuts tcons abbrevs
(* End Cyclone *)
  in pp marks [] [] [] [] [] []

(************************************Entry Point*******************************)
let code_gen mod_name import_file export_file ifc_only (top_decls,global_env) =

  (* Add in predefined exception packets -- for internal use only. *)
  let top_decls = 
    if not ifc_only then
      let d_exn nm = (P.ExternVal(nm^"_pkt",P.ExnType),bogus_loc) in
      let debug_malloc_decls =
	[ (P.ExternVal(current_file_name,P.StringType),bogus_loc);
	  (P.ExternVal(current_line_name,P.IntType (false,P.B4)),bogus_loc) ]
      in
      (* Call stack traces begin. *)
      let stack_trace_decls = 
	[ d_exn P.active_exn] 
      in
      ((if !P.stack_traces then stack_trace_decls else []) @ 
       (* Call stack traces end. *)
       ((* if !debug_malloc then debug_malloc_decls else *) []) @
       (d_exn P.null_pointer_exn)::
       (d_exn P.union_variant_exn)::
       (d_exn P.array_bounds_exn)::
       top_decls) 
    else top_decls in
  
  let env = env_empty global_env in

  let (con_blocks,has_main)           = cg_all mod_name env top_decls in

  (* pop_exit used by Cyclone so always import. *)
  if not ifc_only then
    (gen_import_val (pop_exit_label, pop_exit_con, P.VoidType);
     (* Builtin raises *)
     cg_builtin_raise P.null_pointer_exn;
     cg_builtin_raise P.union_variant_exn;
     cg_builtin_raise P.array_bounds_exn;
     flush_code()); (* Otherwise the last block doesn't get printed. *)
  
  let (templates,code_blocks,abbrevs) = postprocess gen_state.code_marks in
  let abbrevs = Array.of_list (std_abbrevs@abbrevs) in 
  let implementation =     
    { T.import_refs     = 
      [|(T.Int_filename "pop_runtime.tali"); import_file |];
      T.export_refs     = 
      (if has_main then 
	[|(T.Int_filename "tal_prog.tali"); export_file|]
      else [|export_file|]);
      T.pre_imp  = 
        { T.imp_abbrevs = [||];
          T.imp_kindabbrevs = [||]; (* LX *)
          T.con_blocks  = Array.of_list (bogus_option_con_block :: con_blocks);
          T.code_blocks = Array.of_list code_blocks;
          T.data_blocks = Array.of_list gen_state.data_blocks;
(* Cyclone *)
	  T.templates   = Array.of_list templates
(* End Cyclone *)
	};
    } in
  let export_interface = 
    { T.int_abbrevs = abbrevs;
      T.int_kindabbrevs = [||]; (* LX *)
      T.int_cons    = Array.of_list gen_state.export_cons;
      T.int_vals    = Array.of_list gen_state.export_vals
    }	in
  let import_interface = 
    { T.int_abbrevs = abbrevs;
      T.int_kindabbrevs = [||]; (* LX *)
      T.int_cons    = Array.of_list gen_state.import_cons;
      T.int_vals    = Array.of_list gen_state.import_vals
    }	in
  (implementation,import_interface,export_interface)


