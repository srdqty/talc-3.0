(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew,                                     *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* talctxt.ml
 * Contexts for verification of TAL kinds, type constructors, and terms.
 *
 *)

open Numtypes;;
open Identifier;;
open Tal;;

(*** Locations ***)

type tal_loc =
    Loctop 
  | Lockind of identifier   (* kind abbrev  - LX *)
  | Loccon of identifier   (* Imported/Exported type label *)
  | Locval of identifier   (* Imported/Exported value label *)
  | Loccb of identifier    (* Con block l *)
  | Locc of identifier*int (* Code block l instruction i *)
  | Locd of identifier*int (* Data block l item i *)
;;

(*** Verification Errors *)

type verify_error =
    Undefined_label of identifier
  | Undefined_var of identifier
  | Undefined_reg of reg
  | Redefined_label of identifier
  | Kindleq of kind*kind
  | Kindeq of kind*kind
  | Kindmeet of kind*kind
  | Kindjoin of kind*kind
  | Conwf of con*string
  | Neqcon of con*con
  | Nleqcon of con*con
  | Msabsentreg of reg*machine_state*machine_state
  | Msnleq of reg*con*con
  | Conmeet of con*con
  | Conjoin of con*con
  | Msmeet of machine_state*machine_state
  | Msjoin of machine_state*machine_state
  | FPstackeq of fpstack*fpstack
  | FPstackleq of fpstack*fpstack
  | BadUnroll of con  (* con is not unrollable *)
  | Unknown_size of con
  | Bad_offset of int32 (* -n means n bytes before a field/slot
                           +n means n bytes after last valid field/slot *)
  | Bad_depth of int
  | Not_tail of con*con
  | Readonly
  | Stack_write_alignment
  | Coercion of con*string*coercion
  | No_stack_type
  | Genop of string*genop
  | Both_mem of genop*genop
  | Inst_form of string
  | Data_form of string
  | Fallsthru
  | Cyclic_imports of int_ref
  | Doesnt_export
  | Ndisjoint_exports of identifier
  | Multiple_exports
  | Con_def_nleq
  | Intt_nleq of string * identifier 
  | Label_requires_type of identifier
  | Label_bad_mode of identifier * mode
  | Fallsthru_unlabelled
  | Backward_branch of identifier
  | Bad_Name of identifier
  | Var_defined of identifier
(* ---- LX ---- *)
  | Negative_var of identifier
  | Kindwf of kind * string
(* -- end LX -- *)
;;

exception Talfail;;   (* The verifier could not recover from errors *)

(* stuff that changes infrequently *)
type ctxt0 = 
    { ge : ctxt -> verify_error -> unit;
      (* ctxt |- l : k  -- for constructor labels *)
      cheap : (identifier,kind) Dict.dict;
      (* ctxt |- l : c  -- for value labels *)
      vheap : (identifier,con option * mode) Dict.dict;
      (* ctxt |- l = c : k -- for transparent constructor labels *)
      lenv : (identifier,int_con_def) Dict.dict
    } 
and ctxt = {
    ctxt0 : ctxt0;
    loc : tal_loc;
    mode : mode; (* Abs when outside a template. *)
    vc : string;
    (* ctxt |- a : k  -- for constructor variables *)
    delta : (identifier,kind) Dict.dict;
    (* ctxt |- r : c *)
    gamma : machine_state;
    (* Abbreviations *)
    abbrevs : (identifier,con) Dict.dict;
    (* Local renaming of user-bound type variables *)
    locals : (identifier,con) Dict.dict;
    (* Boolean proposition *)
    prop : con;

    (* -- LX -- *)
    (* For making sure all kind variables are positive *)
    kindpolarity : bool;
    (* ctxt |- j kind -- for kind variables *)
    kinds : (identifier,bool) Dict.dict;
    (* kind abbreviations -- for recursive kinds *)
    kindabbrevs : (identifier,kind) Dict.dict
    (* -- end LX -- *)
  }
;;

(* A context is well formed if:
 *   con is well formed
 *   All l in Dom(psi):    con.labs |- psi(l) : Ktype
 *   All a in Dom(kappa):  con      |- kappa(a) : k
 *   All l in Dom(lenv):   con.labs |- lenv(l) : k
 *   All r in Dom(gamma):  con      |- gamma(r) : K4byte
 *   |- prop : Kbool 
 *
 * NOTE: The tal terms which contain constructors may have free variables
 *       in them.  These free variables belong to Dom(srcdelta) and
 *       Dom(srcdelta) is disjoint from Dom(delta).  Dom(kappa) is a subset
 *       of Dom(srcdelta).  Thus, before finding the kind of a constructor
 *       in a tal term, apply the substitution kappa to map variables in
 *       Dom(srcdelta) to Dom(delta). 
 *)

exception Talverify of ctxt * verify_error;;
exception Talfail;;

let empty_ctxt0 =
  { ge=(fun ctxt ve -> raise (Talverify (ctxt,ve)));
    cheap=Dict.empty id_compare; 
    vheap=Dict.empty id_compare;
    lenv=Dict.empty id_compare
  }
let empty_ctxt =
  { ctxt0=empty_ctxt0;
    loc=Loctop; 
    vc=""; 
    mode = Abs;
    delta=Dict.empty id_compare; 
    gamma=ms_empty;
    abbrevs=Dict.empty id_compare;
    locals=Dict.empty id_compare;
    prop=pctrue;
    (* -- LX -- *) 
    kindpolarity = true;
    kinds=Dict.empty id_compare;
    kindabbrevs= Dict.empty id_compare
    (* -- end LX -- *)
  }
;;

let get_loc ctxt = ctxt.loc;;
let get_verify_ctxt ctxt = ctxt.vc;;
let generate_error ctxt ve = ctxt.ctxt0.ge ctxt ve;;
let get_var_map ctxt = ctxt.delta
let get_value_labels ctxt = ctxt.ctxt0.vheap;;

let get_label_kind ctxt l =
  try 
    Dict.lookup ctxt.ctxt0.cheap l
  with Dict.Absent -> generate_error ctxt (Undefined_label l); k4byte
;;

let get_variable_kind ctxt v =
  try Dict.lookup ctxt.delta v
  with Dict.Absent -> generate_error ctxt (Undefined_var v); k4byte
;;

let get_abbrevs ctxt = ctxt.abbrevs;;

let get_locals ctxt = ctxt.locals;;

let get_label_def ctxt l =
  try Dict.lookup ctxt.ctxt0.lenv l
  with Dict.Absent -> generate_error ctxt (Undefined_label l); AbsCon
;;

let get_label_con ctxt l =
  try
    (match fst (Dict.lookup ctxt.ctxt0.vheap l) with
      Some c -> c
    | None -> ctypeof l
(* generate_error ctxt (Label_requires_type l); chptr [] None None *)
   ) 
  with Dict.Absent ->
    generate_error ctxt (Undefined_label l); chptr [] None None
;;

let get_label_con_opt ctxt l =
  try fst (Dict.lookup ctxt.ctxt0.vheap l)
  with Dict.Absent ->
    generate_error ctxt (Undefined_label l); Some (chptr [] None None)
;;

let get_label_con_mode ctxt l =
  try 
    (match Dict.lookup ctxt.ctxt0.vheap l with
    | (Some c,m) -> (c,m)
    | (_,m) -> (ctypeof l,m))
	(* generate_error ctxt (Label_requires_type l); 
	(chptr [] None None,m) *) 
  with Dict.Absent ->
    (generate_error ctxt (Undefined_label l); (chptr [] None None,Abs));;

let get_label_con_opt_mode ctxt l =
  try Dict.lookup ctxt.ctxt0.vheap l
  with Dict.Absent ->
    generate_error ctxt (Undefined_label l); (Some (chptr [] None None),Abs);;

let get_reg_con ctxt r =
  try ms_get_reg ctxt.gamma r
  with Dict.Absent -> generate_error ctxt (Undefined_reg r); cbyte4
;;

let get_machine_state ctxt = ctxt.gamma;;

let get_fpstack ctxt = ms_get_fpstack ctxt.gamma

let get_cc ctxt = ms_get_cc ctxt.gamma;;

let get_cap ctxt = ms_get_cap ctxt.gamma;;

let get_prop ctxt = ctxt.prop;;

let get_mode ctxt = ctxt.mode;;
let set_mode ctxt m = { ctxt with mode = m }

let set_loc ctxt loc =   { ctxt with loc = loc }
;;

let set_verify_ctxt ctxt vc =  { ctxt with vc = vc }
;;

let error_handler ctxt ge' =
  let ctxt0' = ctxt.ctxt0 in { ctxt with ctxt0 = {ctxt0' with ge = ge' }}
;;

let add_con ctxt l k =
  let ctxt0 = ctxt.ctxt0 in
  if Dict.member ctxt0.cheap l then generate_error ctxt (Redefined_label l);
  let cheap = Dict.insert ctxt0.cheap l k in
  { ctxt with ctxt0 = { ctxt0 with cheap = cheap } }
;;

let add_con_def ctxt l cd =
  let ctxt0 = ctxt.ctxt0 in
  let lenv = Dict.insert ctxt0.lenv l cd in
  { ctxt with ctxt0 =  { ctxt0 with lenv = lenv } }
;;

(* v must be fresh! *)
let add_var ctxt v k =
  let delta = Dict.insert ctxt.delta v k in
  let abbrevs = Dict.delete ctxt.abbrevs v in
  { ctxt with delta = delta; abbrevs = abbrevs }
;;

let clear_vars ctxt =
  { ctxt with delta=Dict.empty id_compare }
;;

let set_abbrevs ctxt abbrevs = { ctxt with abbrevs = abbrevs }
;;


(* -- LX -- *)
(* Refine the context for LX, by adding a new abbrev  --
 *   
 * If remove is true, then the identifier v should already be 
 * an abstract variable, and should be removed.
 *
 * In either case, the new abbrev may not contain any references to 
 * old abbrevs, and it is substituted through the ctxt and the old 
 * abbrevs.
 *
 *)
let add_abbrev_map ctxt subst v c remove = 
   try 
      begin
	 let abbrevs = ctxt.abbrevs in 
	 (* Make sure freevars of c do not mention any current abbrevs *)
	 Dict.app_dict (fun v _ -> 
	    match c.freevars with 
		Some (kfvs, cfvs) -> if        
		   Set.member cfvs v then
		   failwith "BUG: new abbrev should not contain references to prev abbrevs"
	     | None -> ())
	    abbrevs;
	 (* remove v from the context *)
	 let delta = if remove then Dict.delete_present ctxt.delta v else ctxt.delta in
	 (* subst new abbrev thru old abbrevs *)
	 let abbrevs = Dict.map_dict subst abbrevs in 
	 (* add new abbrev *)
	 let abbrevs = Dict.insert abbrevs v c in
	 (* subst new abbrev thru rest of ctxt *)
	 let gamma = ms_map subst ctxt.gamma in       
	 { ctxt with abbrevs = abbrevs; delta=delta; gamma = gamma }
      end
with 
Dict.Absent -> raise (Talverify(ctxt,(Undefined_var v )))

let set_kindabbrevs ctxt abbrevs =
   { ctxt with kindabbrevs = abbrevs }
;;

let add_kindabbrev ctxt v c =
   let kinds = Dict.delete ctxt.kinds v in
   let abbrevs = Dict.insert ctxt.kindabbrevs v c in
   let _ = c.kabbrev <- Some v in 
   { ctxt with kindabbrevs = abbrevs; kinds = kinds}
;;

let add_kindabbrevs ctxt abbrevs' = 
  Dict.fold_dict (fun v c ctxt -> add_kindabbrev ctxt v c) abbrevs' ctxt
;;

(* -- end LX -- *)
  
let add_abbrev ctxt v c =
   if Dict.member ctxt.delta v then 
      failwith ("BUG: adding abbrev " ^ (Identifier.id_to_string v) ^ " already in context")
   else  
      let abbrevs = Dict.insert ctxt.abbrevs v c in
      let _ = c.abbrev <- Some v in 
      { ctxt with abbrevs = abbrevs}
	 ;;

let add_abbrevs ctxt abbrevs' = 
  Dict.fold_dict (fun v c ctxt -> add_abbrev ctxt v c) abbrevs' ctxt
;;

let set_locals ctxt locals = { ctxt with locals=locals };;

let add_local_subst subst ctxt id1 con = 
   (* substitute through all of the other local substitutions *)
   let locals = Dict.map_dict (fun old -> subst con id1 old) ctxt.locals in 
   (* add the new substitution to the dictionary *)
   let locals = Dict.insert locals id1 con in
   { ctxt with locals=locals }
;;

let add_val ctxt l c m =
  let ctxt0 = ctxt.ctxt0 in
  if Dict.member ctxt0.vheap l then generate_error ctxt (Redefined_label l);
  let vheap = Dict.insert ctxt0.vheap l (c,m) in
  { ctxt with ctxt0 = {ctxt0 with vheap=vheap}}
;;

let set_val ctxt l c m =
  let ctxt0 = ctxt.ctxt0 in
  let vheap = Dict.insert ctxt0.vheap l (c,m) in
  { ctxt with ctxt0 = {ctxt0 with vheap=vheap}}
;;

let add_reg ctxt r c =
  let gamma = ms_set_reg ctxt.gamma r c in
  { ctxt with gamma=gamma }
;;

let set_machine_state ctxt gamma =
   { ctxt with gamma = gamma }
;;

let set_fpstack ctxt fps = { ctxt with gamma=ms_set_fpstack ctxt.gamma fps };;
let set_cc ctxt cc = { ctxt with gamma=ms_set_cc ctxt.gamma cc };;
let restore_cc ctxt = ms_restore_cc ctxt.gamma;;
let set_cap ctxt con = { ctxt with gamma=ms_set_cap ctxt.gamma con };;

let add_conjunct ctxt c = 
  let p = ctxt.prop in
  match c.rcon, p.rcon with 
    Cprim PCtrue,_ -> ctxt
  | _,Cprim PCtrue -> { ctxt with prop=c }
  | _,_ -> { ctxt with prop=cand [c;p] }
;;
let set_prop ctxt c = { ctxt with prop=c };;


(* NB:  these really only make a join when we're not processing anything
 * but top-level definitions. I need them in the verifier only to glue
 * together the cheap, vheap, etc. from imports.  *)
let ctxt0_join c1 c2 = 
  { ge = c1.ge;
    cheap = Dict.update c1.cheap c2.cheap;
    vheap = Dict.update c1.vheap c2.vheap;
    lenv = Dict.update c1.lenv c2.lenv
  } 
;;

let ctxt_join c1 c2 = 
  { ctxt0 = ctxt0_join c1.ctxt0 c2.ctxt0;
    loc = c1.loc;
    vc = c1.vc;
    mode = if c1.mode != c2.mode 
    then failwith "Compiler bug: modes must match on ctxt join." 
    else c1.mode;
    delta = Dict.update c1.delta c2.delta;
    gamma = c2.gamma;
    abbrevs = Dict.update c1.abbrevs c2.abbrevs;
    locals = Dict.update c1.locals c2.locals;
    prop = c2.prop;
    (* -- LX -- *)
    kindpolarity = 
     if c1.kindpolarity != c2.kindpolarity
     then failwith "Compiler bug: kindpolarities must match on ctxt join"
     else c1.kindpolarity;
    kinds = Dict.update c1.kinds c2.kinds;
    kindabbrevs = Dict.update c1.kindabbrevs c2.kindabbrevs
    (* -- end LX -- *)
  } 
;;      

(* ---- LX ---- *)
let get_kindvars ctxt = ctxt.kinds;;
let get_kindabbrevs ctxt = ctxt.kindabbrevs;;

let add_kind ctxt i = { ctxt with kinds= Dict.insert ctxt.kinds i ctxt.kindpolarity;
                                  kindabbrevs = Dict.delete ctxt.kindabbrevs i  }
let valid_kindvar ctxt i =
   try if Dict.lookup ctxt.kinds i = ctxt.kindpolarity then ()
	 else generate_error ctxt (Negative_var i); ()
   with Dict.Absent -> generate_error ctxt (Undefined_var i); ()
let check_kindvar ctxt i = 
   try if Dict.lookup ctxt.kinds i = ctxt.kindpolarity then true
	 else false
   with Dict.Absent -> false 
      
let reverse_polarity ctxt = 
   { ctxt with kindpolarity = not ctxt.kindpolarity }

let get_polarity ctxt = ctxt.kindpolarity

(* -- end LX -- *)



(* EOF: talctxt.ml *)


