(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* talverify.mli
 * TAL Verifier
 *
 * Checks operands, instructions, interfaces, and implementations for well
 * formedness.
 *)

open Utilities;;
open Numtypes;;
open Identifier;;
open Tal;;
open Talctxt;;
open Talcon;;

let unimplemented s = failwith ("Unimplemented: "^s);;
(*let debug s = (Format.print_string s; Format.print_newline (); ());; *)
let debug s = () ;;

open Talpp
open Format

(* Some types used by the verifier *)
let tag_cons =
  Array.init 20 (fun i -> csing (pcint (int_to_int32 i)))
;; 

(************************************************************************)
(* Verification Flags                                                   *)
(************************************************************************)

(* Dave: certain instructions are only available on pentium pro architecture.
 * We currently verify the pentium pro.
 *)
let pentium_pro () = true

(************************************************************************)
(* Coercions                                                            *)
(************************************************************************)

let raw a : 'a coerce = (a,[]) (* raw: 'a -> 'a coerce *)
let coerce (a,clist : 'a coerce) c : 'a coerce = (a, c:: clist)
   (* coerce: 'a coerce -> coercion -> 'a coerce  *)
let get_raw (raw, clist : 'a coerce) = raw

(* Is c2 an instance of c1 as sums *)
let check_to_sum error ctxt c1 c2 =
  let err () = error "tosum: bad index" in
  let rec aux c1 c2 =
    let c1 = whnorm ctxt c1 and c2 = whnorm ctxt c2 in
    match c1.rcon,c2.rcon with
      Cprod cs1,Cprod cs2 ->
	(try List.iter2 aux cs1 cs2
	with Invalid_argument _ ->
	  error "tosum: products of different lengths")
    | Csum cs1,Csum cs2 ->
	let rec find1 cs1 cs2 =
	  match cs2 with
	    [] -> ()
	  | c2::cs2 -> find2 cs1 (sum_index err c2) c2 cs2
	and find2 cs1 i2 c2 cs2 =
	  match cs1 with
	    [] -> error "tosum: tag not in sum"
	  | c1::cs1 ->
	      let i1 = sum_index err c1 in
	      if i1=i2 then (aux c1 c2; find1 cs1 cs2)
	      else if i1<i2 then find2 cs1 i2 c2 cs2
	      else error "tosum: tag not in sum" in
	find1 cs1 cs2
    | Csum cs1,_ ->
	let i2 = sum_index err c2 in
	let rec find cs1 =
	  match cs1 with
	    [] -> error "tosum: tag not in sum"
	  | c1::cs1 ->
	      let i1 = sum_index err c1 in
	      if i1=i2 then aux c1 c2
	      else if i1<i2 then find cs1
	      else error "tosum: tag not in sum" in
	find cs1
    | _,_ -> leqcon ctxt c2 c1 in
  let (is1,c1o,tco1) = dchptr ctxt c1 in
  match c2.rcon with
    Csing csing ->
      let i = dint ctxt csing in
      if not (List.mem i is1) then error "tosum: tag not in sum";
  | Chptr (is2,c2o,tco2) ->
      if not (List.for_all (fun i -> List.mem i is1) is2) then
	error "tosum: some tag not in sum";
      (match c1o,c2o with
	_,None -> ()
      | None,Some _ -> error "tosum: sum has no pointer"
      | Some c1,Some c2 -> aux c1 c2);
      (match tco1,tco2 with
	None,_ -> ()
      | Some _,None -> error "tosum: not tag for type"
      | Some _,Some _ ->
	  leqcon ctxt (chptr [] None tco2) (chptr [] None tco1))
  | _ -> error "tosum: not a sum instance"
;;

let cdcon_to_con error ctxt a = 
  let rec cdr c n =
    if n=0 then c
    else 
      let (hd,tl) = dcons ctxt c in cdr tl (n-1) in
  let rec slice c n bottom =
    if n=0 then bottom
    else
      let (hd,tl) = dcons ctxt c in ccons hd (slice tl (n-1) bottom) in
  match a with
    Con  c         -> c
  | AReg r         -> get_reg_con ctxt r
  | StackTail(r,i) -> cdr (dsptr ctxt (get_reg_con ctxt r)) i
  | StackSlice(r,i,j,bottom) -> 
      let csp = dsptr ctxt (get_reg_con ctxt Esp) in
      slice (cdr csp i) (j-i) bottom

(*************************************************************************
 * Dave: the result of local_subst substitutes the right fresh variables for
 * local user-defined variables.  Apply to all constructors that appear in the 
 * TAL text before calling check.
 *)
let local_subst ctxt = 
  let locals = get_locals ctxt in
  if Dict.is_empty locals then (fun c -> c)
  else (fun c -> substs (Dict.empty id_compare,locals) c)

(* In context ctxt return the type constructor resulting from coercion of con
 * by coercion.
 * con must be normalized.
 * 
 * Dave: subst_f substitutes the right fresh variables for user-defined
 * variables.  Apply to all constructors be calling check.
 *)
let coercion_con ctxt coercion con subst_f =
  let error s = generate_error ctxt (Coercion (con,s,coercion)) in
  match coercion with
    Pack (c1,c2) ->
      let ctxt = set_verify_ctxt ctxt "pack coercion" in 
      let c1 = subst_f c1 in
      let c2 = subst_f c2 in
      let (k1,c1) = check ctxt c1
      and (k2,c2) = check_whnorm ctxt c2 in
      let (v,k,c1',c2') = dexist ctxt c2 in
      kindleq ctxt k1 k;
      prove ctxt (capp (clam v k c1') c1);
      (* let c = subst c1 v c2' in *)
      leqcon ctxt con (capp (clam v k c2') c1);
      c2
  | Tapp a ->
      debug "coercion_con: doing Tapp";
      let ctxt = set_verify_ctxt ctxt "tapp coercion" in 
      let c = cdcon_to_con error ctxt a in
      let c = subst_f c in
      let (k,c) = check_whnorm ctxt c in
      (* delve into the constructor to find a function to
	 apply the coercion to.  For now, only allow delving into
	 chptrs to products *)
      let rec aux con =
	let wf_error s = 
	  generate_error ctxt (Conwf (con,s)); 
	  raise Talfail in
	match (whnorm ctxt con).rcon with
	  Cforall _ ->
	    let (v1,k1,c1) = dforall ctxt con in
	    kindleq ctxt k k1;
	    defcon(Capp(defcon(Clam(v1,k1,c1)),c))
	| Chptr _ ->
	    let (is,co,tco) = dchptr ctxt con in 
	    (* XXX check acceptability of is and tco fields *)
	    (match co with
	      Some con -> 
		let cs = dprod ctxt con in
		(match cs with
		  [fcon] -> 
		    let (f,v) = dfield ctxt fcon in
		    if v <> Read then
		      (error "prod field not readonly";
		       raise Talfail)
		    else
		      let f' = aux f in
		      let fcon' = defcon(Cfield (f',v)) in
		      let co' = Some (defcon (Cprod [fcon'])) in
		      defcon(Chptr(is,co',tco))
		| _ -> error "prod has > 1 field"; raise Talfail)
	    | None -> wf_error "expecting Chptr con")
	| _ -> wf_error "expecting Cforall or Chptr" in
      aux con
  | Roll c ->
      let ctxt  = set_verify_ctxt ctxt "roll coercion" in 
      let c = subst_f c in
      let (k,c) = check_whnorm ctxt c in
      leqcon ctxt con (unroll_rec true ctxt c); c
  | Unroll ->
      let ctxt = set_verify_ctxt ctxt "unroll coercion" in 
      let c = whnorm ctxt con in
      unroll_rec false ctxt c
  | Tosum c ->
      let ctxt = set_verify_ctxt ctxt "tosum coercion" in 
      let c = subst_f c in
      let (k,c) = check_whnorm ctxt c in 
      check_to_sum error ctxt c con; c
  | Fromsum ->
      let ctxt = set_verify_ctxt ctxt "fromsum coercion" in 
      from_union ctxt con
  | RollTosum c ->
      let ctxt = set_verify_ctxt ctxt "rollsum coercion" in 
      let c = subst_f c in
      let (k,c) = check_whnorm ctxt c in
      let cunroll = whnorm ctxt (unroll_rec true ctxt c) in 
      check_to_sum error ctxt cunroll con; c
  | Toarray (offset,depth,celt) ->
      let ctxt = set_verify_ctxt ctxt "toarray coercion" in 
      let celt = subst_f celt in
      let kelt,celt = check ctxt celt in
      kindleq ctxt kelt kmem;
      let f1,c =
        match (whnorm ctxt con).rcon with
          Chptr (is,Some c,tco) -> (fun c -> chptr is (Some c) tco),c
        | Csptr c -> (fun c -> csptr c),c
	| _ -> error "toarray: not a pointer"; raise Talfail in
      let c =  whnorm ctxt c in
      let f2,c = get_mem_from_offset ctxt c offset in
      let mem = dprod ctxt c in
      (* Given product of cons in memory and a con, 
	 match the element type to groups of cons in memory.
	 Returns the number of array elements *)
      let rec list_divide l i  = 
	 if i = 0 then 
	    [], l 
	 else match l with 
	    [] ->( error "toarray: array not divisible by element type"; raise Talfail)
	  | hd::tl -> let before, after = list_divide tl (i-1) 
	  in hd::before, after in
	    
      let match_con mem celt = 
	 match celt.rcon with 
	    Cprod cs -> 
	       let len = List.length cs in 
	       let rec loop acc mem cs = 
		  match mem with 
		     [] -> acc
		   | _ -> 
		  	let before, after = list_divide mem len in 
		  	List.iter2 (leqcon ctxt) before cs;
		  	loop (acc+1) after cs in 
	       loop 0 mem cs
	  | _ -> 
	       List.iter (fun c2 -> leqcon ctxt c2 celt) mem;
	       List.length mem in
      let num = match_con mem celt in 
      f1 (f2 (carray (pcint (int_to_int32 num)) celt))
  | Slot (i,s) ->
      let c = dsptr ctxt con in
      csptr (write_stack_offset ctxt c i (pcjunk s))
  | Subsume c ->
      let ctxt = set_verify_ctxt ctxt "subsume coercion" in 
      let c = subst_f c in
      let (_,c) = check ctxt c in
      leqcon ctxt con c;
      c
  | Forgetname ->
      let ctxt = set_verify_ctxt ctxt "forget name coercion" in
      let x = dvar ctxt (dname ctxt con) in
      (match (get_name ctxt x) with
	(MayAlias,xcon) -> xcon
      |	(_,_) -> error "forgetname: name is unique"; raise Talfail)
  | Prove ->
      let ctxt = set_verify_ctxt ctxt "proving precondition" in
      let (c1,c2) = dif ctxt con in
      prove ctxt c1;
      c2
  | VirtLabel(_) -> error "VirtLabel found"; raise Talfail
;;

(* A hack for avoiding multiple substitutions -- when we see a bunch of
 * Tapps at the beginning of a coercion, we do them all at once.  Could
 * be generalized to support Tapps in the middle of coercions but this
 * never happens in practice.
 *
 * Dave: Apply subst_f to all constructors to avoid capture of locally
 * defined names.
 *)
let rec match_coercion ctxt c0 coercions subst subst_f = 
 let substs s = substs (Dict.empty id_compare, s) in 
 match c0.rcon,coercions with
   Cforall(x,k1,c1),(Tapp c2)::rest -> 
     debug ("match_coercion: doing Tapp subst for "^(id_to_string x));
     let error s = generate_error ctxt (Coercion (c0,s, (Tapp c2))) in
     let c2 = subst_f (cdcon_to_con error ctxt c2) in
     let (k2,c2) = check ctxt c2 in
     kindleq ctxt k2 k1;
     (match c2.rcon with
       Cvar y when (id_compare x y = 0) -> 
	 match_coercion ctxt c1 rest subst subst_f
     | _ -> 
	 match_coercion ctxt c1 rest (Dict.insert subst x c2) subst_f)
  | _,[] -> if Dict.is_empty subst then c0 else substs subst c0
  | _,_::_ -> 
       List.fold_left (fun c co -> coercion_con ctxt co c subst_f) 
	  (if Dict.is_empty subst then c0 else substs subst c0) coercions
;;

let empty_subst = Dict.empty id_compare
;;
     
let coerce_con f ctxt (raw,clist) = 
  let c = whnorm ctxt (f ctxt raw) in
  match_coercion ctxt c (List.rev clist) empty_subst (local_subst ctxt)
;;

(************************************************************************)
(* Operands                                                             *)
(************************************************************************)

(* inst_mode = how this instruction treats this address
   dest_mode = this label is in a template (Relative) or not (Absolute)
*)
let check_mode ctxt inst_mode lbl lbl_mode =
    (match get_mode ctxt,inst_mode,lbl_mode with
    | Abs,_,Abs -> () 
    | Rel,Rel,Rel -> ()
    | Rel,Abs,Abs -> ()
    | _ -> generate_error ctxt (Label_bad_mode (lbl,lbl_mode)); ())

(* get the current stack type *)
let current_stack_con ctxt = dsptr ctxt (get_reg_con ctxt Esp) 
;;

(* is con a valid stack type if so return the mutator function *)
let valid_stack_con ctxt con =
  verify_stack_tail ctxt con (current_stack_con ctxt)
;;

(* ctxt |- coerce(reg) : c *)
let coerce_reg_con = coerce_con get_reg_con;;

(* ctxt |- coerce(label) : c *)
let coerce_label_con ctxt mode lbl = 
  let f ctxt lbl = 
    let (c,m) = get_label_con_mode ctxt lbl in
    check_mode ctxt mode lbl m; 
    c
  in 
  coerce_con f ctxt lbl ;;

let coerce_label_con_s ctxt mode (l,clist) =
  let (c,m) = get_label_con_mode ctxt l in
  check_mode ctxt mode l m; 
  List.fold_right 
    (fun c co -> coercion_con ctxt c co (local_subst ctxt)) clist c
;;

let coerce_label_con_whrcon ctxt mode lc =
  (whnorm ctxt (coerce_label_con ctxt mode lc)).rcon
;;

(*** The next two functions check genops for validity and readability and
      optionally for writeability returning the genop's type ***)

(* Verify a projection *)
(* PRns : not a projection off the stack
 * PRsp : a project from esp 
 * PRor : a project off the stack from a register other than esp
 * Arguments to verify_prj:
 *    c : type of pointer
 *   pr : prj_ctxt
 *    i : offset
 *  opt : (scale,reg) option
 *    w : if w then must be writeable
 *
 * Currently no array access on stack.
 *
 * Success and Failure of Projection:
 *
 * In order to determine when a project succeeds, we separate the
 * offset into the data structure into 2 parts, the constant part
 * and the non-constant part.  The constant part is i plus the
 * statically known constant part of (scale * reg). For example if
 * i is 4, scale is 1, and reg has type S(10+alpha+2*beta) then 
 * the constant part is 4 + 1*10.  The non-constant part is everything 
 * else (ie: alpha + 2*beta in the example).
 *
 * Projection from Products:
 * - succeeds if the non-constant part is empty (and constant part is in range)
 * - succeeds if the product contains a single array and projection from the
 * array succeeds
 * - doesn't succeed otherwise
 *
 * Projection from Arrays:
 * - if the array is inside a product, consider the constant part of the
 *   array (after this bullet, simply "the constant part") to be the 
 *   constant part of the product minus the size of the fields preceding 
 *   the array
 * - (the constant part) mod (array element size) gives the offset 
 *   into the array element
 * - array element size must be a statically known constant
 * - projection succeeds if the non-constant parts can be statically 
 *   divided by the element size and the result of statically dividing
 *   (non-constants + remaining constants) by element size can be 
 *   proven unsigned less than (array length)
 * - if the division does not succeed, we can't be sure the offset
 *   is properly aligned (even though it may be inside the bounds of the array)
 *   so we fail.
 *
 * For example: division of (remaining const part=4, non const part=8*alpha)
 * by an element size of 2 is possible statically, giving 
 * (remaining const part=2, non const part=4*alpha)
 * 
 * Notice that the soundness of this check relies on the fact that we
 * cannot allocate arrays of size larger than 2^32 - 1 bytes.  We are
 * checking that index <u length-of-array instead of:
 * (index *32 size-of-element) <u (length-of-array *32 size-of-element)
 * where *32 is a 32-bit multiplication.  The first check implies the
 * second since the constraint on array lengths and element sizes implies 
 * the 32-bit multiplication does not overflow.
 *)
exception Static_divide
let rec static_divide cs eltsize =
  if eltsize =$ i32_0 then failwith "compiler error: attempting division by 0"
  else if eltsize =$ i32_1 then cs 
  else
    match cs with
      [] -> []
    | c::cs ->
      	(match c.rcon with
	  Clog (Cmuls,[{rcon=Cprim (PCint i)};c']) ->
	    if mod32 i eltsize =$ i32_0 then
	      (cmuls (i /$ eltsize) c')::(static_divide cs eltsize)
	    else
	      raise Static_divide
      	| Clog (Cmulu,[{rcon=Cprim (PCint i)};c']) ->
	    if mod32 i eltsize =$ i32_0 then
	      (cmulu (i /$ eltsize) c')::(static_divide cs eltsize)
	    else
	      raise Static_divide
      	| _ -> raise Static_divide)

type prj_ctxt = PRns | PRsp | PRor;;

let verify_prj ctxt error c pr i opt w check_gpword =

  let verify_readwrite v =
    if w & v<>ReadWrite then
      generate_error ctxt Readonly
    else if (not w) & v<>Read & v<>ReadWrite then
      error "prj: field not readable" in

  let rec find_con c = 
    match c.rcon with
      Chptr (_::_,_,_) -> error "prj: pointer has tags"; raise Talfail
    | Chptr ([],None,_) -> error "prj: not pointer"; raise Talfail
    | Chptr ([],Some c,_) -> (c,true)
    | Csptr c ->
	(match opt with None -> ()
	| Some _ -> error "prj: variable proj from stack");
      	if pr = PRns then
 	  failwith "Talverify.verify_prj - should not be a stack pointer"
      	else if pr = PRor then
	  (let _ = valid_stack_con ctxt c in ());
      	(c,false)
    | Cname cname ->
	let x = dvar ctxt cname in
	let (_,xcon) = get_name ctxt x in 
	let xcon = whnorm ctxt xcon in
	(match xcon.rcon with
	  Chptr([],Some c,_) -> 
	    (match (whnorm ctxt c).rcon with 
	      Csum cs -> 
		if i =$ i32_0 then (ctagof cname,false)
		else (error "prj: value is a sum and index<>0"; 
		      raise Talfail)
	    | _ -> (c,true))
	| Cname cname' -> 
	    error "prj: named constructors may not be names themselves"; 
	    raise Talfail
	| _ -> find_con xcon)
    | _ -> error "prj: not a pointer or stack"; raise Talfail in

  (* split the operand type into (const_expr, non_const_expr list) *)
  let find_constants opt i =
    match opt with
      None -> (i,[])
    | Some (s,r) ->
	if r = Esp then (error "prj: scaled Esp disallowed"; raise Talfail);
	let reg_con = whnorm ctxt (get_reg_con ctxt r) in
	(match reg_con.rcon with
	  Csing c -> 
	    (match (con_kind ctxt c).rkind with
	      Kint -> 
		let c = 
		  match s with Byte1 -> c
		  | _ -> whnorm ctxt (cmuls (scale_to_int32 s) c) in
		let (const,non_consts) = split_arithmetic ctxt c in
		(i +$ const,non_consts)
	    | _ -> error "prj: variable prj not singleton int"; raise Talfail)
	| _ -> error "prj: variable prj not singleton"; raise Talfail) in

  let (c,mf) = find_con c in
  let (const,non_consts) = find_constants opt i in
  let (_,c,offset) = get_mem_offset_p ctxt (whnorm ctxt c) const in
  match (whnorm ctxt c).rcon with
    Cfield (c,v) -> 
      (match non_consts with
	[] ->
	  if offset =$ i32_0 then (verify_readwrite v; c)
	  else (generate_error ctxt (Bad_offset i); raise Talfail)
      |	_ -> error "prj: variable prj from field"; raise Talfail)
  | Carray (csize,celt) ->
      let eltsize = sizeof ctxt celt in
      (* offset_elt: offset into an element of the array, 
       * offset_array: constant offset to beginning of an element of the array
       *)
      let (offset_elt,offset_array) =
	let offset_elt = mod32 offset eltsize in
	(offset_elt, offset -$ offset_elt) in
      begin
	try 
	  let non_consts' = static_divide non_consts eltsize in
	  let offset_array' = offset_array /$ eltsize        in
	  let handler _ _ =
	    prove ctxt (cltu 
			  (cadd (pcint offset_array::non_consts))
			  (cmuls eltsize csize))             in
	  let ctxt' = error_handler ctxt handler             in
	    prove ctxt' (cltu (cadd (pcint offset_array'::non_consts')) csize)
	with
	  Static_divide -> 
	    error "prj: can't prove offset into array is aligned";raise Talfail
      end;
      (* find the resultant type *)
      let _,celt' = get_mem_offset ctxt (whnorm ctxt celt) offset_elt in
      (match (whnorm ctxt celt').rcon with
	Cfield (c,v) -> verify_readwrite v; c
      | _ -> error "prj: array element not a field"; raise Talfail)
  | _ ->
      if mf then
	error "prj: not a field"
      else
	if check_gpword then
	  kindleq ctxt (con_kind ctxt c) k4byte
	else
	  ();
      c
;;

(* ctxt |- genop : c *)
let genop_con' mode check_gpword w ctxt genop = 
  let error s = generate_error ctxt (Genop (s,genop)) in
  match genop with
    Immed i -> if w then generate_error ctxt Readonly; csing (pcint i)
  | Reg r -> get_reg_con ctxt r
  | Addr l -> 
      if w then generate_error ctxt Readonly; 
      let (c,m) = get_label_con_mode ctxt l in
      check_mode ctxt mode l m;
      c
    (* rc and lc must be either product types or else stack pointer types *)
  | Prjr ((r,_) as rc,i,opt) -> 
      let rc_con = whnorm ctxt (coerce_reg_con ctxt rc) in
      let pr = if r=Esp then PRsp else PRor in
      verify_prj ctxt error rc_con pr i opt w check_gpword
  | Prjl (lc,i,opt) -> 
      let lc_con = whnorm ctxt (coerce_label_con ctxt Abs lc) in
      verify_prj ctxt error lc_con PRns i opt w check_gpword
;;

let genop_con_abs = genop_con' Abs;;
let genop_con_rel = genop_con' Rel;;

(* checks that genop can fit in a general-purpose register *)
let genop_con w ctxt genop = genop_con_abs true w ctxt genop

let coerce_genop_con w = coerce_con (genop_con w);;

let coerce_genop_con_rel w = coerce_con (genop_con_rel true w)

(*** Can a con be written to a genop? ***)
(* Checks also that gop is vaild *)

(* Checks to see if a field in a tuple or the stack is writeable with
 * an object of a given type, and updates the type of that object.
 * ctxt: the current context.
 * error: string -> unit -- what to do when something goes wrong
 * c: the type of the object we're trying to write into
 * pr: indicates whether it can be on the stack or not.
 * i: the offset into the object where we're writing.
 * opt: (scale * reg) option -- possible array indexing operation
 * con: the type of the object we're writing
 * upd: a function to update the context, given the updated type
 * check_con: if true checks whether con <= c.  
 *
 * This is only called by Mov (via genop_write_at) & floating point ops
 *)
let rec writeable_prj ctxt error c pr i opt con upd check_con =
  match c.rcon with
    Chptr (_::_,_,_) -> error "prj: pointer has tags"; raise Talfail
  | Chptr ([],None,_) -> error "prj: not pointer"; raise Talfail
  | Chptr ([],Some c,tco) ->
      let size = sizeof ctxt con in
      let (f,cs,offset,b) = get_mem_offset_p_checked ctxt (whnorm ctxt c) i size in
      let c = match cs with c::rest -> c | [] -> error "prj: no cons"; raise Talfail in 
      (match (whnorm ctxt c).rcon with
	Cfield (c,v) ->
	  (match opt with
	    None when i32_0 =$ offset ->
	      if v = Read then
	    	generate_error ctxt Readonly;
	      if check_con then 
	    	begin
		  leqcon ctxt con c;
		  ctxt
	    	end
	      else 
	    	begin
		  if b then
		    upd ctxt (chptr [] (Some (f (cfield con ReadWrite))) tco)
		  else
		    (error "prj: size mismatch"; raise Talfail)
	    	end
	  | None -> generate_error ctxt (Bad_offset i); raise Talfail
	  | Some _ -> error "prj: variable proj from non-array"; raise Talfail)
      |	Carray (csize,celt) ->
	  (match opt with
	    None -> error "prj: non-variable proj from array"; raise Talfail
	  | Some (s,r') ->
	      if r' = Esp then 
		(error "prj: scaled Esp disallowed"; raise Talfail);
	      let _,celt' = 
		get_mem_offset ctxt (whnorm ctxt celt) offset in
	      (match (whnorm ctxt celt').rcon with
	    	Cfield (c,v) ->
		  if not (v=Write or v=ReadWrite) then
		    generate_error ctxt Readonly;
		  (* always check cons in arrays *)
		  leqcon ctxt con c;
		  ctxt 
	      | _ -> error "prj: array element not a field"; raise Talfail);
	      if not ((sizeof ctxt celt) = scale_to_int32 s) then
	    	(error "prj: scale not equal to element size"; raise Talfail);
	      let cindex = 
	    	match (get_reg_con ctxt r').rcon with
		  Csing i -> i
	    	| _ -> error "prj: index not singleton"; raise Talfail in
	      (* Static Bounds Check *)
	      prove ctxt (cltu cindex csize);
	      ctxt)
      | _ -> error "prj: not a field or array"; raise Talfail)
  | Cname namec ->
      let x = dvar ctxt namec in
      (match get_name ctxt x with
	(MayAlias,xcon) -> 
	 (* notice the call sets check_con to true ensuring that the
	  * type must be compatible. *)
	  let upd ctxt xcon' = change_name ctxt x (MayAlias,xcon') in
	  writeable_prj ctxt error xcon pr i opt con upd true 
      | (Unique,xcon) ->
	 (* notice the call sets check_con to false supporting a total
	  * change of type. *)
	  let upd ctxt xcon' = change_name ctxt x (Unique,xcon') in
	  writeable_prj ctxt error xcon pr i opt con upd false)
  | Csptr c ->
      (match opt with
	None -> ()
      |	Some _ -> error "prj: variable proj from stack"; raise Talfail);
      if pr = PRns then
 	failwith "Talverify.writeable_prj - should not be a stack pointer"
      else if pr = PRor then begin
	let f = valid_stack_con ctxt c in
	let c = write_stack_offset ctxt c i con in
	upd (add_reg ctxt Esp (csptr (f c))) (csptr c)
      end else begin
	let c = write_stack_offset ctxt c i con in
	add_reg ctxt Esp (csptr c)
      end
  | _ -> error "prj: not a pointer or stack"; raise Talfail
;;

let genop_write_at_check check_con ctxt gop con = 
  let error s = generate_error ctxt (Genop (s,gop)) in
  match gop with
    Reg r ->
      kindleq ctxt (con_kind ctxt con) k4byte;
      if r=Esp then begin
	match (whnorm ctxt con).rcon with
	  Csptr c ->
	    let _ = valid_stack_con ctxt c in ()
	| _ -> ()
      end;
      add_reg ctxt r con
  | Prjr ((r,cs) as rc,i,opt) ->
      let rc_con = whnorm ctxt (coerce_reg_con ctxt rc) in
      let pr = if r=Esp then PRsp else PRor in
      let upd =
 	if cs=[] then (fun ctxt c -> add_reg ctxt r c)
 	else (fun ctxt _ -> ctxt) in
      writeable_prj ctxt error rc_con pr i opt con upd check_con
  | Prjl (lc,i,opt) ->
      let lc_con = whnorm ctxt (coerce_label_con ctxt Abs lc) in
      writeable_prj ctxt error lc_con PRns i opt con (fun ctxt _ -> ctxt) true
  | _ -> generate_error ctxt Readonly; ctxt
;;

(* write the type in but if the object is in a heap-allocated object,
 * check that the type is compatible.
 *)
let genop_write_at = genop_write_at_check true;;

(* write the type in and do not check to see if the type is compatible.
 * Used by Nameobj to change the type of something to a named type.
 *)
let genop_overwrite = genop_write_at_check false;;

(*** Miscellaneous Validity Conditions ***)

(* Is operand register or memory? *)
let reg_or_mem_genop ctxt g =
  match g with
    Immed _ -> generate_error ctxt (Genop ("operand is an immediate",g))
  | Reg _ -> ()
  | Addr _ -> generate_error ctxt (Genop ("operand is a label",g))
  | Prjr (_,_,_) -> ()
  | Prjl (_,_,_) -> ()
;;

(* verify that only one of the operands is a path to memory *)
let is_mem gop =
  match gop with
    Immed _ -> false
  | Reg _ -> false
  | Addr _ -> false
  | Prjr (_,_,_) -> true
  | Prjl (_,_,_) -> true
;;

(* Is operand register or address? *)
let reg_or_addr ctxt g =
  match g with
    Immed _ | Prjr (_,_,_) | Prjl (_,_,_) ->
      generate_error ctxt (Genop ("operand is not register or address",g))
  | Reg _ | Addr _ -> ()
;;

let valid_binops ctxt gop1 gop2 = 
  if is_mem gop1 & is_mem gop2 then
    generate_error ctxt (Both_mem (gop1,gop2))
;;

let valid_cbinops ctxt gop1 cgop2 = valid_binops ctxt gop1 (get_raw cgop2);;

(************************************************************************)
(* Arithmetic Operations                                                *)
(************************************************************************)

(* apply ab to args of type Csing(d1) and Csing (d2) and write result to
   dest_genop; set condition code
*)
let update_arithbin ctxt ab dest_genop d1 d2 =
  let k1 = con_kind ctxt d1 in
  let k2 = con_kind ctxt d2 in
  let result_con,cc =
    match ab,k1.rkind,k2.rkind with
      Add,Kint,Kint -> csing (cadd [d1; d2]), CCnoinfo
    | And,Kbool,Kbool -> csing (cand [d1;d2]), CCtest(csing d1,csing d2)
    | Sub,Kint,Kint -> csing (csub d1 d2), CCcmp(csing d1, csing d2)  
    | _ -> cbyte4, CCnoinfo
  in
  set_cc (genop_write_at ctxt dest_genop result_con) cc

(* The following are similar except they do not update cc *)

let update_arithun ctxt un dest_genop d =
  let k = con_kind ctxt d in
  let result_con =
    match un,k.rkind with
      Dec,Kint -> csing (csub d (pcint i32_1))
    | Inc,Kint -> csing (cadd [pcint i32_1; d])
    | Neg,Kint -> csing (csub (pcint i32_0) d)
    | Not,Kbool -> csing (cnot d)
    | _ -> cbyte4
  in
  genop_write_at ctxt dest_genop result_con

let update_arithmd ctxt md d1 d2 =
  let result_con =
    match md with
      Div -> cbyte4
    | Idiv -> cbyte4
    | Imul1 -> 
	(match d1.rcon, d2.rcon with
	  Cprim (PCint i),_ -> csing (cmuls i d2)
	| _,Cprim (PCint i) -> csing (cmuls i d1)
	| _,_ -> cbyte4)
    | Mul -> 
	(match d1.rcon, d2.rcon with
	  Cprim (PCint i),_ -> csing (cmulu i d2)
	| _, Cprim (PCint i) -> csing (cmulu i d1)
	| _,_ -> cbyte4)
  in
  add_reg ctxt Eax result_con

let update_arithsr ctxt sr dest_genop d1 d2 =
  let result_con =
    match sr with
      Rcl | Rcr | Rol | Ror | Sar | Shr -> cbyte4
    | Sal | Shl -> 
	(match d2.rcon with
	  Cprim (PCint i) when unsigned_lt32 i (int_to_int32 31) -> 
	    let i = pow32 i32_2 i in
	    csing (cmulu i d1)
	| _ -> cbyte4)
  in
  genop_write_at ctxt dest_genop result_con

(************************************************************************)
(* Condition Code Stuff                                                 *)
(************************************************************************)

(* Replace type variable a in all types in the register state of ctxt with c *)
let refine_ms ctxt a c =
  let ms = get_machine_state ctxt in
  let ms = ms_map (subst c a) ms in
  set_machine_state ctxt ms
;;

(* Based on the comparsion of two values being used to tag types compute
 * contexts specialised for the success & failure of the jump.
 * Cases:
 *   a^(w|rw) compared to c^(r|rw) where ftv(c)=0 and kind(a)=kind(c):
 *     add a<=c to context
 * For now we don't have BQ so require  a^rw cmp c^rw and subst c for a
 *)
let process_tag_compare ctxt eq c1 v1 c2 v2 =
  if (v1=ReadWrite (*or v1=Write*)) & (v2=ReadWrite (*or v2=Read*)) then
    match (whnorm ctxt c1).rcon with
      Cvar a ->
	 if (is_closed c2) then begin
	  let k1 = get_variable_kind ctxt a
	  and k2 = con_kind ctxt c2 in
	  kindleq ctxt k2 k1;
	  if eq then
	    refine_ms ctxt a c2,ctxt
	  else
	    ctxt,refine_ms ctxt a c2
	end else
	  ctxt,ctxt
    | _ -> ctxt,ctxt
  else
    ctxt,ctxt
;;

(* In the branch on tags instructions we need to compare tag values according
 * to a condition, this function does this for us
 *)

let inst_form ctxt s = generate_error ctxt (Inst_form s);;

let interpret_condition ctxt cc =
  match cc with
    Above -> (>)
  | AboveEq -> (>=)
  | Below -> (<)
  | BelowEq -> (<=)
  | Eq -> (=)
  | NotEq -> (!=)
  | _ ->
      inst_form ctxt "tag comparison: condition code is invalid"; raise Talfail
;;

let split_condition_tag ctxt cc i st co tco =
  let f = interpret_condition ctxt cc in
  let condition x = f x i in
  let rec split l matches not_matches =
    match l with
      [] -> (List.rev matches,List.rev not_matches)
    | h::t ->
   	if condition h then split t (h::matches) not_matches
   	else split t matches (h::not_matches) in
  let (tag_matches,tag_not_matches) = split st [] [] in
  let (svt_matches,svt_not_matches) = 
    if i<min_pointer_integer then
      if condition min_pointer_integer then (co,None) else (None,co)
    else if is_non_pointer_integer i then
      (match cc with
   	Eq -> (None,co)
      |	NotEq -> (co,None)
      |	_ -> (co,co))
    else (co,co) in
  let side tags svts tco =
    let mt_list l = match l with []   -> true | _ -> false in
    let mt_opt  o = match o with None -> true | _ -> false in
    if(mt_list tags && mt_opt svts && mt_opt tco)
    then None
    else Some(chptr tags svts tco) in
  (side tag_matches svt_matches tco, side tag_not_matches svt_not_matches tco)
;;
    
let split_sum ctxt i cc c =
  let cs = dsum ctxt c in
  let g = interpret_condition ctxt cc in
  let condition c =
    g (sum_index (fun () -> inst_form ctxt "split_sum: bad sum tag") c) i in
  let rec split l matches not_matches =
    match l with
      [] -> (List.rev matches,List.rev not_matches)
    | c::t ->
 	if condition c then split t (c::matches) not_matches
    	else split t matches (c::not_matches) in
  let cs1,cs2 = split cs [] [] in
  (csum cs1,csum cs2)
;;

(* Based on the current register state's condition code information and a
 * condition code to branch on produce two new contexts specialised for the
 * success & failure respectively of the jump
 *)

let process_conditional ctxt cc c1 c2 =
  match cc with
    Above -> add_conjunct ctxt (cltu c2 c1), add_conjunct ctxt (clteu c1 c2)
  | AboveEq -> add_conjunct ctxt (clteu c2 c1), add_conjunct ctxt (cltu c1 c2)
  | Below -> add_conjunct ctxt (cltu c1 c2), add_conjunct ctxt (clteu c2 c1)
  | BelowEq -> add_conjunct ctxt (clteu c1 c2), add_conjunct ctxt (cltu c2 c1)
  | Eq -> add_conjunct ctxt (ceq c1 c2), add_conjunct ctxt (cne c1 c2)
  | Greater -> add_conjunct ctxt (clts c2 c1), add_conjunct ctxt (cltes c1 c2)
  | GreaterEq -> add_conjunct ctxt (cltes c2 c1),add_conjunct ctxt (clts c1 c2)
  | Less -> add_conjunct ctxt (clts c1 c2), add_conjunct ctxt (cltes c2 c1)
  | LessEq -> add_conjunct ctxt (clteu c2 c1), add_conjunct ctxt (cltu c1 c2)
  | NotEq -> add_conjunct ctxt (cne c1 c2), add_conjunct ctxt (ceq c1 c2)
  | _ -> ctxt, ctxt

exception EmptyLeftSide  of ctxt
exception EmptyRightSide of ctxt

let process_cc ctxt cc =
  match get_cc ctxt with
    CCnoinfo -> ctxt,ctxt
  | CCcmp (c1,c2) ->
      (match (whnorm ctxt c1).rcon,(whnorm ctxt c2).rcon with
	Csing c1,Csing c2 -> process_conditional ctxt cc c1 c2
      |	Chptr ([],Some _,Some (c1,v1)),Chptr ([],Some _,Some (c2,v2)) ->
	  if cc=Eq then
	    process_tag_compare ctxt true c1 v1 c2 v2
	  else if cc=NotEq then
	    process_tag_compare ctxt false c1 v1 c2 v2
	  else
	    ctxt,ctxt
      (* when c1 is Name(x) and x -> a tag or pointer in the current
       * capability, refine x's type according to the test.  This
       * effectively replaces the old btagi macro instruction.
       *)
      |	Cname c1,Csing csing ->
	  let i = dint ctxt csing in
	  let x = dvar ctxt c1 in
	  let (ai,xcon) = get_name ctxt x in
	  (match (whnorm ctxt xcon).rcon with
	    Chptr (is,co,tco) ->
	      let sides = split_condition_tag ctxt cc i is co tco in
	      (match sides with 
		None,None -> 
		 failwith "Talverify.process_cc - both sides are empty pointers"
	      |	None, Some c  -> 
		  raise (EmptyLeftSide  (change_name ctxt x (ai,c)))
	      |	Some c, None ->  
		  raise (EmptyRightSide (change_name ctxt x (ai,c)))
	      |	Some c1, Some c2 -> 
		  (change_name ctxt x (ai,c1),
		   change_name ctxt x (ai,c2)))
	  | _ -> (ctxt,ctxt))
      (* Here, we're comparing the tag of some object named x to an int.
       * The name x should map to a pointer to a sum.  The test refines 
       * the type of x.  This effectively replaces the old btagvar macro 
       * instruction.
       *)
      |	Ctagof c1,Csing sing -> 
	  let i = dint ctxt sing in
	  let x = dvar ctxt c1 in
	  let (ai,xcon) = get_name ctxt x in
	  (match (whnorm ctxt xcon).rcon with
	    Chptr ([],Some c,tco) ->
	      let nc1,nc2 = split_sum ctxt i cc c in
	      (change_name ctxt x (ai,chptr [] (Some nc1) tco),
	       change_name ctxt x (ai,chptr [] (Some nc2) tco))
	  | _ -> (ctxt,ctxt))
      |	_,_ -> ctxt,ctxt)
  | CCtest (_,_) -> ctxt,ctxt
;;

(************************************************************************)
(* Instructions                                                         *)
(************************************************************************)

exception Terminal_Jmp;;
exception Fall_Thru of ctxt * (con list);;

(*** Generic x86 Instructions ***)

(* Normal case: two byte4s, first is writeable, not both mem.
 * Special case: add to stack pointer type is "free"
 * Special case: sub from ESP is a "stack allocate"
 *
 *  ctxt |- genop1 : byte4  ctxt |- genop2 : byte4  
 *     ctxt |- (genop1,genop2) valid binops
 *     ctxt |- genop1 writeable
 *  ----------------------------------------------
 *  ctxt |- ArithBin ab genop1,genop2 : ctxt
 *
 *  ctxt |- r : Stackptr(c1::....::ci::c)
 *  ctxt |- n : byte4  
 *     ctxt |- (genop1,genop2) valid binops
 *     ctxt |- genop1 writeable
 *  ----------------------------------------------(n=sizeof(c1::...::ci))
 *  ctxt |- Add r,n : ctxt[r:Stacpkptr(c)]
 *
 *  ctxt |- ESP : Sptr(c)
 *  ---------------------------------------------------------
 *  ctxt |- Sub ESP,4n : ctxt[ESP:Sptr(junk1::...::junkn::c)]
 *)
let verify_ArithBin ctxt ab genop1 genop2 =
  if ab=Sub & genop1=(Reg Esp) then begin
    match genop2 with
      Immed n when n >=$ i32_0 ->
	let cstk = current_stack_con ctxt in
	let rec add_junk i cstk = 
	  if i =$ i32_0 then cstk 
	  else if i <$ i32_0 then 
	    (inst_form ctxt "stack adjustment not multiple of 4"; 
	     raise Talfail)
	  else add_junk (i -$ i32_4) (ccons pcjunk4 cstk) in
	let cstk = add_junk n cstk in
	add_reg ctxt Esp (csptr cstk)
    | _ -> inst_form ctxt "ESP - non-immed"; raise Talfail
  end else begin
    match ab,genop1,genop2 with
      Xor,Reg r1,Reg r2 when (compare_regs r1 r2)=0 ->
	add_reg ctxt r1 tag_cons.(0)
    | _,_,_ ->
    	valid_binops ctxt genop1 genop2;
	let c1 = whnorm ctxt (genop_con false ctxt genop1) in
    	match c1.rcon with
        (* A normal binop *)
	  Cprim (PCbytes Byte4) -> 
	    leqcon ctxt (genop_con false ctxt genop2) cbyte4;
	    genop_write_at ctxt genop1 cbyte4
	| Csing d1 ->
	    let c2 = whnorm ctxt (genop_con false ctxt genop2) in
	    (match c2.rcon with
	      Csing d2 ->
		update_arithbin ctxt ab genop1 d1 d2
	    | _ ->
	    	leqcon ctxt c2 cbyte4;
		genop_write_at ctxt genop1 cbyte4)
        (* adjusting a pointer onto the stack -- note that this provides
	 * "stack free" but it's more general in that we can calculate
	 * pointers into the middle of the stack from other pointers into
	 * the middle of the stack, as long as the sizes of the intervening
         * types are known (i.e., there's no intervening append). *)
    	| Csptr c ->
	    (match ab,genop1,genop2 with
	      (Add,Reg r,Immed i) -> 
	    	let c' = get_stack_tail ctxt i c in
            	add_reg ctxt r (csptr c')
	    | (_,_,_) ->
		inst_form ctxt "ArithBin: stack pointer"; raise Talfail)
	| Chptr([],Some c,None) ->
	    (match ab,genop1,genop2 with
	      (Add,Reg r,Immed i) ->
		let (_,c') = get_mem_from_offset ctxt c i in
		add_reg ctxt r (cptr c')
	    | (_,_,_) ->
		inst_form ctxt "ArithBin: heap pointer"; raise Talfail)
    	| _ -> inst_form ctxt "ArithBin"; raise Talfail
  end
;;

(* ctxt |- genop : byte4   ctxt |- genop writeable
 * -----------------------------------------------
 * ctxt |- ArithUn arithun genop : ctxt
 *)

let verify_ArithUn ctxt arithun genop =
  let c = whnorm ctxt (genop_con false ctxt genop) in
  match c.rcon with
    Csing d -> update_arithun ctxt arithun genop d
  | _ -> leqcon ctxt c cbyte4; genop_write_at ctxt genop cbyte4
;;

(* Div,IDiv: divides Eax by the operand and puts the quotient in Eax and 
 *  remainder in Edx. (Div is unsigned, IDiv is signed)
 * Mul,Imul1: multiples Eax by the operand and puts the 64-bit result in
 *  Eax and Edx.  (Mul is unsigned, Imul1 is signed)
 * In any of the cases, Eax must be a Byte4, the operand must be a Byte4,
 * and the resulting context maps Edx to Byte4.
 *
 * ctxt |- genop : byte4  ctxt |- Eax : byte4
 * -----------------------------------------------
 * ctxt |- ArithMd arithmd genop : ctxt[Eax,Edx:byte4]
 *
 * Dave: we now check to ensure no divide by 0.  Arguments to idiv 
 * must be provably unsigned greater than 0
 *)
let divide_by_zero_check ctxt arithmd c =
  match arithmd with
  | Imul1 | Mul -> ()
  | Div | Idiv -> 
      match c.rcon with
	Csing d1 -> 
	  prove (set_verify_ctxt ctxt "verifying no divide by zero") 
	    (cltu (pcint i32_0) d1)
      |	_ -> inst_form ctxt "divide-by-zero error"

let verify_ArithMD ctxt arithmd genop =
  let c1 = whnorm ctxt (get_reg_con ctxt Eax) in
  let c2 = whnorm ctxt (genop_con false ctxt genop) in
  divide_by_zero_check ctxt arithmd c2; 
  match c1.rcon,c2.rcon with
    Csing d1, Csing d2 -> update_arithmd ctxt arithmd d1 d2
  | _,_ ->
      leqcon ctxt c1 cbyte4;
      leqcon ctxt c2 cbyte4;
      add_reg (add_reg ctxt Eax cbyte4) Edx cbyte4
;;

(* Arithmetic shift operations. If iopt is None, then Ecx is used
 * as the shift amount.  Otherwise, the integer immediate is used
 * as the shift amount.
 *
 * ctxt |- genop : byte4  ctxt |- genop writeable
 * ctxt |- Ecx : byte4
 * ----------------------------------------------
 * ctxt |- ArithSR arithsr genop None : ctxt
 *
 * ctxt |- genop : byte4  ctxt |- genop writeable
 * ----------------------------------------------
 * ctxt |- ArithSR arithsr genop (Some i) : ctxt
 *
 * Dave: if iopt is outside the range 0...32 then this
 * likely indicates a compiler error (all bits are shifted
 * out of the operand).  Therefore TALx86 disallows such 
 * operands even though they are safe. 
 *)

let verify_ArithSR ctxt arithsr genop iopt =
  let range_check i =
    if i <=$ i32_0 or i >$ i32_32 then
	inst_form ctxt "ArithSR: immediate operand out of range" in
  let c = whnorm ctxt (genop_con true ctxt genop) in
  match c.rcon,iopt with
    Csing d1, Some i -> 
      range_check i;
      update_arithsr ctxt arithsr genop d1 (pcint i)
  | Csing d1, None ->
      let ecx_con = get_reg_con ctxt Ecx in
      (match (whnorm ctxt ecx_con).rcon with
	Csing d2 -> update_arithsr ctxt arithsr genop d1 d2
      |	_ -> leqcon ctxt ecx_con cbyte4; genop_write_at ctxt genop cbyte4)
  | _,Some i -> 
      leqcon ctxt c cbyte4;
      range_check i; 
      genop_write_at ctxt genop cbyte4
  | _,None -> 
      leqcon ctxt c cbyte4;
      leqcon ctxt (get_reg_con ctxt Ecx) cbyte4; 
      genop_write_at ctxt genop cbyte4
;;

(* Byte-swap 
 * ctxt |- reg : byte4
 * ------------------------
 * ctxt |- Bswap reg : ctxt
 *)

let verify_Bswap ctxt reg =
  restore_cc ctxt;
  leqcon ctxt (get_reg_con ctxt reg) cbyte4;
  add_reg ctxt reg cbyte4
;;

(* Call:
 * This is a little delicate.  The called label must have a code type which is
 * a supertype of the current code type with the return address pushed;
 * however, the return address type is garned from the code type of the called
 * label; however, the called label might ignore the return label and there may
 * be no return type.  In the later case we should treat call as a terminal
 * jump.
 *
 * For now: if the called label defines ESP then it must have a code type on
 * top, use it.  Otherwise its a terminal jump.
 * 
 * ctxt |- cgop : Ccode g1
 * ctxt |- g1(ESP) = Sptr (Ccode g2::c')
 * ctxt |- ctxt.gamma [ESP: Sptr (Ccode g2::(ctxt.gamma(ESP)))] <= g1
 * ------------------------------------------------------------------ESP in g1
 * ctxt |- Call cgop : ctxt[gamma: g2]
 *
 * ctxt |- cgop : Ccode g1
 * ctxt |- ctxt.gamma <= g1
 * --------------------------------ESP not in g1
 * ctxt |- Call cgop : terminal jump
 *)
let verify_Call ctxt ((gop,coercion) as cgop) =
  restore_cc ctxt;
  let c = coerce_genop_con_rel false ctxt cgop in 
  let g2 = dcodems ctxt c in
  let g1 = get_machine_state ctxt in
  (try
    let called_st = ms_get_reg g2 Esp in
    let c = whnorm ctxt (current_stack_con ctxt) in
    let c' = dsptr ctxt called_st in
    let (ca,c') = dcons ctxt c' in
    let gret = dcodems ctxt ca in
    leqcon ctxt c c';
    machine_state_leq ctxt (ms_set_reg g1 Esp called_st) g2;
    set_machine_state ctxt gret
  with Dict.Absent ->
    machine_state_leq ctxt g1 g2; raise Terminal_Jmp)
;;

(* Conditional move: it must be the case that if cgop : c, then
 * r : c.  This ensures that after the conditional move, r : c.
 *
 * ctxt |- r : c1    ctxt |- cgop : c2    ctxt |- c2<=c1
 * -----------------------------------------------------
 * ctxt |- Cmovcc r,cgop : ctxt
 *)

let verify_Cmovcc ctxt cond r cgop = 
  restore_cc ctxt;
  leqcon ctxt (coerce_genop_con false ctxt cgop) (get_reg_con ctxt r);
  ctxt
;;

(* We generalize to allow comparisons of any two values of equal type.
 * Exception: we also allow S(i) to be compared with a B4 or S(j).
 * Exception: allow two pointers whose mem types are equal.
 * 
 * ctxt |- (genop1,coerce genop2) valid binops
 * ctxt |- genop1 reg_or_mem_genop
 * ctxt |- genop1 : c   ctxt |- coerce genop2 : c
 * -----------------------------------------------
 * ctxt |- Cmp genop1,coerce genop2 : byte4
 *)

let compare_allowed ctxt c1 c2 =
  (match c1.rcon,c2.rcon with
    (Cprim (PCbytes Byte4) | Csing _),
    (Cprim (PCbytes Byte4) | Csing _) -> ()
  | Chptr (_,_,_),Chptr (_,_,_) -> ()
(*  | Chptr (_,c1o,_),Chptr (_,c2o,_) ->
      (match c1o,c2o with Some c1,Some c2 -> eqcon ctxt c1 c2 | _ -> ())*)
  | (Cname c1), Csing _ -> ()
  | (Ctagof c1), Csing _ -> ()
  (* this allows comparing a bool (or other tag) to an integer without
   * refining the type. *)
  | (Chptr (tags,_,_)),_ -> leqcon ctxt c2 cbyte4
  | _,(Chptr (tags,_,_)) -> leqcon ctxt c1 cbyte4
  | _,_ -> eqcon ctxt c1 c2)

let verify_Cmp ctxt (genop1,_ as cgenop1) cgenop2 =
  valid_cbinops ctxt genop1 cgenop2; reg_or_mem_genop ctxt genop1;
  let c1 = whnorm ctxt (coerce_genop_con false ctxt cgenop1)
  and c2 = whnorm ctxt (coerce_genop_con false ctxt cgenop2) in
  compare_allowed ctxt c1 c2;
  set_cc ctxt (CCcmp (c1,c2))
;;

(* ctxt |- Eax : byte4
 * ----------------------------------
 * ctxt |- Cbw/Cwde : ctxt[Eax:byte4]
 *
 * ctxt |- Eax : byte4
 * -------------------------------------
 * ctxt |- Cdq/Cwd : ctxt[Eax,Edx:byte4]
 *)

let verify_Conv ctxt c =
  restore_cc ctxt;
  match c with
    (Cbw | Cwde) -> 
      leqcon ctxt (get_reg_con ctxt Eax) cbyte4;
      add_reg ctxt Eax cbyte4
  | (Cdq | Cwd) -> 
      leqcon ctxt (get_reg_con ctxt Eax) cbyte4;
      add_reg (add_reg ctxt Eax cbyte4) Edx cbyte4
;;

(* ctxt |- genop : byte4
 * ---------------------------------------
 * ctxt |- Imul3 r,genop,i : ctxt[r:byte4]
 *)

let verify_Imul3 ctxt r genop i =
  leqcon ctxt (genop_con false ctxt genop) cbyte4; add_reg ctxt r cbyte4
;;

(* A jump helper function.
 * verify_jump add_block ctxt lc  iff  ctxt |- jmp lc
 *)
let verify_jmp' mode add_block ctxt (l,_ as lc) =
  let c = coerce_label_con_s ctxt mode lc in 
  let msl = dcodems_or_label ctxt c in
  (match msl with
  | MachineState ms -> machine_state_leq ctxt (get_machine_state ctxt) ms
  | Lab l -> (add_block ctxt l)) (* If label has no type. *)
;;

let verify_jmp = verify_jmp' Rel;;

(* A helper function for Jcc and Jecxz *)
let split_sides ctxt cc =
  restore_cc ctxt;
    (try 
      let ctxt_taken,ctxt_fallthru = process_cc ctxt cc in
      (Some ctxt_taken), (Some ctxt_fallthru)
    with
      EmptyLeftSide  c -> (None,   Some c)
    | EmptyRightSide c -> (Some c, None))
;;

(* Add condition code information to the current context
 *
 * If a side of the branch cannot be taken, do not type-check it, else...
 *
 * If iopt is None then:
 *
 * ctxt |- lc : Ccode(g)    ctxt |- ctxt.gamma <= g
 * ------------------------------------------------
 * ctxt |- Jcc cc,lc : ctxt
 *
 * If iopt is Some instructions then
 *   1. Instructions must have no run-time effect
 *   2. Verify instructions in the ctxt for the branch taken producing ctxt'
 *   3. Verify Jcc using ctxt' as in the case for None
 *)
let verify_Jcc verify_instr add_block ctxt cc lc iopt =
  let left_side,right_side = split_sides ctxt cc in
  (match left_side with
    None -> ()
  | Some ctxt_taken ->
    (match iopt with
      None -> verify_jmp add_block ctxt_taken lc
    | Some is ->
      	if List.for_all is_virtual_instruction is then
	  let ctxt = List.fold_left (verify_instr add_block) ctxt_taken is in
	  verify_jmp add_block ctxt lc
      	else
	  inst_form ctxt "Jcc: coercion uses non-virtual instructions"));
  (match right_side with
    None -> raise Terminal_Jmp
  | Some ctxt_fallthru -> ctxt_fallthru)
;;

(* 
 * ctxt |- lc : Ccode(g)   ctxt |- ctxt.gamma <= g
 * ctxt |- Ecx : byte4
 * -----------------------------------------------
 * ctxt |- Jecxz lc : ctxt
 *
 * Handle iopt and branches not taken as above.
 *
 *)

let verify_Jecxz verify_instr add_block ctxt lc iopt =
  restore_cc ctxt;
  let c = whnorm ctxt (get_reg_con ctxt Ecx) in
  let ctxt = set_cc ctxt (CCcmp (c,csing (pcint i32_0))) in
  let left_side,right_side = split_sides ctxt BelowEq in

  let ctxt_taken,ctxt_fallthru = process_cc ctxt BelowEq in
  (match left_side with
    None -> ()
  | Some ctxt_taken ->
      (restore_cc ctxt_taken;
       (match iopt with
	 None -> verify_jmp add_block ctxt_taken lc
       | Some is ->
	   if List.for_all is_virtual_instruction is then
	     let ctxt = List.fold_left (verify_instr add_block) ctxt_taken is in
	     verify_jmp add_block ctxt lc
	   else
	     inst_form ctxt "Jecxz: coercion uses non-virtual instructions")));
  (match right_side with
    None -> raise Terminal_Jmp
  | Some ctxt_fallthru ->
      (restore_cc ctxt_fallthru;
       ctxt_fallthru))
;;

(* ctxt |- gc : Ccode(g)    ctxt |- ctxt.gamma <= g
 * ------------------------------------------------
 * ctxt |- branch lc : ctxt
 *)

let verify_Jmp add_block ctxt gc =
  restore_cc ctxt;
  (match gc with
    (Addr l,cs) -> verify_jmp add_block ctxt (l,cs)
  | _ ->
      let msl = dcodems_or_label ctxt (coerce_genop_con_rel false ctxt gc) in
      (match msl with
      |	MachineState ms -> machine_state_leq ctxt (get_machine_state ctxt) ms
      |	Lab l -> add_block ctxt l));
  raise Terminal_Jmp
;;

(* this is really only useful for moving an offset from the stack
 * into a register or moving a label into a register which is more
 * easily accomplished via a mov.
 *
 *  ctxt |- l : c
 *  ---------------------------
 *  ctxt |- Lea r,[l+0] : ctxt[r:c]
 *
 *  ctxt |- rc : Cprod(fs)
 *  ----------------------
 *  ctxt |- Lea r,[rc+0] : ctxt[r:Cprod(fs)]
 *
 *  ctxt |- rc : Stackptr(c1::...::cn::c)
 *  Stackptr(c1::...::cn::c) tailof ctxt.gamma(Esp)  <-- not really needed
 *  -----------------------------------------------
 *  ctxt |- Lea r,rc[i] : ctxt[r:Stackptr(c)]
 *)

let verify_Lea ctxt r g =
  restore_cc ctxt;
  match g with
    Immed _ -> inst_form ctxt "Lea: immediate"; raise Talfail
  | Reg _ -> inst_form ctxt "Lea: register"; raise Talfail
  | Addr _ -> inst_form ctxt "Lea: label"; raise Talfail
  | Prjr (rc,i,None) ->
      let rc_con = coerce_reg_con ctxt rc in
      (match (whnorm ctxt rc_con).rcon with
	Csptr c ->
	  let _ = valid_stack_con ctxt c in
	  add_reg ctxt r
	    (csptr (get_stack_tail ctxt i c))
      |	Chptr ([],_,_) ->
	  if i <>$ i32_0 then inst_form ctxt "Lea: middle tuple pointer";
	  add_reg ctxt r rc_con
      | _ -> inst_form ctxt "Lea: non pointer type"; raise Talfail)
  | Prjr (_,_,Some _) -> 
      inst_form ctxt "Lea: middle array pointer"; raise Talfail
  | Prjl (lc,i,None) when i=$i32_0-> add_reg ctxt r (coerce_label_con ctxt Abs lc)
  | Prjl (_,_,_) -> inst_form ctxt "Lea: middle pointer"; raise Talfail
;;
	      
(* ctxt |- Ecx : B4  ctxt |- lc : code(g)  ctxt |- g<=ctxt.gamma
 * -------------------------------------------------------------------
 * ctxt |- loop lc,bo : if bo=None then Terminal_Jmp else ctxt[Ecx:B4]
 *)

let verify_Loop add_block ctxt lc bo =
  restore_cc ctxt;
  leqcon ctxt (get_reg_con ctxt Ecx) cbyte4;
  let ctxt = add_reg ctxt Ecx cbyte4 in
  verify_jmp add_block ctxt lc;
  match bo with
    None -> raise Terminal_Jmp
  | Some _ -> ctxt
;;

(* NG - I've vastly simplified the move rule by moving all the smarts into
 *      the code for genop_write_at
 *
 *   ctxt |- gop writeable at c
 *   ctxt |- cgop : c
 *   gop,cgop valid binops
 *   --------------------------
 *   ctxt |- Mov gop,cgop
 *)

let verify_Mov ctxt gop cgop =
  restore_cc ctxt;
  valid_cbinops ctxt gop cgop;
  let c_source = coerce_genop_con false ctxt cgop in
  genop_write_at ctxt gop c_source
;;

(*
 *   if gop1 is a reg 
 *   then ctxt |- gop1 : byte4; update con gop1 byte4
 *   else ctxt |- gop1 writeable at byte(part1)
 *
 *   if gop2 is a reg 
 *   then ctxt |- gop2 : byte4
 *   else ctxt |- gop2 : byte(part2)
 *
 *   part1>=part2   
 *   valid_binops gop1 gop2
 *   ----------------------------------------
 *   ctxt |- Movsx/Movzx/Mov (gop1,part1), (gop2,part2)
 * if part1=part2 then this is just a move.
 *)
let verify_Movpart ctxt signext gop1 part1 gop2 part2 =
  restore_cc ctxt;
  valid_binops ctxt gop1 gop2;
  begin match part1,part2 with
    RPe,_ | RPx,RPx | RPx,RPh | RPx,RPl | RPh,RPh | RPl,RPl | RPh,RPl |
    RPl,RPh -> ()
  | _ -> inst_form ctxt "Movsx/Movzx/Mov: Cannot truncate size."; raise Talfail
  end;
  let part_con p = 
    match p with
      RPe -> cbyte4
    | RPx -> cbyte2
    | RPl | RPh -> cbyte1
  in
  begin match gop2 with
    Reg r -> leqcon ctxt (get_reg_con ctxt r) cbyte4
(*  | Immed i -> () (* should really check it's in range! *)  *)
  | _ -> leqcon ctxt (genop_con false ctxt gop2) (part_con part2)
  end;
  begin match gop1 with
    Reg r -> 
      if part1<>RPe then leqcon ctxt (get_reg_con ctxt r) cbyte4;
      add_reg ctxt r cbyte4
  | _ -> genop_write_at ctxt gop1 (part_con part1)
  end
;;

(*
  let aux r s1 gop s2 =
    if s1<>Byte4 then inst_form ctxt "Movsx/Movzx: dest must be 32 bit";
    if s2<>Byte2 & s2<>Byte1 then
      inst_form ctxt "Movsx/Movzx: src must be 8/16 bit";
    (match (whnorm ctxt (genop_con false ctxt gop)).rcon with
      Cprim (PCbytes s3) ->
        if s2<>s3 then 
          inst_form ctxt "Movsx/Movzx: operand size does not match scale"
    | _ -> inst_form ctxt "Movsx/Movzx: operand not bytes");
    add_reg ctxt r cbyte4
  in
  | Movsx (r,s1,gop,s2) -> aux r s1 gop s2
  | Movzx (r,s1,gop,s2) -> aux r s1 gop s2
*)

(* Pop
 *)

let verify_Pop ctxt genop =   (* Dan: Fixed to allow a memory operand *)
  restore_cc ctxt;
  let c_source = coerce_genop_con false ctxt (Prjr ((Esp,[]),i32_0,None),[]) in
  let ctxt = genop_write_at ctxt genop c_source in
  let ctxt = verify_ArithBin ctxt Add (Reg Esp) (Immed i32_4) in
  ctxt
;;

(* Popad pops all of the registers.  However, the value for the
 * Esp register is discarded.
 *
 * ctxt |- Esp : sptr(c0::c1::c2::c3::c4::c5::c6::c7::c)
 * ctxt |- ci : K4byte 0<=i<=7
 * ---------------------------------------------------------
 * ctxt |- Popad : ctxt[Eax:c7, Ebx:c4, Ecx:c6, Edx:c5,
 *                      Esp:sptr(c), Ebp:c2, Esi:c1, Edi:c0]
 *)

let verify_Popad ctxt =
  restore_cc ctxt;
  let strip_check ctxt r stkcon =
    let (c1,c2) = dcons ctxt stkcon in
    kindleq ctxt (con_kind ctxt c1) k4byte; (add_reg ctxt r c1,c2) in
  let stkcon = current_stack_con ctxt in
  let (ctxt,stkcon) = strip_check ctxt Edi stkcon in
  let (ctxt,stkcon) = strip_check ctxt Esi stkcon in
  let (ctxt,stkcon) = strip_check ctxt Ebp stkcon in
  let (ctxt,stkcon) = strip_check ctxt Ebx stkcon in
  let (ctxt,stkcon) = strip_check ctxt Ebx stkcon in
  let (ctxt,stkcon) = strip_check ctxt Edx stkcon in
  let (ctxt,stkcon) = strip_check ctxt Ecx stkcon in
  let (ctxt,stkcon) = strip_check ctxt Eax stkcon in
  add_reg ctxt Esp (csptr stkcon)
;;

(* Popfd pops a 4-byte value into the flags register.  Hence, the
 * stack pointer must point to a byte4 before hand, and afterwards,
 * points to the tail. 
 *   ctxt |- Esp : sptr(byte4::c)
 *   -----------------------------------------
 *   ctxt |- Popfd : ctxt[Esp:Stackptr(c)]
 *)

let verify_Popfd ctxt =
  let (c,c') = dcons ctxt (current_stack_con ctxt) in
  eqcon ctxt cbyte4 c;
  add_reg ctxt Esp (csptr c')
;;

(* ctxt |- Esp : sptr(c')
 * ctxt |- gc : c
 * ctxt |- c : T4
 * ----------------------------------------
 * ctxt |- pushd gc : ctxt[Esp:sptr(c::c')]
 *)

(* Support for other sizes needed *)
let verify_Push ctxt gc =
  restore_cc ctxt;
  let c = coerce_genop_con false ctxt gc in
  kindleq ctxt (con_kind ctxt c) k4byte;
  let cstack = ccons c (current_stack_con ctxt) in
  add_reg ctxt Esp (csptr cstack)
;;

(* ctxt |- Eax,Ebx,Ecx,Edx,Esi,Edi,Ebp : c7,c4,c6,c5,c1,c0,c2
 * ctxt |- Esp : sptr(c)
 * -----------------------------------------------------------------------
 * ctxt |- pushad : ctxt[Esp:sptr(c0::c1::c2::sptr(c)::c4::c5::c6::c7::c)]
 *
 * N.B. registers have to have K4byte-s in them.
 *)

let verify_Pushad ctxt = 
  restore_cc ctxt;
  let c3 = get_reg_con ctxt Esp in
  let cs = dsptr ctxt c3 in
  let c0 = get_reg_con ctxt Edi
  and c1 = get_reg_con ctxt Esi
  and c2 = get_reg_con ctxt Ebp
  and c4 = get_reg_con ctxt Ebx
  and c5 = get_reg_con ctxt Edx
  and c6 = get_reg_con ctxt Ecx
  and c7 = get_reg_con ctxt Eax in
  let cs = List.fold_right ccons [c0;c1;c2;c3;c4;c5;c6;c7] cs in
  add_reg ctxt Esp (csptr cs)
;;

(* ctxt |- Esp : sptr(c)
 * -----------------------------------------
 * ctxt |- Pushfd : ctxt[Esp:sptr(byte4::c)]
 *)

let verify_Pushfd ctxt =
  restore_cc ctxt;
  let cs = current_stack_con ctxt in
  add_reg ctxt Esp (csptr (ccons cbyte4 cs))
;;

(* Rdtsc: reads the time stamp counter into EDX:EAX.
   This instruction is available on the Pentium and Pentium II but is not
   guaranteed to exist on future processors.  

   The OS may disable this operation then your just out of luck.

   Still for our purposes quite useful. *)
let verify_Rdtsc ctxt = add_reg (add_reg ctxt Eax cbyte4) Edx cbyte4
;;

(* Retn: pops the return address off the stack and then pops an additional
 * (optional) i bytes off the stack, then jumps to the return address.
 *
 *  ctxt |- Esp : Ccode(G)::c1::c2::...::cn::ctail'
 *  ctxt |- sizeof(c1) + ... + sizeof(cn) = i
 *  ctxt |- ctxt.gamma[Esp:ctail'] <= G
 *  -----------------------------------------------
 *  ctxt |- Retn (Some i) : ctxt'
 *)

let verify_Retn ctxt iopt =
  restore_cc ctxt;
  let (cg,ctail) = dcons ctxt (current_stack_con ctxt) in
  let g2 = dcodems ctxt cg in
  let ctail' = 
    match iopt with 
      None -> ctail 
    | Some i -> get_stack_tail ctxt i ctail in
  let newctxt = add_reg ctxt Esp (csptr ctail') in
  machine_state_leq ctxt (get_machine_state newctxt) g2;
  raise Terminal_Jmp
;;

(* Setcc writes either 0 or 1 into the low part of the operand.
 * if the operand is demonstrably between 0 and 255 we make the
 * result have type Chptr([0;1],None)
 * otherwise we make the genop B4.
 *
 *   ctxt |- gop writeable   ctxt |- gop : Chptr(is,None)
 *   ----------------------------------------------------0,1 in is;is<256
 *   ctxt |- Setcc gop : ctxt
 *)

let verify_Setcc ctxt cc gop =
  restore_cc ctxt;
  let con = (whnorm ctxt (genop_con true ctxt gop)) in
  let is_small = 
    (match con.rcon with
      Chptr (is,None,_) -> 
      let check_tag i = (land32 i (lnot32 i32_255)) =$ i32_0 in
      ((List.mem i32_0 is) && (List.mem i32_1 is) && List.for_all check_tag is)
  | Csing c ->
      let i = dint ctxt c in
      (land32 i (lnot32 i32_255)) =$ i32_0
  | _ -> false) in
  if is_small then genop_write_at ctxt gop (chptr [i32_0;i32_1] None None)
  else (leqcon ctxt con cbyte4;
	genop_write_at ctxt gop cbyte4)
;;

(* shifts: gop must be writeable and a byte4, r must be a byte4
 *         Ecx must be byte4 if it is the shift amount
 *
 * ctxt |- gop : byte4  ctxt |- gop writeable
 * ctxt |- Ecx : byte4    ctxt |- r : byte4
 * ----------------------------------------------
 * ctxt |- Shld/Shrd gop,r,None : ctxt
 *
 * ctxt |- gop : byte4  ctxt |- gop writeable
 * ctxt |- r : byte4
 * ----------------------------------------------
 * ctxt |- Shld/Shrd gop,r,(Some i) : ctxt
 *)

let verify_Shld_Shrd ctxt gop r iopt =
  leqcon ctxt (genop_con false ctxt gop) cbyte4;
  leqcon ctxt (get_reg_con ctxt r) cbyte4;
  (match iopt with
    Some _ -> ()
  | None -> leqcon ctxt (get_reg_con ctxt Ecx) cbyte4);
  genop_write_at (add_reg ctxt r cbyte4) gop cbyte4
;;

(* ctxt |- (gop1,gop2) valid_binops
 * ctxt |- gop1 : byte4  ctxt |- gop2 : byte4
 * ------------------------------------------
 * ctxt |- test gop1,gop2 : ctxt
 *)

let verify_Test ctxt gop1 gop2 =
  valid_binops ctxt gop1 gop2;
  let c1 = genop_con false ctxt gop1
  and c2 = genop_con false ctxt gop2 in
  leqcon ctxt c1 cbyte4; leqcon ctxt c2 cbyte4;
  set_cc ctxt (CCtest (c1,c2))
;;

(* exchange gop1 and gop2.
 *   ctxt |- r : c2
 *   ctxt |- gop1 : c1
 *   ctxt |- gop1 writeable
 *   ctxt |- c2 <= c1
 *   ----------------------------------------------
 *   ctxt |- Xchg gop1,gop2 : ctxt[r:c1]
 *
 * Exception if gop1=r1 then don't require c2<=c1 and return
 *   ctxt[r:c1,r1:c2]
 * NG - could add case where gop1=[r+i] and r:sptr c1' to change the stack
 *      type.  This isn't implemented yet.
 *)

let verify_Xchg ctxt gop1 r =
  restore_cc ctxt;
  let c1 = genop_con true ctxt gop1
  and c2 = get_reg_con ctxt r in
  match gop1 with
    Reg r1 ->
      add_reg (add_reg ctxt r1 c2) r c1
  | _ ->
      leqcon ctxt c2 c1; add_reg ctxt r c1
;;

(*** TAL specific instructions ***)

(*** SCW : as far as I can tell, this code is dead.
let verify_array ctxt gop1 es rind gop2 =
  let error s = generate_error ctxt (Genop (s,gop1)) in
  let (c1,pr,offset) =
    match gop1 with
      Prjr ((r,_ as rc),offset,None) ->
        coerce_reg_con ctxt rc, (if r=Esp then PRsp else PRor), offset
    | Prjr (_,_,Some _) -> 
	error "Asub/Aupd: array has variable proj"; raise Talfail
    | Prjl (lc,offset,None) -> coerce_label_con ctxt lc, PRns, offset
    | Prjl (_,_,Some _) ->
	error "Asub/Aupd: array has variable proj"; raise Talfail
    | _ -> error "Asub/Aupd: array not a projection"; raise Talfail in
  let c1 =
    match (whnorm ctxt c1).rcon with
      Chptr (_::_,_,_) -> error "prj: pointer has tags"; raise Talfail
    | Chptr ([],None,_) -> error "prj: not pointer"; raise Talfail
    | Chptr ([],Some c,_) -> c
    | Csptr c ->
      	if pr = PRns then
 	  failwith "Talverify.verify_array - should not be a stack pointer"
      	else if pr = PRor then
	  (let _ = valid_stack_con ctxt c in ());
      	c
    | _ -> error "prj: not a pointer or stack"; raise Talfail in
  let (_,c1,offset) = get_mem_offset_p ctxt (whnorm ctxt c1) offset in
  let (csize,celt,v) =
    let (csize,celt) = darray ctxt c1 in
    let _,celt = get_mem_offset ctxt (whnorm ctxt celt) offset in
    let (c,v) = dfield ctxt celt in
    (csize,c,v) in
  let c2 = dsing ctxt (genop_con false ctxt gop2) in
  eqcon ctxt csize c2;
  if es<>(sizeof ctxt celt) then inst_form ctxt "Asub/Aupd: bad element size";
  leqcon ctxt (get_reg_con ctxt rind) cbyte4;
  (celt,v)
;;
***)

(* JGM: I'm treating this as a "MOV cgop,cgop".  Since both operands
 * may be in memory, I can't call verify_Mov directly.  But this should
 * save a lot of hassle and covers the cases that matter:  registers and
 * stack slots.
 *)
let verify_Coerce ctxt cgop = 
  restore_cc ctxt;
  let c_source = coerce_genop_con false ctxt cgop in
  genop_write_at ctxt (fst cgop) c_source
;;

(* Dave: Coerce an object indirectly through a name 
 *
 * ctxt |- CurrentCap = C + {x!tau}
 * -----------------------------------------------------------
 * ctxt |- CoerceName coerce(x) : ctxt[ C + {x -> coerce(tau)} ]
 *
 * Currently name must be unique.  We believe it is sound if
 * name is not unique but we do not currently have a proof of
 * that fact.
 *)
let verify_CoerceName ctxt ((name,coercions) as nc) =
  restore_cc ctxt;
  let name = dvar ctxt ((local_subst ctxt) (cvar name)) in
  let (alias,c) = get_name ctxt name in
  match alias with
    Unique ->
      let c_result = coerce_con (fun _ _ -> c) ctxt nc in
      change_name ctxt name (alias,c_result)
  | MayAlias -> 
      inst_form ctxt "illegal coercion under may-alias name"; raise Talfail

let byte8_field = cfield cbyte8 ReadWrite;;
let byte4_field = cfield cbyte4 ReadWrite;;
let byte2_field = cfield cbyte2 ReadWrite;;
let byte1_field = cfield cbyte1 ReadWrite;;

(* calculate a type from a mallocarg option -- if the option is None, then
 * we assume a tuple of i/4 junk words (i.e., 4 bytes).  *)
let mallocargcon ctxt i maopt = 
  match maopt with
    None -> 
      let rec loop c = 
	if c =$ i32_0 then []
	else if c <$ i32_4 then 
	  (inst_form ctxt "malloc: bad malloc arg"; raise Talfail)
	else byte4_field :: (loop (c -$ i32_4))
      in cprod_b (loop i)
  | Some ma -> 
      let rec macon ma = 
	match ma with
	  Mbytes Byte4 -> (byte4_field,i32_4)
	| Mbytes Byte8 -> (byte8_field,i32_8)
	| Mbytes Byte2 -> (byte2_field,i32_2)
	| Mbytes Byte1 -> (byte1_field,i32_1)
	| Mprod mas -> 
	    let (cons,count) = macons mas in
	    (cprod cons,count)
	| Mbytearray(s,i) -> 
	    (carray (pcint i) (cfield (pcbytes s) ReadWrite), 
	     (scale_to_int32 s)*$ i)
      and macons mas = 
	match mas with 
	  [] -> ([],i32_0)
	| ma::rest -> 
	    let (cs,count) = macons rest in
	    let (c,x) = macon ma in
	    (c::cs,x +$ count) in
      let (con,count) = macon ma in
      if (count <>$ i) then 
	(inst_form ctxt "malloc: size mismatch"; raise Talfail);
      cptr con;;
	   
(* Add a new Kname x (alpha converting) to the context.  Add to the
 * current capability the fact that x is unique and has type pointer
 * to junk (of size i).  Then trash all but the callee-save registers
 * and give eax the type Cname(x).  
 *
 * Dave: if the x is supplied fresh by programmer then use it, otherwise
 * generate a fresh x' and enter the substition x -> x'
 *)
let supply_fresh_tvar ctxt x =
  if (Dict.member (get_var_map ctxt) x || 
      Dict.member (get_abbrevs ctxt) x ||
      Dict.member (get_locals ctxt) x) then
    let id = id_new "*" in
    (add_local_subst subst ctxt x (cvar id), id)
  else (ctxt,x)

let verify_Malloc ctxt x i maopt = 
  if i <$ i32_0 then inst_form ctxt "malloc of negative value";
  let (ctxt,x) = supply_fresh_tvar ctxt x in
  let xcon = cvar x in
  let ctxt = add_var ctxt x kname in
  let c = mallocargcon ctxt i maopt in 
  let ctxt = add_name ctxt x (Unique,c) in
  let ms = get_machine_state ctxt in
  let ms = ms_del_regs ms [Eax;Ecx;Edx] in
(*  let ms = ms_del_regs ms [Eax;Ebx;Ecx;Edx;Esi;Edi] in *)
  let ctxt = set_machine_state ctxt (ms_set_reg ms Eax (cname xcon)) in
  ctxt
;;

(* Apply each proof rule in succession to the context *)
let verify_Proof ctxt pf = 
  let prove ctxt premises = prove ctxt (cand premises) in
  let subst_f = local_subst ctxt in
  try
    List.fold_left (fun ctxt (rule,cs) -> 
      let cs = List.fold_right (fun c cs ->
	let c = subst_f c in check ctxt c; c::cs) cs [] in
      	Tallogic.apply ctxt prove rule cs)
      ctxt pf
  with 
    Tallogic.Bad_rule x -> 
      inst_form ctxt ("proof rule ill-formed: "^x); 
      raise Talfail
  | Not_found ->
      inst_form ctxt "non-existant proof rule";
      raise Talfail

(* verify gc is an exists, and introduce the new type variable a into
 * scope, as well as moving the value under the existential into the
 * register r.  Also, it's necessary to alpha-convert a so that it
 * is unique and does not conflict with something already in the
 * context...we do so by choosing a fresh variable, a'' and adding
 * an abbreviation that maps a to a'' for all constructors obtained
 * from the user.
 *
 * Dave: we now treat (PCbytes Byte4) as an abbreviation for Exists i:Int.S(i)
 * Unpack operates on B4's as if they were this existential.
 * Since S(e) <= B4, Unpack must also operate on singletons.  
 * Unpack of S(e) is equivalent to a pack and subsequent unpack.
 *
 *)

let unpack_aux ctxt a rcon update_ctxt =
  let (ctxt,a) = supply_fresh_tvar ctxt a in
  let a_con = cvar a in
  match rcon with
    Cexist(a',k,c1',c2') ->
      let c1 = subst a_con a' c1' in
      let c2 = subst a_con a' c2' in
      let ctxt = add_var ctxt a k in
      let ctxt = add_conjunct ctxt c1 in 
      kindleq ctxt (con_kind ctxt c2) k4byte;
      let ctxt = update_ctxt ctxt c2 in
      ctxt
  | Cprim (PCbytes Byte4) | Csing _ ->
      let ctxt = add_var ctxt a kint in
      let ctxt = update_ctxt ctxt (csing a_con) in
      ctxt
  | _ -> 
      inst_form ctxt "Unpack: genop not Exists, B4, or singleton";
      raise Talfail

let verify_Unpack ctxt a r gc =
  restore_cc ctxt;
  let rcon = (whnorm ctxt (coerce_genop_con false ctxt gc)).rcon in
  unpack_aux ctxt a rcon (fun ctxt c -> add_reg ctxt r c)
;;

(* Treat this as an unpack into a virtual register and mov back into g *)
let verify_Sunpack ctxt a g =
  restore_cc ctxt;
  let rcon = (whnorm ctxt (genop_con true ctxt g)).rcon in
  unpack_aux ctxt a rcon (fun ctxt c -> genop_write_at ctxt g c)
;;


(* If ctxt |- gop : c, then add x:(MayAlias,c) to the current capability
 * and change the type of the gop to Name(x), alpha-converting x as 
 * appropriate.  We only allow naming (1) a register or (2) a stack slot
 * so as to avoid unsoundness issues with threads.  We could strengthen
 * this to support naming read-only fields in objects, or fields of 
 * unique objects but for now we're keeping things simple.  Also, we
 * must check that the object we're naming is a Kbyte4. 
 *)
let verify_Nameobj ctxt x gop = 
  restore_cc ctxt;
  let c = genop_con true ctxt gop in
  kindleq ctxt (con_kind ctxt c) k4byte; 
  let (ctxt,x) = supply_fresh_tvar ctxt x in
  let xcon = cvar x in
  let ctxt = add_var ctxt x kname in
  let namecon = cname xcon in
  let ctxt = add_name ctxt x (MayAlias,c) in
  let error s = generate_error ctxt (Genop(s,gop)); raise Talfail in
  let ctxt = 
  match gop with
    Reg r -> add_reg ctxt r namecon
  | Prjr((r,[]),i,None) -> 
      (match (get_reg_con ctxt r).rcon with
	Csptr _ -> genop_write_at ctxt gop namecon
      |	_       -> error "Nameobj: invalid operand")
  | _ -> error "Nameobj: invalid operand"
  in 
(*
  Talpp.print_ctxt Format.std_formatter Talpp.std_options ctxt;
  Format.print_newline ();
*)
  ctxt
;;

let verify_ForgetUnique ctxt x =
  restore_cc ctxt;
  let error () = 
    (inst_form ctxt ((id_to_string x)^" name is not unique"); raise Talfail) in
  let xcon = (local_subst ctxt) (cvar x) in
  let x' = dvar ctxt (snd (check_whnorm ctxt xcon)) in
  match get_name ctxt x' with
    (Unique,c) -> change_name ctxt x' (MayAlias,c)
  | _ -> error ()
;;

let verify_RemoveName ctxt x = 
(*  Talpp.print_ctxt Format.std_formatter Talpp.std_options ctxt;
    Format.print_newline ();
*)
  restore_cc ctxt;
  let xcon = (local_subst ctxt) (cvar x) in
  let (_,c) = check_whnorm ctxt xcon in
  let x' = dvar ctxt c in
  remove_name ctxt x'
;;

(* Floating Point Instructions *)
let fp_error ctxt s = inst_form ctxt ("FP error: "^s)

let setfps ctxt i s = 
  set_fpstack ctxt 
    (fpstack_adjust (fun s' -> fp_error ctxt (s^": "^s')) (get_fpstack ctxt) i)
let popfps ctxt   s = setfps ctxt 1 s

(* check for numargs on top of stack. adjust stack by given amount *)
let checkfp ctxt numargs stack_adjustment opname =
  let fps = get_fpstack ctxt in
  (match numargs with
    0 -> ()
  | 1 -> 
      if fpstack_inrange fps 0 then () 
      else fp_error ctxt (opname^": register out of range")
  | 2 -> 
      if fpstack_inrange fps 0 && fpstack_inrange fps 1 then ()
      else fp_error ctxt (opname^": register out of range")
  | _ -> invalid_arg "checkfp: too many arguments");
  if stack_adjustment = 0 then ctxt
  else set_fpstack ctxt (fpstack_adjust (fp_error ctxt) fps stack_adjustment)

(* check a floating point memory operand *)
let checkfp_mem ctxt g opname pcon =
  if is_mem g then
    match (whnorm ctxt (genop_con_abs false false ctxt g)).rcon with
      Cprim pc -> 
	if pc = pcon then () 
	else fp_error ctxt (opname^": wrong primcon")
    | _ -> fp_error ctxt (opname^": not primcon")
  else fp_error ctxt (opname^": non-memory arg")

let checkfp_realmem ctxt s g opname =
  checkfp_mem ctxt g opname 
    (if      s = Byte4 then PCfloat32
     else if s = Byte8 then PCfloat64
     else (fp_error ctxt (opname^": floating point scale wrong size"); raise Talfail))

let checkfp_intmem ctxt s g opname =
  if s = Byte2 or s = Byte4 then
    checkfp_mem ctxt g opname (PCbytes s)
  else
    fp_error ctxt (opname^": fp integer scale wrong size")

(* check args from ST, ST(i) *)
let checkfp_regs ctxt i opname =
  let fps = get_fpstack ctxt in
  if fpstack_inrange fps 0 && fpstack_inrange fps i then ()
  else fp_error ctxt (opname^": register arguments out of range")

let verify_FPnoargs ctxt op =
  restore_cc ctxt;
  match op with
    Fclex | Fnclex | Fnop | Fwait ->
      (* no arguments; no pops; no results *)
      ctxt
  | Finit | Fninit ->
      (* initialize floating point unit *)
      set_fpstack ctxt fpstack_empty
  | F2xm1 | Fabs | Fchs | Fcos | Frndint | Fsin | Fsqrt | Ftst | Fxam ->
      (* 1 operand on top of stack; no pops; if result then on top of stack *)
      checkfp ctxt 1 0 "unary fp operator"
  | Fcompp | Fucompp ->
      (* 2 operands on top of stack; pop both; no result *)
      checkfp ctxt 2 2 "fp compare" 
  | Fpatan | Fyl2x | Fyl2xp1 ->
      (* 2 operands on top of stack; pop one operand; result on stack *)
      checkfp ctxt 2 1 "fpatan/fyl2x/fyl2xp1"
  | Fptan | Fsincos | Fxtract ->
      (* 1 operand on top of stack; push once; result is top 2 stack slots *)
      checkfp ctxt 1 (-1) "fptan/fsincos/fxtract"
  | Fprem | Fprem1 | Fscale ->
      (* 2 operands on top of stack; no pops; result on top of stack *)
      checkfp ctxt 2 0 "fprem/fprem1/fscale"
  | Fdecstp ->
      (* decrement top of stack pointer. *)
      set_fpstack ctxt (fpstack_rotate (get_fpstack ctxt) (-1))
  | Fincstp ->
      (* increment top of stack pointer. *)
      set_fpstack ctxt (fpstack_rotate (get_fpstack ctxt) 1)
  | Fld1 | Fldz | Fldpi | Fldl2e | Fldl2t | Fldlg2 | Fldln2 ->
      (* push constant onto top of stack *)
      checkfp ctxt 0 (-1) "fld constant"

let verify_FPsomeargs ctxt op args =
  match op with
    Fadd | Fdiv | Fdivr | Fmul | Fsub | Fsubr ->
      (* binary fp operations, generic operations *)
      restore_cc ctxt;
      (match args with
       	FPstack i -> fp_error ctxt "binary fp op: requires 2 arguments"; ctxt
      |	FPstack2 (b,i) -> checkfp_regs ctxt i "binary fp op"; ctxt
      |	FPgenop (s,g) -> checkfp_realmem ctxt s g "binary fp op"; ctxt)
  | Faddp | Fdivp | Fdivrp | Fmulp | Fsubp | Fsubrp ->
      (* binary fp operations, args: reg, ST, pop ST *)
      restore_cc ctxt;
      (match args with
	FPstack2 (b,i) -> 
	  if b then 
	    fp_error ctxt "binary fp op/pop: 2nd arg must be top of stack"
	  else 
	    checkfp_regs ctxt i "binary fp op/pop";
	  popfps ctxt "binary fp op/pop"
      | _ -> fp_error ctxt "binary fp op/pop: bad args"; ctxt)
  | Fiadd | Ficom | Fidiv | Fidivr | Fimul | Fisub | Fisubr ->
      (* binary fp-int operation, args: ST, int in memory, dest: ST *)
      restore_cc ctxt;
      (match args with
	FPgenop (s,g) -> checkfp_intmem ctxt s g "binary fp-int"; ctxt
      |	_ -> fp_error ctxt "binary fp-int: bad args"; ctxt )
  | Fcom ->
      (* binary operation, but 1 arg is always in ST *)
      restore_cc ctxt;
      (match args with
	FPstack i -> fp_error ctxt "fcom: binary operator"; ctxt
      |	FPstack2 (b,i) ->
	  if b then fp_error ctxt "fcom: 2nd arg must be top of stack"
	  else checkfp_regs ctxt i "fcom";
	  ctxt
      |	FPgenop (s,g) -> checkfp_realmem ctxt s g "fcom"; ctxt)
  | Fcomp ->
      restore_cc ctxt;
      (match args with
	FPstack i -> fp_error ctxt "fcomp: binary operator"; ctxt
      |	FPstack2 (b,i) -> checkfp_regs ctxt i "fcomp"; popfps ctxt "fcomp"
      |	FPgenop (s,g) -> checkfp_realmem ctxt s g "fcomp"; popfps ctxt "fcomp")
  | Ficomp -> 
      restore_cc ctxt;
      (match args with
	FPgenop (s,g) -> checkfp_intmem ctxt s g "ficomp"; popfps ctxt "ficomp"
      |	_ -> fp_error ctxt "ficomp: bad args"; ctxt)	
  | Fucom | Fxch ->
      restore_cc ctxt;
      (match args with
	FPstack i -> checkfp_regs ctxt i "fucom/fxch"; ctxt
      |	FPstack2 _ -> fp_error ctxt "fucom/fxch: not binary operator"; ctxt
      |	FPgenop _ -> fp_error ctxt "fucom/fxch: memory ops not allowed"; ctxt)
  | Fucomp ->
      (* args: reg, ST or none; pop ST *)
      restore_cc ctxt;
      (match args with
	FPstack i -> 
	  checkfp_regs ctxt i "fucomp";
	  popfps ctxt "fucomp"
      | _ -> fp_error ctxt "fucomp: bad args"; ctxt)
  | Ffree ->
      restore_cc ctxt;
      (match args with
	FPstack i -> set_fpstack ctxt (fpstack_free_reg (get_fpstack ctxt) i)
      |	_ -> fp_error ctxt "ffree: unary arg only"; ctxt)
  | Fld | Fild ->
      restore_cc ctxt;
      let is_iop = 
	match op with Fld -> false | Fild -> true | _ -> failwith "impossible"
      in
      (match args with
	FPstack i ->
	  if fpstack_inrange (get_fpstack ctxt) i then 
	    (if is_iop then fp_error ctxt "fild: register arg")
	  else fp_error ctxt "fld/fild: reg arg out of range"
      |	FPstack2 _ -> fp_error ctxt "fld/fild: 2 args"
      |	FPgenop (s,g) -> 
	  if is_iop then checkfp_intmem ctxt s g "fild"
	  else checkfp_realmem ctxt s g "fld");
      setfps ctxt (-1) "fld/fild"
  | Fst | Fstp | Fist | Fistp ->
      restore_cc ctxt;
      if fpstack_inrange (get_fpstack ctxt) 0 then ()
      else fp_error ctxt "fst[i/p]: top of stack out of range";
      let stack_adjustment, is_iop = 
	match op with
	| Fst   -> 0, false
	| Fist  -> 0, true
	| Fstp  -> 1, false
	| Fistp -> 1, true
	| _     -> failwith "impossible" in	  
      let ctxt =
      	match args with
	  FPstack i -> 
	    if not is_iop then
	      set_fpstack ctxt (fpstack_init_reg (get_fpstack ctxt) i)
	    else (fp_error ctxt "fist[p]: register arg"; ctxt)
      	| FPstack2 _ -> fp_error ctxt "fst[i/p]: 2 args"; ctxt
      	| FPgenop (s,g) -> 
	  (match s,is_iop with
	    Byte4,false -> genop_write_at ctxt g pcfloat32
	  | Byte8,false -> genop_write_at ctxt g pcfloat64
	  | Byte2,true  -> genop_write_at ctxt g cbyte2
	  | Byte4,true  -> genop_write_at ctxt g cbyte4
	  | Byte8,true  -> genop_write_at ctxt g cbyte8
	  | _ -> fp_error ctxt "fst[i/p]: invalid scale"; ctxt) in
      setfps ctxt stack_adjustment "fst[i/p]"
  | Fcomi | Fcomip | Fucomi | Fucomip ->
      (* Dave: These instructions available on pentium pro and above only *)
      (* Do not reset condition codes: they should be no info *)
      if not (pentium_pro ()) then 
	fp_error ctxt "fp compare and set cc: only available on pentium pro";
      let sa = 
	match op with
	  Fcomi | Fucomi -> 0
	| Fcomip | Fucomip -> 1
	| _ -> failwith "impossible" in
      (match args with
	FPstack2 (b,i) -> 
	  let fps = get_fpstack ctxt in
	  if fpstack_inrange fps i then 
	    let error s = fp_error ctxt ("fp compare and set cc: "^s) in
	    set_fpstack ctxt (fpstack_adjust error fps sa)
	  else
	    (fp_error ctxt "fp compare and set cc: register out of range";
	     ctxt)
      |	_ -> 
	  fp_error ctxt "fp comp and set cc: args must be registers"; ctxt)
  | Fstsw | Fnstsw ->
      restore_cc ctxt;
      (match args with
	FPgenop (s,g) ->
	  if not (s = Byte2) then
	    (fp_error ctxt "fstsw/fnstsw: arg must be 2 bytes"; ctxt)
	  else
	    (match g with
	      Reg Eax -> 
		let ms = get_machine_state ctxt in
		set_machine_state ctxt (ms_set_reg ms Eax cbyte4)
	    | _ ->  genop_write_at ctxt g cbyte2)
      |	_ -> fp_error ctxt "fstsw/fnstsw: arg must be memory or AX";ctxt)

(* Cyclone *)

(* get_unique_reg_con : ctxt -> reg -> identifier * con
   expects the register to be a name(x), where x is Unique.
   return x and the con x is bound to.
   Should this go in talctxt or talcon???
 *)
let get_unique_reg_con ctxt reg =
  let rcon = get_reg_con ctxt reg in
  let rname = dvar ctxt (dname ctxt rcon) in
  begin match (get_name ctxt rname) with
    (Unique,c) -> (rname,c)
  | (_,_) -> (inst_form ctxt "Register is not uniquely named";
	      raise Talfail)
  end
;;
 
(* Auxiliary functions used for verifying fills in Cyclone.
** cg_nm is the name of a code-generation region
** cg_con is the con associated with that region
** tp_con should be the con of a pointer to a template copy.
** hole is the label of the hole to fill in that copy.
** fill_con is the con of the value that will fill the hole.
** The return value will be the context with the hole removed.
*)
let verify_fill ctxt cg_nm cg_con tp_con hole fill_con =
begin
  match tp_con.rcon,cg_con.rcon with
    Ctptr iv,
    Ctrgn(pre,post,t) ->
      let rec aux l =
        match l with
          [] -> (inst_form ctxt "verify_fill 2"; [])
        | (i,labels,holes)::ls ->
            if (id_compare i iv)<>0 then (i,labels,holes)::(aux ls)
            else
              let rec clobber_hole hs =
                match hs with
                  [] -> (inst_form ctxt "verify_fill 3"; [])
                | (h,h_con)::hs ->
                    if (id_compare h hole)<>0 then (h,h_con)::(clobber_hole hs)
                    else (leqcon ctxt fill_con h_con; hs)
              in (i,labels,clobber_hole holes)::ls
      in
      let t' = aux t in
      change_name ctxt cg_nm (Unique,ctrgn (pre, post, t'))
  | _ -> (inst_form ctxt "verify_fill 1"; ctxt)
end;;

let verify_CgFill ctxt cg_reg tp_reg fill_reg tmpl hole  =
  begin
 (* tmpl is the template of the label, needed for MASM.  If we
    ** stick with MASM we should check this -- we don't right now. *)
    let tp_con = get_reg_con ctxt tp_reg in   (* template pointer *)
    let fill_con = get_reg_con ctxt fill_reg in (* value to fill hole *)
    let (cg_nm,cg_con) = get_unique_reg_con ctxt cg_reg in
    verify_fill ctxt cg_nm cg_con tp_con hole fill_con
  end

let verify_CgFillJmp ctxt cg_reg r1 l1a l1b r2 l2a l2b =
  begin
    (* XXX - l1a and l2a are template labels, needed by MASM;
       if we stick with MASM we should check this. *)
    let (cg_nm,cg_con) = get_unique_reg_con ctxt cg_reg in
    let src_con = get_reg_con ctxt r1 in
    let tgt_con = get_reg_con ctxt r2 in 
     (* r2,l2a,l2b identify the target; find out its con *)
    match tgt_con.rcon, cg_con.rcon with
      Ctptr iv, Ctrgn(pre,post,t) ->
        begin
          let rec aux l =
            match l with
              [] -> raise Not_found
            | (i,labels,holes)::ls ->
              if i=iv then List.assoc l2b labels
              else aux ls in
          try
            let fill_con = aux t in
            verify_fill ctxt cg_nm cg_con src_con l1b fill_con
          with Not_found ->
            (inst_form ctxt "verify_fill_jump 3"; ctxt)
        end
    | _ -> (inst_form ctxt "verify_fill_jump 1"; ctxt)
  end
(* End Cyclone *)


(* --- LX instructions --- *)

(*  ctxt |- c = a : *[k1...kn] 
 *  -----------------------
 *  ctxt |- letprod [b1,...,bn],c : ctxt[ a=*[b1...bn] ]
 * 
 *  ctxt |- c = *[c1...cn] : *[k1...kn]
 *  -------------------------------------
 *  ctxt |- letprod [b1...bn],c : ctxt [ b1=c1 ... bn=cn ]
 *
 *  If c is equivalent to a type variable, this instruction 
 *  replaces that variable with the tuple of the type variables b1 ... bn
 *  within the context.
 *  Otherwise, if c is a tuple, the b's are bound to each 
 *  of the components of the tuple.
 *)
let verify_Letprod ctxt bs con =
   let (kind,con) = check_whnorm ctxt con in 
   match kind.rkind with 
      Kprod ks -> 
       ( match (con.rcon) with 
	  Cvar a -> 
	     (try let (bs,ctxt) = List.fold_right2
		   (* For each b, rename it if it is already bound in the context
		      and add it to the context *)
		   (fun v k (vars,ctxt) -> 
		      let ctxt,v = supply_fresh_tvar ctxt v in 
		      let ctxt = add_var ctxt v k in 
		      v::vars, ctxt) bs ks ([], ctxt) in 
    	          let c = (defcon(Ctuple (List.map cvar  bs))) in 
		  (* replace a with c in the context... *)
		  let ctxt = refine_ms ctxt a c  in 
		  (* ...and in all future references to a in the code *) 
		  add_local_subst subst ctxt a c 
	     with Invalid_argument _ -> 
		inst_form ctxt "Letprod: wrong number of refinement variables"; 
		raise Talfail)
   	| Ctuple cs ->  
	     (try List.fold_left2 
		   (fun ctxt b c ->
		      add_local_subst subst ctxt b c) ctxt bs cs
	     with Invalid_argument _ -> 
		inst_form ctxt "Letprod: wrong number of refinement variables"; 
		raise Talfail)
	| _ -> inst_form ctxt "Letprod: Must have either a variable or product"; 
	     raise Talfail)
    | _ ->  inst_form ctxt "Letprod: requires product kind"; raise Talfail

(*  ctxt |- c = a : mu j.k
 *  ------------------------
 *  ctxt |- letroll b,c : ctxt[ a=(fold b) ]
 *
 *  ctxt |- c = fold c' : mu j.k
 *  ----------------------------
 *  ctxt |- letroll b,c : ctxt [ b=c' ]
 *
 *  If c is equivalent to a type variable a, within the ctxt a is replaced
 *  by (fold b). 
 *  Otherwise, if c is a fold, then b is bound to the body of the fold.
 *) 
let verify_Letroll ctxt b con =
   let (kind,con) = check_whnorm ctxt con in 
   match kind.rkind with 
    Kmu (schema,j) as muk -> 
       ( match (con.rcon) with 
	  Cvar a -> 
 	     let unrolled = unroll_kind kind in
	     let ctxt,b = supply_fresh_tvar ctxt b in 
	     let ctxt = add_var ctxt b unrolled in  
	     let newcon = (defcon(Cfold (kind,cvar b))) in
	     let ctxt = refine_ms ctxt a newcon in 
	     add_local_subst subst ctxt a newcon
   	| Cfold(k2, c) -> 
	     add_local_subst subst ctxt b c
	| _ -> inst_form ctxt "Letroll requires a variable or roll constructor";
	     raise Talfail)
    | _ ->  inst_form ctxt "Roll refinment requires a constructor with recursive kind"; 
	 raise Talfail
	  
(* ctxt |- c = a : +[k1 ... kn]
 * ctxt[ a=inj j b ] |- gc : void      (j not = i, 1 <= j<= n)
 * ------------------------------
 * ctxt |- vcase i,b,c,gc : ctxt [a= inj i b]
 *
 * ctxt |- c = inj i c' : +[ k1 ... kn]
 * ------------------------------------
 * ctxt |- vcase i,b,c,gc : ctxt [b=c']
 *
 * If c is equivalent to a typevariable a, then (inj i b) is
 * substituted for a within the context. Furthermore, to check that i
 * is the "correct" index, for each j != i, if (inj j b) is
 * substituted for a instead, then gc must be of type void.  
 *
 * If c is equivalent to (inj j c') and i=j then b is bound to c'.  
 *)  
let verify_Vcase ctxt i c b gc = 
   let (kind,con) = check_whnorm ctxt c in
      match kind.rkind with
      Ksum ks -> 
	 (match (con.rcon) with 
	    Cvar a ->
	       let gcon = coerce_genop_con false ctxt gc in  
	       let gkind = con_kind ctxt gcon in 	       	
	       (* make sure other branches are dead in refined context *)
	       let deadcase j  =  
		  let ctxta = add_var ctxt b (List.nth ks j) in
		  let cj = (defcon(Cinj(j, cvar b,kind))) in 
		  let ctxta = refine_ms ctxt a cj in	
		  let ctxta = set_verify_ctxt ctxta "Vcase: genop is not refined to void" in 
		  leqcon ctxta (subst cj a gcon) (defcon(Cvoid(gkind))) in 
	       for j = 0 to (List.length ks)-1 do
		  if i<>j then deadcase j else ()
	       done;
	       (* Add new con variable to ctxt with correct kind *)
	       let ctxt,b = supply_fresh_tvar ctxt b in 
	       let ctxt = add_var ctxt b (try List.nth ks i 
	       with Failure "nth" -> inst_form ctxt "Vcase sum kind wrong"; raise Talfail ) in 
	       (* refine context *)
	       let ci = (defcon(Cinj(i, cvar b,kind))) in 
	       let ctxt = refine_ms ctxt a ci in 
	       add_local_subst subst ctxt a ci 
		  
	  | Cinj(i2, c, k) -> 
	       if i = i2 then
		  add_local_subst subst ctxt b c 
	       else 
		  (inst_form ctxt "Vcase on wrong branch"; raise Talfail)
	  | _ -> inst_form ctxt "Must have variable or injection"; raise Talfail)
    | _ -> inst_form ctxt "Vcase requires sum kind"; raise Talfail

(* end LX instructions *)

(*** Verify an instruction ***)

(* add_block is called whenever an instruction does a jump to a label
 * that has no type in the current context.
 *)
let rec verify_instr add_block ctxt i =
  let ctxt = set_cc ctxt CCnoinfo in

  match i with
  (* Generic x86 instructions *)
    ArithBin(ab,g1,g2) -> verify_ArithBin ctxt ab g1 g2
  | ArithUn(au,g) -> verify_ArithUn ctxt au g
  | ArithMD(amd,g) -> verify_ArithMD ctxt amd g
  | ArithSR(sr,g,iopt) -> verify_ArithSR ctxt sr g iopt
  | Bswap(r) -> verify_Bswap ctxt r
  | Call(gc) -> verify_Call ctxt gc
  | Clc -> ctxt
  | Cmc -> ctxt
  | Cmovcc(c,r,gopt) -> verify_Cmovcc ctxt c r gopt
  | Cmp(g1,g2) -> verify_Cmp ctxt g1 g2
  | Conv(c) -> verify_Conv ctxt c
  | Imul3(r,g,i) -> verify_Imul3 ctxt r g i
  | Int i -> inst_form ctxt "interrupt unimplemented"; raise Talfail
  | Into -> inst_form ctxt "interrupt on overflow unimplemented"; raise Talfail
  | Jcc (c,lc,iopt) -> verify_Jcc verify_instr add_block ctxt c lc iopt
  | Jecxz (lc,iopt) -> verify_Jecxz verify_instr add_block ctxt lc iopt
  | Jmp gc -> verify_Jmp add_block ctxt gc
  | Lahf -> restore_cc ctxt; add_reg ctxt Eax cbyte4
  | Lea (r,g) -> verify_Lea ctxt r g
  | Loopd (lc,bo) -> verify_Loop add_block ctxt lc bo
  | Mov (gop,gcop) -> verify_Mov ctxt gop gcop
  | Movpart (se,gop1,p1,gop2,p2) -> verify_Movpart ctxt se gop1 p1 gop2 p2
  | Nop -> restore_cc ctxt; ctxt
  | Pop genop -> verify_Pop ctxt genop
  | Popad -> verify_Popad ctxt
  | Popfd -> verify_Popfd ctxt
  | Push gc -> verify_Push ctxt gc
  | Pushad -> verify_Pushad ctxt
  | Pushfd -> verify_Pushfd ctxt
  | Rdtsc -> verify_Rdtsc ctxt
  | Retn iopt -> verify_Retn ctxt iopt
  | Sahf -> leqcon ctxt (get_reg_con ctxt Eax) cbyte4; ctxt
  | Setcc (c,g) -> verify_Setcc ctxt c g
  | Shld (gop,r,iopt) -> verify_Shld_Shrd ctxt gop r iopt
  | Shrd (gop,r,iopt) -> verify_Shld_Shrd ctxt gop r iopt
  | Stc -> ctxt
  | Test (gop1,gop2) -> verify_Test ctxt gop1 gop2
  | Xchg (gop,r) -> verify_Xchg ctxt gop r
  (* TAL specific instructions *)
  | Coerce rc -> verify_Coerce ctxt rc
  | CoerceName nc -> verify_CoerceName ctxt nc
  | Comment _ -> restore_cc ctxt; ctxt
  | Fallthru cs -> restore_cc ctxt; raise (Fall_Thru (ctxt,cs))
  | Malloc (x,i,maopt) -> verify_Malloc ctxt x i maopt
  | Proof pf -> verify_Proof ctxt pf
  | Unpack (a,r,gc) -> verify_Unpack ctxt a r gc
  | Sunpack (a,g) -> verify_Sunpack ctxt a g
  | Nameobj (x,g) -> verify_Nameobj ctxt x g
  | ForgetUnique x -> verify_ForgetUnique ctxt x
  | RemoveName x -> verify_RemoveName ctxt x
  | FPnoargs op -> verify_FPnoargs ctxt op
  | FPsomeargs (op,args) -> verify_FPsomeargs ctxt op args
(* Cyclone *)
  | CgStart (x,c) ->
      begin
	let c = local_subst ctxt c in
        let (k,c) = check ctxt c in
        kindleq ctxt k k4byte;
	let newcon = ctrgn(c,Some c,[]) in
	(* The following is stolen from verify_Malloc.  
	   If that changes this should too. *)
	let (ctxt,x) = supply_fresh_tvar ctxt x in
	let xcon = cvar x in
	let ctxt = add_var ctxt x kname in
	let namecon = cname xcon in
	let ctxt = add_name ctxt x (Unique,newcon) in
	let ms = get_machine_state ctxt in
	let ms = ms_del_regs ms [Eax] in
	let ctxt = set_machine_state ctxt (ms_set_reg ms Eax namecon) in
	ctxt
      end
  | CgDump (r1,i1,r2,i2) ->
      begin
	let ctxt = set_verify_ctxt ctxt "CGDUMP" in
	let (cg_nm,cg_con) = get_unique_reg_con ctxt r1 in

	let (pre,post_opt,t) = 
	  match cg_con.rcon with
	  | Ctrgn(pre,post_opt,t) -> (pre,post_opt,t) 
	  | _ -> (inst_form ctxt "CGDUMP with bad cg region"; raise Talfail) in

	let (beg_con,end_con_opt,labels,holes) =
	  match (get_label_con ctxt i2).rcon with
          | Ctmpl (w,x,y,z) -> (w,x,y,z)
	  | _ -> (inst_form ctxt "CGDUMP of non-template"; raise Talfail) in

	(* The following leqcon check looks like its going in the wrong 
	 * direction but its not.  Let me explain. 
	   
	   The post-condition of both templates and code-regions is not a 
	   machine state, it is a type of a code pointer.  Usually of the
	   form All [....].code {....}.

	   Because "code" refers to a pointer the sense of the subtyping 
	   relation is flipped. For machine states the following holds 
	            {EAX : B4} <= {}.  
	   (From the state on the left we can always jump to the state on the 
	   right.)
	   But the followings fails to hold for code pointers 
	            code {EAX: B4} !<= code {}.
	   (A code pointer with the type on the left cannot be coerced into a
	   a code pointer with the type on the right, because the RHS type 
	   has a weaker pre-condition.)
	   *)
	(match post_opt with
	| None -> ()
	| Some post -> leqcon ctxt beg_con post);

	let (ctxt,i1) = supply_fresh_tvar ctxt i1 in
	let ctxt = add_var ctxt i1 kname in
        let ctxt = add_reg ctxt r2 (ctptr i1) in
	let ctxt = change_name ctxt cg_nm
	    (Unique,ctrgn(pre,end_con_opt,(i1,labels,holes)::t)) in
	ctxt
      end
(* TO FIX: We don't check that holes and fills match up properly,
   that is, CgHole should match up with CgFill, and CgHoleBtagi should
   match up with CgFillBtagi.  We expect this won't be necessary in the
   new assembler (there will only be one kind of hole and fill).
   It's inconvenient to do now with MASM.
*)
  | CgHole (r,i1,i2) ->
      begin (* Hole i2 in template i1 *)
        match (get_label_con ctxt i1).rcon with
          Ctmpl(beg_con,end_con,labels,holes) ->
            begin
              try 
                let hole_con = List.assoc i2 holes in
                genop_write_at ctxt (Reg r) hole_con
              with Not_found ->
                (inst_form ctxt
                   "labels in cghole don't identify a hole in a template";
                 ctxt)
            end
        | _ -> (inst_form ctxt
                  "cghole second argument must be the label of a template";
                ctxt)
      end
  | CgHoleJmp(l,(lab,c)) ->
      verify_Jmp add_block ctxt (Addr lab,c)
  | CgHoleJcc(cc,l,lc,iopt) ->
      verify_Jcc verify_instr add_block ctxt cc lc iopt
  | CgFill (r1,r2,tmpl,hole,r3) -> verify_CgFill ctxt r1 r2 r3 tmpl hole
  | CgFillJmp(r0,r1,l1a,l1b,r2,l2a,l2b) ->
      verify_CgFillJmp ctxt r0 r1 l1a l1b r2 l2a l2b
  | CgFillJcc(r0,r1,l1a,l1b,r2,l2a,l2b) ->
      (* Same verification conditions as CgFillJmp.
	 Why are there two instructions? *)
      verify_CgFillJmp ctxt r0 r1 l1a l1b r2 l2a l2b
  | CgForget (cg_nm,tp_v) ->
      begin
	let cg_con = get_name_type ctxt cg_nm in
	let err ctxt i msg= 
	  (inst_form ctxt ("cgforget "^(string_of_int i)^": "^msg); ctxt) 
	in
        match cg_con.rcon with
          Ctrgn(pre,post,t) ->
	    let (t_elim,t_keep) = List.partition (fun (i',_,_) -> tp_v=i') t in
	    let ctxt = 
	      match t_elim with
		[] -> err ctxt 1 "no such template in region"
	      | [(_,_,[])] -> ctxt
	      | [(_,_,_)] -> err ctxt 2 "template contains holes"
	      | _ -> err ctxt 3 "Multiple templates have the same name?"
	    in
            change_name ctxt cg_nm (Unique,ctrgn (pre,post,t_keep))
        | _ -> err ctxt 4 "Name does not correspond to a code gen. region."
      end
  | CgEnd r ->
      begin
	let err ctxt i msg = 
	  (inst_form ctxt ("cgend "^(string_of_int i)^": "^msg); ctxt)
	in
	let (cg_nm,cg_con) = get_unique_reg_con ctxt r in
	
        match cg_con.rcon with
          Ctrgn(pre,None,t) ->
            if List.exists (function (_,_,[]) -> false | _ -> true) t
            then err ctxt 2 "Region still contains holes."
            else
	      begin
		let ctxt = remove_name ctxt cg_nm in		
		add_reg ctxt r pre
	      end
	| Ctrgn(pre,_,t) -> err ctxt 1 "Region post condition is not false."
        | _ -> err ctxt 1 "Register is not a code generation region."
      end
(* End Cyclone *)
(* -- LX -- *)
  | Letprod (is,c) -> verify_Letprod ctxt is c
  | Letroll (i,c) -> verify_Letroll ctxt i c
  | Vcase (i,c,a,gc) -> verify_Vcase ctxt (int32_to_int i) c a gc
(* end LX *)

;;

(**********************************************************************)    
(* Code Blocks and Code Trees                                         *)
(**********************************************************************)

(* Add the code labels to psi:
 * Check and normalise the constructor and check it has kind K4byte
 * (if present).
 *)
let add_code_labels ctxt cbv mode =
  let aux ctxt (l,c,_) =
    let ctxt = 
      set_verify_ctxt (set_loc ctxt (Locc (l,-1))) "adding code labels" in
   (match c with
     None -> add_val ctxt l None mode
   | Some c -> 
     let (k,c') = check ctxt c in
     kindleq ctxt k k4byte; add_val ctxt l (Some c') mode) in
  vector_fold aux ctxt cbv
;;

(* We need to verify a code tree starting from a given label (with a type).
 * We keep a list, undone_tree_blocks, of code blocks that have yet to be
 * verified.  The list includes the label of the block, the instructions,
 * the index (in the code block vector) of the code block, and the ctxt
 * under which the instructions should be checked.  The index is used to
 * verify that branches to "untyped" labels are forward branches in order
 * to ensure termination (i.e., that we have no cycles.)
 *
 * We start by adding the appropriate context, index, label, and instructions
 * of the typed block.  We then verify each instruction in that block which
 * in turn may add more blocks to the undone_tree_blocks list (see verify_
 * instruction).  Special care must be taken when a block falls through to 
 * the next block.  
 *)
(* CYCLONE
 * All this is complicated by RTCG
 * Each template is stored in a different code-block vector (cbv), and therefore
 * code-trees cannot cross template boundaries.
 * Unfortunately jumps from a template to an absolute address are perfectly okay
 * but will fail to type-check because they cross a template boundary.
 * An easy fix would be to add labels to addresses targeted by jumps from
 * inside a template. This doesn't work for nullFailureLabel because there
 * is no type we can put on this label.
 * To work around all these problems, verify_code_tree had to be restructured.
 * It now returns a list of blocks that are not in this cbv to be handled later.
 * This list is always empty except when we are in a template.
 * If we are in a template, the block_mapping contains numbers for blocks in 
 * the template (in the range 0...|cbv|) and blocks at the top level (in the 
 * range |cbv|...# top-level blocks + |cbv|).  Undoable blocks come from the 
 * latter range.  They are verified as soon as the template is completed, but 
 * we pass in an adjustment (offset = |cbv|) to verify_code_tree_work, so that 
 * it can recover the positions of the blocks in top-most code-block vector.
 *)
(* FMS: Split verify_code_tree into two functions.
   verify_code_tree does as before but returns a list of blocks it could not handle. (These blocks occur in a later vector of code_blocks.)
   verify_code_tree_work takes a list of unprocessed blocks and checks them 
   The offset is for adjusting indices between different cbv's *)
let verify_code_tree_work block_mapping cbv todo_blocks offset post_check
   =
  let adjust (ctxt,l,i) =
    let i = i - offset in
    let (_,_,inst) = cbv.(i) in
    (ctxt,l,i,inst)                             in
  let todo_blocks = List.map adjust todo_blocks in
  let undone_tree_blocks = ref todo_blocks      in
  let undoable_tree_blocks = ref [] in (* Blocks from another later cbv *)
(*  debug("TREE ROOT: "^(id_to_string l));*)
  let add_block current_index ctxt l = 
    let block_index = 
      try Dict.lookup block_mapping l
      with Dict.Absent -> 
	(generate_error ctxt (Undefined_label l); 
	 raise Talfail) in
    if current_index < block_index then
      (if block_index > Array.length(cbv) then 
	undoable_tree_blocks := (ctxt,l,block_index) :: (!undoable_tree_blocks)
      else 
	let (_,_,insts) = cbv.(block_index) in
	undone_tree_blocks := (ctxt,l,block_index,insts)::(!undone_tree_blocks))
    else generate_error ctxt (Backward_branch l) in
  let verify_block (ctxt,l,current_index,insts) =
    let lbl = id_to_string l in
    debug("verifying block "^(id_to_string l));
    let add_block = add_block current_index in
    let ctxt = set_verify_ctxt ctxt "verifying instructions" in
    let fti = ref 0 in
    (* Verify a vector of instructions.  "Normal" termination is handled
     * when a Terminal_Jmp exception is raised.  If the code falls through
     * to the next block, then we must check that:
     *  (i) the fall through is the last instruction of the block
     *  (ii) "jumping" to the next block is legal in the current context.
     * Note that it is now possible to fall through to the next block without
     * an explicit "Fallthru" instruction when the next block has no type.
     * It is also possible to explicitly Fallthru in this situation.  However,
     * If the next block is labelled with a type that has bound type 
     * variables, then an explicit Fallthru (with instantiation) is 
     * necessary.
     *)
    let rec inst_loop ctxt n i =
      let ctxt = set_loc ctxt (Locc (l,i)) in
      (* debug("verifying instruction "^(string_of_int i)); *)
      fti := i;
      if i = n then raise (Fall_Thru(ctxt,[])) else
      let ctxt = verify_instr add_block ctxt insts.(i) in
      inst_loop ctxt n (i+1) in
    try
      inst_loop ctxt (Array.length insts) 0
    with 
      Terminal_Jmp -> ()
    | Talfail -> ()
    | Fall_Thru(ctxt,cs) ->
	if !fti < (Array.length insts - 1) then
	  begin
	    generate_error ctxt (Inst_form "fall through in middle of block");
	    ()
	  end
    	else if current_index = (Array.length cbv - 1) then
	  begin
(* Cyclone *)
            match post_check with
              None ->
(* End Cyclone *)
                generate_error ctxt (Inst_form "falls off end of code"); ()
(* Cyclone *)
            | Some(None) ->
		generate_error ctxt (Inst_form "falls off end of template"); ()
            | Some(Some post) ->
		let subst_f = local_subst ctxt in
                let post =
	          List.fold_right 
		    (fun c co -> coercion_con ctxt c co subst_f)
                    (List.rev(List.map (fun x -> Tapp (Con x)) cs)) post in
                leqcon ctxt post (ccode_ms(get_machine_state ctxt));
                ()
(* End Cyclone *)
	  end
	else 
	  begin
	    let (l,_,_) = cbv.(current_index + 1) in
	    let lc = List.fold_left (fun gc c -> coerce gc (Tapp (Con c)))
		(raw (Addr l)) cs in
	    try verify_instr add_block ctxt (Jmp lc); ()
	    with 
	      Terminal_Jmp -> ()
	    | Talfail -> ()
	  end
    | exn -> raise exn
  in let rec verify_blocks () = 
    (match !undone_tree_blocks with
      [] -> ()
    | b::rest -> ((undone_tree_blocks := rest); 
		  verify_block b; 
		  verify_blocks()))
  in 
  verify_blocks();
  !undoable_tree_blocks
;;


let verify_code_tree block_mapping cbv ctxt i post_check =
  let (l,copt,_) = cbv.(i) in
  let c = get_label_con ctxt l in
  let ctxt = set_loc ctxt (Locc (l,-1)) in
  let (vks,prop,msc) = separate_fun_type ctxt c in
  let f ctxt (v,k) = 
     let k = check_kind ctxt k in
     add_var ctxt v k in
  let ctxt = List.fold_left f ctxt vks in
  let gamma = dms ctxt (snd (check ctxt msc)) in
  let ctxt = set_machine_state ctxt gamma in
  let ctxt = set_prop ctxt prop in
  try 
    verify_code_tree_work block_mapping cbv [(ctxt,l,i)] 0 post_check
  with exn ->
    let ctxt = set_loc ctxt (Locc (l,i)) in
    generate_error ctxt (Inst_form "uncaught exception"); raise exn
    
  ;;

(* We need a map from labels to integer indices so that
 * verify_code_tree can easily determine when a branch is forward or not.
 * All of the smarts regarding fallthrus are now encapsulated in the code
 * trees.  
 *)  
let compute_block_mapping cbv extra_cbv = 
  let rec add_label cbv i d = 
    if i >= Array.length(cbv) then d
    else let (l,_,_) = cbv.(i) in 
    add_label cbv (i+1) (Dict.insert d l i)
  in 
  add_label (Array.append cbv extra_cbv) 0 (Dict.empty id_compare)
;;

(* Verify each of the code blocks.
 * Note that we only verify code trees that start with a typed label.
 * Hence, the verifier no longer has the property that it can detect
 * dead code.  In particular, completely random code will type-check
 * as long as none of the labels have types (!) because those labels
 * cannot be exported or jumped to etc.
 *)
(* let dummy_cb = (id_new "", None, [||])  *)
(* FMS: block_mapping is now computed above, needed for RTCG. *)
let verify_code_blocks ctxt block_mapping cbv
(* Cyclone *)
   post_check
(* End Cyclone *)
   =
  (* used to map labels to the block index in cbv *)
  let ctxt = set_loc ctxt Loctop in
  let ctxt = set_verify_ctxt ctxt "verifying code blocks" in
  (* specialize verify_code_tree to the current block mapping, 
   * code-block vector, and outer context.
   *)
  let verify_ctree i = verify_code_tree block_mapping cbv ctxt i
(* Cyclone *)
      post_check
(* End Cyclone *)
  in
  (* For each block, if the block has a type then it's the 
   * beginning of a code-tree to be verified.  Otherwise, the
   * block is skipped.  *)
  let rec loop i accum =
    if i >= Array.length(cbv) then accum
    else 
      match cbv.(i) with
	(_,None,_) -> loop (i+1) accum
      | (l,Some c,insts) -> loop (i+1) ((verify_ctree i) @ accum)
  in
  loop 0 []

(**********************************************************************)    
(* Data Blocks                                                        *)
(**********************************************************************)

(* Infer the type of a data label:
 * For now just look at last coercion and grab any obvious type otherwise fail
 *)
let infer_data_type ctxt l dis clist =
  match clist with
  | (Pack (_,c))::_ -> c
  | (Roll c)::_ -> c
  | (Tosum c)::_ -> c
  | (RollTosum c)::_ -> c
  | (Subsume c)::_ -> c
  | _ -> generate_error ctxt (Data_form "needs label type"); raise Talfail
;;

(* Add the data labels to psi:
 * Check and normalise the constructor and check it has kind K4byte
 *)
let add_data_labels ctxt dbv =
  let aux ctxt (l,_,co,(dis,clist)) =
    let ctxt =
      set_verify_ctxt (set_loc ctxt (Locd (l,-1))) ("adding data labels") in
    let c = 
      match co with
	None -> infer_data_type ctxt l dis clist
      |	Some c -> c in
    let (k,c') = check ctxt c in
    kindleq ctxt k ktype; 
    add_val ctxt l (Some c') Abs in
  vector_fold aux ctxt dbv
;;

let rec myrep i n l =
  if n=0 then l else myrep i (n-1) (i::l)
;;

let verify_data_items l ctxt exps_int_type dis =
  let rec aux n saved cur dis =
    let ctxt = set_loc ctxt (Locd (l,n)) in
    match dis with
      [] ->
	(match saved with
	  [] -> List.rev cur
	| s::ss -> aux (n+1) ss ((cprod (List.rev cur))::s) dis)
    | (Dlabel cl)::dis ->
 	aux (n+1) saved ((cfield (coerce_label_con ctxt Abs cl) ReadWrite)::cur)
	  dis
    | (Dbytes (s))::dis ->
	 aux
	    (n+1)
	    saved 
	    (myrep (cfield (pcbytes Byte1) ReadWrite) (String.length s) cur)
	    dis
    | (D2bytes _)::dis ->
	aux (n+1) saved ((cfield (pcbytes Byte2) ReadWrite)::cur) dis
    | (D4bytes ci)::dis ->
	let aux' ctxt i = csing (pcint i) in
	aux (n+1) saved ((cfield (coerce_con aux' ctxt ci) ReadWrite)::cur) dis
    | (Dfloat32 s)::dis ->
	aux (n+1) saved ((cfield (pcfloat32) ReadWrite)::cur) dis
    | (Dfloat64 s)::dis -> 
	aux (n+1) saved ((cfield (pcfloat64) ReadWrite)::cur) dis
    | (Djunk)::dis ->
	aux (n+1) saved ((pcjunk i32_4)::cur) dis
    | (Drep (ci,s))::dis -> 
	 let go (* (old: 'a) 
	        (read: Stringchan.string_chan -> 'a)                     
	        (compare: 'a -> 'a -> unit)            
	        (return: 'a -> rep_item)   
	        (emit: Buffer.t -> 'a -> unit) *)
	       old read compare return emit
	       = 
	    match !s with 
	       Some s -> 
		  let obj = read (Stringchan.from_string s) in 
		  compare old obj;
		  return obj
	     | None -> 
		  let buf = Buffer.create 100 in 
		  emit buf old;
		  let str = Buffer.contents buf in 
		  s:= Some str;
		  return old in 
	 let ci = match ci with 
	    RCon c ->
	       go (substs (get_kindabbrevs ctxt, get_abbrevs ctxt) c)
		  Talbinin.Str.read_in_con 
		  (eqcon empty_ctxt) 
		  (fun c -> RCon c)
		  Talbinout.Buf.emit_out_con
	  | RKind k -> 
	       go (ksubsts (get_kindabbrevs ctxt) k)
		  Talbinin.Str.read_in_kind 
		  (kindeq empty_ctxt) 
		  (fun k -> RKind k) 
		  Talbinout.Buf.emit_out_kind
	  | RLabel l -> 
	       go l 
		  Talbinin.Str.read_in_label 
		  (fun ctxt l1 l2 -> Identifier.id_compare l1 l2 == 0)
		  (fun l -> RLabel l)
		  Talbinout.Buf.emit_out_label
	 in 
	 aux (n+1) saved ((cr ci)::cur) dis
    | (Dup)::dis -> aux (n+1) (cur::saved) [] dis
    | (Ddown)::dis ->
	(match saved with
	  [] ->
	    generate_error ctxt (Data_form "too many tal_ends"); raise Talfail
	| s::ss -> aux (n+1) ss ((cprod (List.rev cur))::s) dis) in
  cprod (aux 0 [] [] dis)
;;

let rec get_tag_type ctxt c dis =
  match (whnorm ctxt c).rcon with
    Chptr (_,_,Some (c,v)) ->
      let (_,c) = check (clear_vars ctxt) c in
      let rec aux dis =
	match dis with
	  [] ->
	    generate_error ctxt (Data_form "zero length block tagging type")
	| (Dbytes (s))::_ when (String.length s)>0 -> ()
	| (Dbytes (s))::dis -> aux dis
	| (Drep _)::dis -> ()
	| (Dlabel _|D2bytes _|D4bytes _|Djunk|Dfloat32 _|Dfloat64 _)::_ -> ()
	| (Dup)::dis -> aux dis
	| (Ddown)::dis -> aux dis in
      aux dis;
      Some (c,v)
  | Cexist (a,k,_,c) -> get_tag_type ctxt c dis
  | _ -> None
;;
    	
let verify_data_block ctxt exps_int_type (l,_,co,(dis,clist)) = 
  let ctxt =
    set_verify_ctxt (set_loc ctxt (Locd (l,-1))) "verifying data blocks" in
  let tco =
    match co with
      Some c -> get_tag_type ctxt (snd (check ctxt c)) dis
    | _ -> None in
  let ctxt = set_verify_ctxt ctxt "verifying data items" in
  let c1 = chptr [] (Some (verify_data_items l ctxt exps_int_type dis)) tco in
  let ctxt = set_verify_ctxt ctxt "verifying coercions" in
  let subst_f = local_subst ctxt in
  let c1 = List.fold_right (fun c c1 -> 
    coercion_con ctxt c c1 subst_f) clist c1 in
  let ctxt = set_verify_ctxt ctxt "verifying label type" in
  match co with
    None -> c1
  | Some c2 -> let (_,c2) = check ctxt c2 in leqcon ctxt c1 c2; c2
;;

let verify_data_blocks ctxt exps_int_type dbv =
  for i=0 to Array.length dbv - 1 do
    try 
      verify_data_block ctxt exps_int_type dbv.(i); ()
    with Talfail -> ()
  done
;;

(* Cyclone *)

(**********************************************************************)    
(* Templates                                                          *)
(**********************************************************************)

(* Add the template labels to psi:
 * Check and normalise the constructor and check it has kind K4byte
 *)
let add_template_labels ctxt templatev =
  let aux ctxt (template_begin_label,
                c,
                cbs) =
    let ctxt =
      set_verify_ctxt
        (set_loc ctxt (Locc (template_begin_label,-1)))
        "adding templates" in
    let (k,c') = check ctxt c in
    kindleq ctxt k k4byte;
    (* Gather actual labels and holes from body of template *)
    let labels1 =
      List.concat
        (List.map
           (function (l,Some c,_) -> [(l,c)]
             | _ -> [])
           cbs) in
    let holes1 = 
      List.concat
        (List.map
           (fun (_,_,is) -> 
              List.concat(
                List.map
                  (fun i ->
                     match i with
                       CgHole(r,lt,lh) -> [lh]
                     | CgHoleJmp(temp,(hole,c)) -> [hole]
                     | CgHoleJcc(cc,temp,(hole,c),_) -> [hole]
                     | _ -> [])
                  (Array.to_list is)))
           cbs) in
    begin
      match c'.rcon with
        (* Extract the labels and holes from the template type *)
        Ctmpl(_,_,labels2,holes2) ->
          begin
            (* Make sure the actual labels of the template match up with
               the labels in the type of the template *)
	    let labels1 = 
	      Sort.list (fun (x1,y1) (x2,y2) -> compare x1 x2 >= 0) labels1 in
	    let labels2 = 
	      Sort.list (fun (x1,y1) (x2,y2) -> (compare x1 x2) >= 0) labels2 in
	    let rec loop ls1 ls2 = 
	      match (ls1, ls2) with
		(((l1,c1)::tl1), (((l2,c2)::tl2) as ls2')) ->
		  if compare l1 l2 <> 0 then loop tl1 ls2' 
		  else begin
                    let (k1,c1) = check ctxt c1 in
                   (* c2 was checked before, but now we need to compare
                      ** it to c1. *)
                    let (k2,c2) = check ctxt c2 in
                    kindeq ctxt k1 k2;
                    eqcon ctxt c1 c2;
		    loop tl1 tl2
		  end
	      |	(_,[]) -> () (* more labels in the template code okay. *)
	      |	([],_) -> 
		  inst_form ctxt ("Found labels in the template type " ^
				  "not present in the code.")
	    in
	    loop labels1 labels2;

            (* Check that each hole in the body of the template appears
            ** in the type of the template.  The kind of each hole in
            ** the template type has been checked already; its type
            ** will be checked later. *)
	    let rec loop holesL holesT =
	      match (holesL,holesT) with
		([],[]) -> ()
	      | ([],_)  ->
		  inst_form ctxt "Holes in template type do not occur in body"
	      | (_,[])  ->
		  inst_form ctxt "Holes in template body do not occur in type"
	      | (h1::t1,(h2,_)::t2) ->
		  if (id_compare h1 h2)<>0 then
		    inst_form ctxt (  "Hole ("
				    ^ (id_to_string h1)
				    ^ ") in body does not match hole ("
				    ^ (id_to_string h2)
				    ^ ") in type"
				    );
		  loop t1 t2
	    in
	    let holes1 = Sort.list (fun h1 h2 -> id_compare h1 h2 <= 0) holes1 in
	    let holes2 = Sort.list (fun (h1,c1) (h2,c2) -> id_compare h1 h2<=0) holes2 in
	    loop holes1 holes2
          end
      | _ -> inst_form ctxt "template does not have template type"
    end;
    add_val ctxt template_begin_label (Some c') Abs
  in
  vector_fold aux ctxt templatev
;;

let verify_template ctxt block_mapping l cbv =
  (* CAREFUL: get template_con from ctxt, because add_template expands
  ** any type abbrevs before putting it in ctxt. *)
  let template_con = get_label_con ctxt l in
  (* Get the pre- and post-conditions from the template type.  Add the
     labels and holes given by the template type to the context; note
     that this ensures that the labels and holes are distinct. *)
  (* AFTER TALC-1.6:
     The types of the labels are taken from the code blocks and not the
     template con because for some reason the template con is alpha
     converted.
  *)
  let (pre,post_opt,ctxt) =
    match template_con.rcon with
      Ctmpl(pre,post_opt,labels,holes) ->
        (pre,
         post_opt,
         List.fold_left
           (fun ctxt (l,c) -> add_val ctxt l (Some c) Rel)
           (add_code_labels ctxt cbv Rel) (* take label cons from cbv *)
           holes)
    | _ ->
      begin
        inst_form ctxt "template does not have template type";
        raise Talfail
      end
  in
  let ctxt = set_mode ctxt Rel in
  let undone_blocks = 
    verify_code_blocks ctxt block_mapping cbv (Some(post_opt)) in
  (* Check that precondition holds of first label *)
  (try
    let (_,first_label_con_opt,_) = cbv.(0) in
    match first_label_con_opt with
      None ->
        inst_form ctxt "first label of template does not have a type";
        raise Talfail
    | Some con -> 
        let (k,c) = check ctxt con in
        kindleq ctxt k k4byte;
        leqcon ctxt c pre
  with Invalid_argument _ -> ()); (* template is empty *)
  undone_blocks
;;

let verify_templates ctxt block_mapping imp =
  let tv = imp.templates in
  for i=0 to Array.length tv - 1 do
    try
      let (l,_,cbs) = tv.(i) in
      let ctxt = set_loc ctxt Loctop in
      let cbv = Array.of_list cbs in
      let block_mapping' = compute_block_mapping cbv imp.code_blocks in
      let undone_blocks = verify_template ctxt block_mapping' l cbv in
      let still_undone = 
	verify_code_tree_work block_mapping' imp.code_blocks undone_blocks
	  (Array.length cbv) None in
      if still_undone <> [] then 
	(generate_error ctxt 
	   (Inst_form "BOGUS error form: Uncheckable blocks.")) 
    with Talfail -> ()
  done
;;
(* End Cyclone *)

(**********************************************************************)    
(* Interfaces/Implementations                                         *)
(**********************************************************************)

type ref2int = int_ref -> tal_int

(*** Type Abbreviation Processing ***)

let process_kindabbrevs ctxt abbrevs = 
   let process_abbrev ctxt (l,k) = 
      let k = check_kind (set_loc ctxt (Lockind l)) k in
      add_kindabbrev ctxt l k in
   Array.fold_left process_abbrev ctxt abbrevs
;;

let process_abbrevs ctxt abbrevs = 
   let process_abbrev ctxt (l,c) = 
     let c = (snd (check (set_loc ctxt (Loccb l)) c)) in 
     add_abbrev ctxt l c 
  in
  Array.fold_left process_abbrev ctxt abbrevs
;;

(*** For building phi from con blocks and imported cons ***)

let add_con_blocks ctxt lkcs =
  let aux ctxt (l,k,_) =
    add_con (set_loc ctxt (Loccon l)) l k in
  Array.fold_left aux ctxt lkcs
;;

(* verify that a con-definition is well-formed under ctxt0 and then
 * add it to ctxt. 
 *)
let verify_import_con_def ctxt0 ctxt (x,k,cd) = 
  let k = check_kind ctxt0 k in
  let cd = 
    match cd with
      AbsCon -> cd  (* always well-formed *)
    | BoundCon c -> 
	let (k',c) = check ctxt0 c in
	kindleq ctxt0 k' k;
	BoundCon c
    | ConcCon c ->
	let (k',c) = check ctxt0 c in
	kindleq ctxt0 k' k;
	ConcCon c
  in
  (add_con_def ctxt x cd, (x,k,cd))
;;

(* verify and enter an imported/exported label's type into the context *)
let add_impexp_val ctxt0 ctxt (l,c) =
  let ctxt0 = set_loc ctxt0 (Locval l) in
  let (k,c) = check ctxt0 c in
  kindleq ctxt0 k k4byte;
  (add_val ctxt l (Some c) Abs, (l,c))
;;

(* given an import reference, read in the associated interface,
 * check that the interface is well-formed in an empty context,
 * and then build a new context corresponding to the interface
 * with the l:k<=c, l:k=c, x=c, and l:c mappings updated.
 *)
let process_import_export ctxt0 ctxt int =
  let ctxt0 = set_verify_ctxt ctxt0 "verifying interface" in
  (* verify and add each abbreviation to the context -- note that 
   * abbrevs can refer to the labels entered previously and to each other
   * in a left-to-right fashion. *)

  let ctxt0 = process_kindabbrevs ctxt0 int.int_kindabbrevs in
  let ctxt0 = process_abbrevs ctxt0 int.int_abbrevs in

  let ctxt = add_kindabbrevs ctxt (get_kindabbrevs ctxt0) in
  let ctxt = add_abbrevs ctxt (get_abbrevs ctxt0) in
  (* verify and add each l=c definition to the context -- note that the label
   * definitions (c) can refer to the abbreviations.  *)
  
  let cr = ref ctxt in
  for i=0 to (Array.length int.int_cons) - 1 do
    let (ctxt,cd) = verify_import_con_def ctxt0 (!cr) int.int_cons.(i) in
    int.int_cons.(i) <- cd;
    cr := ctxt
  done;
  for i=0 to (Array.length int.int_vals) - 1 do
    let (ctxt,lc) = add_impexp_val ctxt0 (!cr) int.int_vals.(i) in
    int.int_vals.(i) <- lc;
    cr := ctxt
  done;
  !cr
(*
  let ctxt = Array.fold_left (verify_import_con_def ctxt0) ctxt int.int_cons in
  (* verify and add each value label l's type c to the context -- 
   * note that the label's type (c) can refer to abbreviations or 
   * label types entered above. *)
  let ctxt = Array.fold_left (add_impexp_val ctxt0) ctxt int.int_vals in
  ctxt
*)
;;

(* given an array of import or export interfaces, verify that each is 
 * well-formed, under ctxt, and accumulate a context with all of the
 * bindings from those interfaces -- return the accumulated context.
 *)
let verify_imports_exports ctxt impexps = 
  Array.fold_left (process_import_export ctxt) ctxt impexps
;;

(* from a list of interfaces, get all of the con definitions and values. *)
let get_int_type impexps = 
  let (impexp_cons,impexp_vals) = 
    Array.fold_left (fun (cons,vals) impexp -> 
      (vector_append impexp.int_cons cons,
       vector_append impexp.int_vals vals)) ([],[]) impexps in
  {it_cons=impexp_cons;it_vals=impexp_vals}
;;
  

(*** For verifing the con defs used in term level processing ***)

let verify_con_block ctxt (l,k,c) =
   let ctxt = set_loc ctxt (Loccb l) in
   let (k',c') = check ctxt c in
   kindleq ctxt k' k;
   add_con_def ctxt l (ConcCon c')
;;

(*** Verifing Exports ***)
let verify_exports ctxte ctxti exps = 
  (* we've already checked that the interfaces are well-formed -- we just
   * need to check that they agree with the implementation. 
   *)
  (* check an export interface to see that it agrees with the implementation
   * context ctxti *)
  debug("verifying exports");
  let ctxte = set_verify_ctxt ctxte "verifying exports" in
  let ctxti = set_verify_ctxt ctxti "verifying exports" in
  let check_export exp_int = 
    (* check that an exported constructor definition agrees with ctxti *)
    let check_exp_con (l,k,cd as lkcd) = 
      let ctxti = set_loc ctxti (Loccon l) in
      let ctxte = set_loc ctxte (Loccon l) in
      kindleq ctxti (get_label_kind ctxti l) k;
      match cd with
	 AbsCon -> ()
       | BoundCon c ->
	  (match get_label_def ctxti l with
	    AbsCon | BoundCon _ -> 
	      (* this shouldn't happen as conblocks are added concrete *)
	      failwith "Talverify.verify_exports - internal error 1"
	  | ConcCon c2 -> leqcon ctxti c2 c
		  )
      |	ConcCon c ->
	   (match get_label_def ctxti l with
	     AbsCon | BoundCon _ -> 
	      (* this shouldn't happen as conblocks are added concrete *)
	       failwith "Talverify.verify_exports - internal error 2"
	  | ConcCon c2 -> eqcon ctxti c2 c)
    in
    (* check that an exported value's type agrees with the implementation *)
    let check_exp_val (l,c) = 
      let ctxti = set_loc ctxti (Locval l) in
      let ctxte = set_loc ctxte (Locval l) in
      kindleq ctxte (con_kind ctxte c) k4byte;
      leqcon ctxti (get_label_con ctxti l) c in
    (* check all exported cons and values in the export interface *)
    Array.iter check_exp_con exp_int.int_cons;
    Array.iter check_exp_val exp_int.int_vals in
  (* check all exported interfaces *)
  Array.iter check_export exps
;;

(* add all of the con labels that appear in the import interface(s)
 * and the export interface(s).
 *)
let add_all_con_labels ctxt imps exps = 
  Array.fold_left 
    (fun ctxt exp -> add_con_blocks ctxt exp.int_cons)
    (Array.fold_left 
       (fun ctxt imp -> add_con_blocks ctxt imp.int_cons) ctxt imps) exps
;;

let add_local_con_blocks ctxt exported_labels defined_labels = 
  let is_exported l = 
    List.exists (fun (x,_,_) -> (id_compare x l) = 0) exported_labels in
  let f ctxt (l,k,_) = 
    let ctxt = set_loc ctxt (Loccon l) in
    if is_exported l then ctxt else add_con ctxt l k
  in Array.fold_left f ctxt defined_labels
;;

(* Takes an initial context and a module and returns two new contexts.
   One for checking the implementation, and the other for checking that
   the exports match the export interface. *)
let init_ctxt ctxt0 tal_mod =     
  let imps = tal_mod.imports in
  let exps = tal_mod.exports in
  (* add all of the defined labels in the imports and exports 
   * to the context so their definitions can refer to one another. *)
  debug("adding all con labels\n");
  let ctxt = add_all_con_labels ctxt0 imps exps in
  (* hang on to this context as we need it for verifying that the
   * exports match the implementation *)
  let ctxte = ctxt in
  (* Verify exported interfaces are well-formed under ctxt1.  Ignore 
   * resulting accumulated context. *)
  debug("verifying exports\n");
  let _ = verify_imports_exports ctxt exps in
  (* Verify imported interfaces are well-formed under ctxt1, and add 
   * their abbreviations, con blocks, and value labels to the context 
   * yielding ctxt2. *)
  let ctxt = verify_imports_exports ctxt imps in
  (ctxt,ctxte)
;;

(*** Verifing Implementations ***)

let verify_imp ctxt (imps_int_type,exps_int_type) tal_imp =
  (* add con-blocks for the implementation that are not in the exports *)
  debug("adding local con blocks\n");
  let ctxt = 
    add_local_con_blocks ctxt exps_int_type.it_cons tal_imp.con_blocks in
  (* check and add abbrevs for the implementation -- these are allowed to
   * refer to imported abbrevs, imported labels types, and label types 
   * defined in the implementation. *)
  debug("doing kind abbrevs\n");
  let ctxt = process_kindabbrevs ctxt tal_imp.imp_kindabbrevs in
  debug("doing con abbrevs\n");
  let ctxt = process_abbrevs ctxt tal_imp.imp_abbrevs in
  (* check and add label constructor definitions for the implementation *)
  debug("adding label constructors definitions\n");
  let ctxt = Array.fold_left verify_con_block ctxt tal_imp.con_blocks in
  (* check and add value label types for the implementation *)
  debug("adding value label definitions\n");
  let ctxt = add_code_labels ctxt tal_imp.code_blocks Abs in
(*
  Talpp.print_ctxt Format.std_formatter Talpp.std_options ctxt;
  Format.print_newline ();
*)
  debug("adding data label definitions\n");
  let ctxt = add_data_labels ctxt tal_imp.data_blocks in
  (* Cyclone *)
  debug("adding templates label definitions\n");
  let ctxt = add_template_labels ctxt tal_imp.templates in
  (* verify templates *)
  debug("verifying templates label definitions\n");
  let block_mapping = compute_block_mapping tal_imp.code_blocks 
      (Array.init 0 (fun x -> failwith "Impossible")) in
  verify_templates ctxt block_mapping tal_imp;
  (* End Cyclone *)
  (* verify code blocks *)
  debug("verifying code definitions\n");
  let ctxt = set_mode ctxt Abs in
  let undone_blocks = 
    verify_code_blocks ctxt block_mapping tal_imp.code_blocks None in
  (match undone_blocks with 
  |	[] -> () 
  | _ -> generate_error ctxt (Inst_form "BOGUS error form: verify_code_blocks failed.");
      raise Talfail);
      (* verify data blocks *)
  debug("verifying data definitions\n");
  verify_data_blocks ctxt exps_int_type tal_imp.data_blocks;
  ctxt
;;


(********** Moved from tallinkchk to break circular dependency ********)

let get_program_interface ref2int imprefs exprefs : (tal_int_type * tal_int_type) =
  (* get interfaces from files *)
  let imps = Array.map ref2int imprefs in
  let exps = Array.map ref2int exprefs in
  (* add in all of the l:k info so we can check and expand abbreviations *)
  let ctxt = add_all_con_labels empty_ctxt imps exps in
  (* this has the side effect of expanding all abbreviations in the
   * export and import interfaces *)
  let _ = verify_imports_exports ctxt exps in
  let _ = verify_imports_exports ctxt imps in
  (* get the tal_int_type for the imports and exports *)
  let imp_it = get_int_type imps in
  let exp_it = get_int_type exps in
  (imp_it,exp_it)
;;


(**********************************************************************)    
(* EOF: talverify.ml                                                  *)
(**********************************************************************)

