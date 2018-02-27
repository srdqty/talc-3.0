(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* x86tal.mli
 * 
 * This is a fairly complete specification of the abstract syntax for
 * a typed version of 32-bit (flat-model) iNTEL 80386, Pentium, Pentium Pro,
 * and/or Pentium II assembly language.  
 *
 * TODO:  1. floating point
 *
 *)

(**********************************************************************)
(* Miscellanous stuff *)

open Utilities;;
open Numtypes;;
open Identifier;;

type mode = Abs | Rel
type scale = Byte1 | Byte2 | Byte4 | Byte8
let scale_to_int32 s =
   match s with
     Byte1 -> i32_1
   | Byte2 -> i32_2
   | Byte4 -> i32_4
   | Byte8 -> i32_8
;;
let scale_to_int s = int32_to_int(scale_to_int32 s)
;;

type reg = Eax | Ebx | Ecx | Edx | Esi | Edi | Ebp | Esp | Virt of identifier
let compare_regs r1 r2 =
  match r1 with
    Virt i1 ->
      (match r2 with
	Virt i2 ->
	  id_compare i1 i2
      |	_ -> 1)
  | _ ->
      (match r2 with
	Virt _ -> -1
      |	_ -> compare r1 r2)
;;

(* For part word stuff: e = 32bit, x = 16bit, h = "high" 8bit, l = low 8bit *)
type reg_part = RPe | RPx | RPh | RPl;;

(**********************************************************************)
(* Kinds *)

(* Kbyte <= Ktype, Kmemi <= Kmem *)
type rkind = 
    Kbyte of scale	  (* describes types of 8/16/32/64 bit values *)
  | Ktype   		  (* describes types of all values *)
  | Kmemi of int32        (* types for memory of size i *)
  | Kmem                  (* types for memory or heap blocks *)
  | Kstack  		  (* describes types of the stack & pointers into it *)
  | Kint                  (* integer kind *)
  | Kbool                 (* boolean kind *) 
  | Karrow of kind * kind (* functions from constructors to constructors *)
  | Kprod of kind list    (* tuples of constructors *)
  | Kname                 (* "names" used for alias information *)
  | Kcap                  (* capabilities for alias information *)
  | Kms                   (* machine states *)
(* ----- LX kinds ----- *)
  | Ksum of kind list
  | Kvar of identifier
  | Kmu of kmu_schema * identifier 
(* ----- end LX ----- *)

(* LX *)
and kmu_schema = (identifier * kind) list 
and kind =
   { 
   mutable rkind : rkind;                           (* the raw kind *)
   mutable freekindvars : identifier Set.set option; (* free kind vars *)
   mutable kabbrev : identifier option   (* is this con an abbreviation *)
 } 
;;

(* helper functions for creating kinds *)
let defkind k = { rkind = k; freekindvars = None; kabbrev = None }
let defprimkind k = { rkind = k; freekindvars = Some (Set.empty id_compare); kabbrev = None}

(* kind constructors *)
let kbyte s = defprimkind (Kbyte s)
let ktype = defprimkind Ktype
let kmemi i = defprimkind (Kmemi i)
let kmem = defprimkind(Kmem)
let kstack = defprimkind Kstack
let kint = defprimkind Kint
let kbool = defprimkind Kbool
let karrow k1 k2 = defkind (Karrow (k1,k2))
let kprod ks = defkind (Kprod ks)
let kunit = defprimkind(Kprod [])
let ksum ks = defkind (Ksum ks)
let kvar i = {rkind=Kvar i; freekindvars = Some (Set.singleton id_compare i); kabbrev=None}
let kmu i l = defkind (Kmu (i,l))
let kname = defprimkind Kname
let kcap = defprimkind Kcap
let kms = defprimkind Kms
(* end LX *)

let k4byte = kbyte Byte4;; (* 32 bit values *)

(**********************************************************************)
(* Type Constructors *)

(* primitive constructors *)
type primcon = 
    PCbytes of scale      (* : Kbyte s *)
  | PCfloat32             (* : Kbyte Byte4 *)
  | PCfloat64             (* : Kbyte Byte8 *)
  | PCjunk of int32       (* : Uninitialized junk on the stack or in memory *)
  | PCjunkbytes of scale  (* : junk of kind Kbyte s *)
  | PCint of int32
  | PCtrue
  | PCfalse
(* fields in tuples & arrays have variances:
     readonly, writeonly, readwrite *)
type variance = Read | Write | ReadWrite;;

(* arithmetic and logical operators *)
type log =
    Cadd  (* 32 bit add *)
  | Csub  (* 32 bit sub *)
  | Cmuls (* 32 bit signed mul *)
  | Cmulu (* 32 bit unsigned mul *)
  | Cand  (* logical and *)
  | Cor   (* logical or *)
  | Cimp  (* logical implication *)
  | Ciff  (* logical bi-implication *)
  | Cnot  (* logical not *)
  | Clts  (* signed less than *)
  | Cltu  (* unsigned less than *)
  | Cltes (* signed less than or equal to *)  
  | Clteu (* unsigned less than or equal to *)

(* alias information *)
type alias_info = Unique | MayAlias;;

(* floating point stack: 8 fp register tags (ST(0),ST(1),...,ST(7)) *)
type  fpreg   = FPempty | FPfull | FPany
type fpstack = fpreg * fpreg * fpreg * fpreg * fpreg * fpreg * fpreg * fpreg
;;

type con_state = NotNorm | Normalized | WeakHead;;
type rcon = 
(* the language portion of con's *)
    Cvar of identifier
  | Clam of identifier * kind * con
  | Capp of con * con
  | Ctuple of con list
  | Cproj of int * con
(* ---- LX ---- *)
  | Cinj of int * con * kind
  | Ccase of con * identifier * con list
  | Cfold of kind * con
  | Cpr of identifier * (identifier * identifier * kind * identifier * kind * con) list
  | Cvoid of kind
(*  | Cprnat of identifier * kind * con * identifier * con *)
(* -- end LX -- *)
(* the "type" portion of con's *)
  | Clab of identifier
  | Cprim of primcon  (* see above *)
  | Crec of (identifier * kind * con) list
  | Cforall of identifier * kind * con
  | Cexist of identifier * kind * con * con (* E[i:k such that c1].c2 *)
  | Ccode of con 
  | Cms of machine_state
  | Cmsjoin of con * con
  | Chptr of int32 list*con option*(con*variance) option
  | Cfield of con*variance
  | Cprod of con list
  | Csum of con list
  | Carray of con*con
  | Csing of con
(* the "stack" portion of con's *)
  | Csptr of con
  | Cempty
  | Ccons of con * con
  | Cappend of con * con
(* the arithmetic and logical portion of the con's *)
  | Clog of log * con list
  | Cif of con * con (* if c1:bool then c2 else void *)
(* the alias information of con's *)
  | Cname of con
  | Ccap of (identifier,(alias_info*con)) Dict.dict
  | Cjoin of con list
  | Ctagof of con (* :Kname -> K4byte the tag of a sum value *)
(* Cyclone *)
  | Ctmpl of con * con option
        * (identifier * con) list * (identifier * con) list
  | Ctptr of identifier
  | Ctrgn of con * con option
        * (identifier * (identifier * con) list * (identifier * con) list) list
(* End Cyclone *)
  (* an explicit substitution applied to a con *)
  | Csubst of con * esubst
(* Enil - empty substitution, Es replace var with con, Eo left-to-composition*)
  | Cr of rep_item (* type of a type representation *)
(* the type hack portion*)
  | Ctypeof of identifier

and esubst = Enil | Es of identifier * con | Eo of esubst * esubst
and con = 
  { mutable rcon     : rcon;   (* "raw" constructor *)
    mutable con_state : con_state;
    mutable freevars : (identifier Set.set * identifier Set.set) option;
    mutable hash : int; 
    mutable abbrev : identifier option   (* is this con an abbreviation *)
  } 
(* free kind and con vars of rcon *)

and machine_state = {
    ms_eax : con option;
    ms_ebx : con option;
    ms_ecx : con option;
    ms_edx : con option;
    ms_esi : con option;
    ms_edi : con option;
    ms_ebp : con option;
    ms_esp : con option;
(*    ms_regs : (reg,con) Dict.dict; *)
    ms_fpstack : fpstack;
    mutable ms_cc : ccinfo;
    ms_save_cc : ccinfo;
    ms_cap : con;
  }
and ccinfo =
    CCnoinfo
  | CCcmp of con*con
  | CCtest of con*con
and rep_item = RCon of con | RKind of kind | RLabel of identifier

(* floating point registers and stack *)
let get_fpreg fps i = 
  let (a,b,c,d,e,f,g,h) = fps in 
  match i with
    0 -> a
  | 1 -> b
  | 2 -> c
  | 3 -> d
  | 4 -> e
  | 5 -> f
  | 6 -> g
  | 7 -> h
  | _ -> invalid_arg "get_fpreg: bad reg"; a
;;
let set_fpreg fps i fpr = 
  let (a,b,c,d,e,f,g,h) = fps in 
  match i with
    0 -> (fpr,b,c,d,e,f,g,h)
  | 1 -> (a,fpr,c,d,e,f,g,h)
  | 2 -> (a,b,fpr,d,e,f,g,h)
  | 3 -> (a,b,c,fpr,e,f,g,h)
  | 4 -> (a,b,c,d,fpr,f,g,h)
  | 5 -> (a,b,c,d,e,fpr,g,h)
  | 6 -> (a,b,c,d,e,f,fpr,h)
  | 7 -> (a,b,c,d,e,f,g,fpr)
  | _ -> invalid_arg "set_fpreg: bad reg"; fps
;;

let fpstack_empty = 
  (FPempty,FPempty,FPempty,FPempty,FPempty,FPempty,FPempty,FPempty)
;;

let fpstack_isempty     fps   = fps = fpstack_empty;;
let fpstack_isempty_reg fps i = get_fpreg fps i = FPempty;;
let fpstack_isfull_reg  fps i = get_fpreg fps i = FPfull;;

let fpstack_equal fps1 fps2   = fps1 = fps2;;

let fpstack_leq fps1 fps2 = 
  let rec aux i =
    if i < 8 then 
      let st1 = get_fpreg fps1 i in
      let st2 = get_fpreg fps2 i in
      if st2 = FPany then aux (i+1)
      else if st1 = st2 then aux (i+1) else false
    else true in
aux 0
;;

let fpstack_get_fpreg fps i =
  if 0 <= i && i <=7 then get_fpreg fps i 
  else invalid_arg ("fpstack_get_fpreg: arg = " ^ string_of_int i)

let fpstack_set_fpreg fps i fpr =
  if 0 <= i && i <= 7 then set_fpreg fps i fpr
  else invalid_arg ("fpstack_set_fpreg: arg = " ^ string_of_int i)

(* create stack with height i, i must be in the range 0..8 *)
let fpstack_create i =
  if 0 < i && i <= 8 then
    let rec aux fps j =
      if j < 0 then fps
      else aux (set_fpreg fps j FPfull) (j-1) 
    in
    aux fpstack_empty (i-1)
  else
    invalid_arg 
      ("fpstack_create: cannot create stack with height "^string_of_int i)
;;

(* initialize fp register, i must be in the range 0..7 *)
let fpstack_init_reg fps i = 
  if 0 <= i && i <= 7 then set_fpreg fps i FPfull
  else invalid_arg ("fpstack_init_reg: cannot initialize reg"^string_of_int i)
;;

let fpstack_hide_reg fps i =
  if 0 <= i && i <= 7 then set_fpreg fps i FPany
  else invalid_arg ("fpstack_hide_reg: cannot hide reg"^string_of_int i)

(* initialize fp register, i must be in the range 0..7 *)
let fpstack_free_reg fps i = 
  if 0 <= i && i <= 7 then set_fpreg fps i FPempty
  else invalid_arg ("fpstack_free_reg: cannot free reg"^string_of_int i)
;;

(* true if ST(i) a valid reference in the fp stack *) 
let fpstack_inrange fps i = get_fpreg fps i = FPfull;;

(* The following check overflow/underflow and raise FPstack_error "overflow"
 * or FPstack_error "underflow".  Overflow occurs when attempting to
 * push onto a slot that is already full. Underflow occurs when attempting
 * a pop of an empty register. (NOTE: INTEL manual appears to have an error
 * in its description of stack pop, which doesn't allow you to pop the last
 * element off the stack.  See Intel Arch. Software Developer's Manual,
 * volume 1,  page 7-20.  The manual is unclear, but it seems to state that
 * underflow is detected by checking to see if the result position of the top
 * pointer is empty instead of checking to see if the start position of the
 * top pointer is empty.  Here we check to see if the start position of the
 * top pointer is empty.  This way, we can pop the last element and move
 * the pointer to a position where it points to an empty slot.)
 * 
 * Negative numbers push; Positive numbers pop
 * Argument index must be in the range -2..2
 *)
let fpstack_adjust error fps i = 
  let overflow  () = error "overflow"; fps  in
  let underflow () = error "underflow"; fps in
  let (a,b,c,d,e,f,g,h) = fps in
  match i with
    0  -> fps
  | -1 -> 
      if h = FPfull then overflow()
      else (FPfull,a,b,c,d,e,f,g)
  | -2 ->
      if h = FPfull or g = FPfull then overflow()
      else (FPfull,FPfull,a,b,c,d,e,f)
  | 1  ->
      if a = FPempty then underflow()
      else (b,c,d,e,f,g,h,FPempty)
  | 2  -> 
      if a = FPempty or b = FPempty then underflow()
      else (c,d,e,f,g,h,FPempty,FPempty)
  | _ -> invalid_arg ("fpstack_adjust: cannot adjust by"^string_of_int i)
;;

(* Bump the top-of-stack pointer by a max of 2 in either direction.  
 * Has the effect of rotating empty or filled stack slots with respect to
 * the top of the stack.
 * Positive int arguments rotate in the direction of popping, negative in the
 * direction of pushing. 
 * Do *not* fill or empty stack slots while rotating.  
 * Do *not* check for overflow or underflow.
 *)
let fpstack_rotate fps i =
  let (a,b,c,d,e,f,g,h) = fps in
  match i with
    0  -> fps
  | -1 -> (h,a,b,c,d,e,f,g)
  | -2 -> (g,h,a,b,c,d,e,f)
  | 1  -> (b,c,d,e,f,g,h,a)
  | 2  -> (c,d,e,f,g,h,a,b)
  | _ ->  invalid_arg ("fpstack_rotate: cannot rotate by"^string_of_int i)
;;

(* Hash Consing *)

(* just pray that these are inlined...*)
let comb i1 i2 = (i1 lsl 5) lxor i1 lxor i2;;
let combine i1 i2 = (comb i1 i2) land 0x1fffffff;;
let combine1 = combine;;
let comb2 i i1 i2 = (comb i (comb i1 i2));;
let combine2 i i1 i2 = (comb2 i i1 i2) land 0x1fffffff;;
let comb3 i i1 i2 i3 = (comb i (comb2 i i1 i2));;
let combine3 i i1 i2 i3 = (comb i (comb i1 (comb i2 i3))) land 0x1fffffff;;
let combine4 i i1 i2 i3 i4 = 
  (comb i (comb i1 (comb i2 (comb i3 i4)))) land 0x1fffffff;;
let combine6 i i1 i2 i3 i4 i5 i6 = 
  (comb i (comb3 (comb2 i1 i2 i3)  i4 i5 i6)) land 0x1fffffff;;

let hash_id = Hashtbl.hash;;
let hash_log = Hashtbl.hash;;

let hash_scale sc = 
  match sc with
  | Byte1 -> 1
  | Byte2 -> 2
  | Byte4 -> 3
  | Byte8 -> 4

let hash_primcon pc = 
  match pc with
  | PCbytes sc -> hash_scale sc
  | PCjunk i -> combine 9 (int32_to_int i)
  | PCjunkbytes sc -> 10 + (hash_scale sc)
  | PCint i -> combine 15 (int32_to_int i)
  | PCtrue -> 16
  | PCfalse -> 17
  | PCfloat32 -> 18
  | PCfloat64 -> 19

let rec rcon_hash r = 
  match r with
    Cvar x -> combine1 1 (hash_id x)
  | Clam(x,k,c) -> combine2 2 (hash_id x) c.hash
  | Capp(c1,c2) -> combine2 3 c1.hash c2.hash
  | Ctuple(cs) -> hash_con_list 4 cs
  | Cproj(i,c) -> combine2 5 i c.hash
  | Cinj(i,c,k) -> combine2 6 i c.hash 
  | Ccase(c,x,cs) -> combine2 (hash_con_list 7 cs) c.hash (hash_id x) 
  | Cfold(k,c) -> combine1 8 c.hash
  | Cpr(x,ys) -> 
      List.fold_left (fun i (x,y,k1,z,k2,c) -> 
	combine4 i (hash_id x) (hash_id y) (hash_id z)
	   c.hash) (combine1 9 (hash_id x)) ys
  | Cvoid k -> 10 
  | Clab x -> combine1 11 (hash_id x)
  | Cprim pc -> combine1 12 (hash_primcon pc)
  | Crec xkcs -> 
      List.fold_left 
	(fun i (x,k,c) -> combine2 i (hash_id x) c.hash) 13 xkcs
  | Cforall(x,k,c) -> combine2 14 (hash_id x) c.hash
  | Cexist(x,k,c1,c2) -> combine3 15 (hash_id x) c1.hash c2.hash
  | Ccode c -> combine1 16 c.hash
  | Cms ms -> combine1 17 (hash_ms ms)
  | Cmsjoin(c1,c2) -> combine2 18 c1.hash c2.hash
  | Chptr(is,copt,cvopt) -> 
      combine2 (List.fold_left (fun i j -> combine i (int32_to_int j)) 19 is)
	(match copt with None -> 0 | Some c -> c.hash)
	(match cvopt with None -> 0 | 
	Some (c,v) -> c.hash)
  | Cfield(c,v) -> combine1 20 c.hash 
  | Cprod cs -> hash_con_list 21 cs
  | Csum cs -> hash_con_list 22 cs
  | Carray(c1,c2) -> combine2 23 c1.hash c2.hash
  | Csing c -> combine 24 c.hash
  | Csptr c -> combine 25 c.hash
  | Cempty -> 26
  | Ccons(c1,c2) -> combine2 27 c1.hash c2.hash
  | Cappend(c1,c2) -> combine2 28 c1.hash c2.hash
  | Clog(log,cs) -> hash_con_list (combine 29 (hash_log log)) cs
  | Cif(c1,c2) -> combine2 30 c1.hash c2.hash
  | Cname c -> combine1 31 c.hash
  | Ccap d -> 
      Dict.fold_dict (fun x (ai,c) i -> combine2 i (hash_id x) c.hash) d 32
  | Cjoin cs -> hash_con_list 33 cs
  | Ctagof c -> combine1 34 c.hash
  | Ctmpl(c,copt,xs,ys) -> 
      let f = List.fold_left (fun i (x,c) -> combine2 i (hash_id x) c.hash) in
      f (f (combine2 35 c.hash (match copt with None -> 0 | Some c -> c.hash))
	   xs) ys
  | Ctptr x -> combine1 36 (hash_id x)
  | Ctrgn(c,copt,xs) -> 
      combine2 37 c.hash (match copt with None -> 0 | Some c -> c.hash)
  | Csubst(c,e) -> combine1 38 c.hash
  (* SCW -- I sure hope I did Cr right! *)
  | Cr ci ->
       (match ci with 
	  RCon c -> combine1 39 c.hash
	| RKind k -> 40
	| RLabel l -> combine1 41 (hash_id l))
  | Ctypeof l -> combine 42 (hash_id l)
and hash_con_list i cs = 
  match cs with 
    [] -> i
  | c1::rest -> hash_con_list (combine i c1.hash) rest
and hash_copt c = 
  match c with
    None -> 0
  | Some c -> c.hash
and hash_ms ms = 
  combine (hash_copt ms.ms_eax) (hash_copt ms.ms_esp) 
(*
  combine3
   (combine2 (hash_copt ms.ms_eax) (hash_copt ms.ms_ebx) (hash_copt ms.ms_ecx))
   (combine2 (hash_copt ms.ms_edx) (hash_copt ms.ms_esi) (hash_copt ms.ms_edi))
   (combine2 (hash_copt ms.ms_ebp) (hash_copt ms.ms_esp) ms.ms_cap.hash)
   (match ms.ms_cc with
     CCnoinfo -> 0
   | CCcmp(c1,c2) -> combine2 1 c1.hash c2.hash
   | CCtest(c1,c2) -> combine2 2 c1.hash c2.hash)
*)
;;
(*
let rcon_hash r = 0
*)

let prim_string pc = 
  match pc with
    PCbytes Byte1 -> "b1"
  | PCbytes Byte2 -> "b2"
  | PCbytes Byte4 -> "b4"
  | PCbytes Byte8 -> "b8"
  | PCfloat32 -> "f4"
  | PCfloat64 -> "f8"
  | PCjunk i -> "junk"^(string_of_int(int32_to_int i))
  | PCjunkbytes Byte1 -> "1junk"
  | PCjunkbytes Byte2 -> "2junk"
  | PCjunkbytes Byte4 -> "4junk"
  | PCjunkbytes Byte8 -> "8junk"
  | PCint i -> "int"^(string_of_int(int32_to_int i))
  | PCtrue -> "true"
  | PCfalse -> "false"

let rcon_string r = 
  match r with
    Cvar x -> "var("^(id_to_string x)^")"
  | Clam(x,k,c) -> "lam"
  | Capp(c1,c2) -> "app"
  | Ctuple(cs) -> "tuple"
  | Cproj(i,c) -> "proj"
  | Cinj(i,c,k) -> "inj"
  | Ccase(c,x,cs) -> "case"
  | Cfold(k,c) -> "fold"
  | Cpr(x,ys) -> "pr"
  | Cvoid k -> "void"
  | Clab x -> "lab("^(id_to_string x)^")"
  | Cprim pc -> "prim("^(prim_string pc)^")"
  | Crec xkcs -> "rec"
  | Cforall(x,k,c) -> "forall"
  | Cexist(x,k,c1,c2) -> "exist"
  | Ccode c -> "code"
  | Cms ms -> "ms"
  | Cmsjoin(c1,c2) -> "msjoin"
  | Chptr(is,copt,cvopt) -> "hptr"
  | Cfield(c,v) -> "field"
  | Cprod cs -> "prod"
  | Csum cs -> "sum"
  | Carray(c1,c2) -> "array"
  | Csing c -> "sing"
  | Csptr c -> "sptr"
  | Cempty -> "empty"
  | Ccons(c1,c2) -> "cons"
  | Cappend(c1,c2) -> "append"
  | Clog(log,cs) -> "log"
  | Cif(c1,c2) -> "if"
  | Cname c -> "name"
  | Ccap d -> "cap"
  | Cjoin cs -> "join"
  | Ctagof c -> "tagof"
  | Ctmpl(c,copt,xs,ys) -> "tmpl"
  | Ctptr x -> "tptr"
  | Ctrgn(c,copt,xs) -> "trgn"
  | Csubst(c,e) -> "subst"
  | Cr(c1) -> "r"
  | Ctypeof _ -> "typeof"
;;

let rec rcon_equal r1 r2 = 
  match r1,r2 with
    Cvar x,Cvar y -> id_eq x y
  | Clam(x1,k1,c1),Clam(x2,k2,c2) -> 
      c1 == c2 && (id_eq x1 x2) && (kind_eq k1 k2)
  | Capp(c1a,c1b),Capp(c2a,c2b) ->
      c1a == c2a && c1b == c2b
  | Ctuple cs1,Ctuple cs2 -> equal_cons cs1 cs2
  | Cproj(i1,c1),Cproj(i2,c2) -> i1 = i2 && c1 == c2
  | Cinj(i1,c1,k1),Cinj(i2,c2,k2) -> i1 = i2 && c1 == c2 && (kind_eq k1 k2)
  | Ccase(c1,x1,cs1),Ccase(c2,x2,cs2) -> 
      c1 == c2 && (id_eq x1 x2) && (equal_cons cs1 cs2)
  | Cfold(k1,c1),Cfold(k2,c2) -> c1 == c2 && (kind_eq k1 k2)
  | Cpr _,Cpr _ -> false
  | Cvoid k1,Cvoid k2 -> kind_eq k1 k2
  | Clab x,Clab y -> id_eq x y
  | Cprim pc1,Cprim pc2 -> pc1 = pc2
  | Crec vkcs1,Crec vkcs2 ->
      let rec loop vkcs1 vkcs2 = 
	match vkcs1,vkcs2 with
	  [],[] -> true
	| (x1,k1,c1)::r1,(x2,k2,c2)::r2 ->
	    c1 == c2 && (kind_eq k1 k2) && (id_eq x1 x2) && loop r1 r2
	| _, _ -> false
      in loop vkcs1 vkcs2
  | Cforall(x1,k1,c1),Cforall(x2,k2,c2) -> 
      c1 == c2 && (id_eq x1 x2) && (kind_eq k1 k2)
  | Cexist(x1,k1,c11,c12),Cexist(x2,k2,c21,c22) ->
      c11 = c21 && c12 == c22 && (id_eq x1 x2) && (kind_eq k1 k2)
  | Ccode c1,Ccode c2 -> c1 == c2
  | Cms ms1,Cms ms2 -> 
      let cmpopt copt1 copt2 = 
	match copt1,copt2 with
	  None,None -> true
	| Some c1,Some c2 -> c1 == c2
	| _,_ -> false in
      cmpopt ms1.ms_eax ms2.ms_eax &&
      cmpopt ms1.ms_ebx ms2.ms_ebx &&
      cmpopt ms1.ms_ecx ms2.ms_ecx &&
      cmpopt ms1.ms_edx ms2.ms_edx &&
      cmpopt ms1.ms_esi ms2.ms_esi &&
      cmpopt ms1.ms_edi ms2.ms_edi &&
      cmpopt ms1.ms_ebp ms2.ms_ebp &&
      cmpopt ms1.ms_esp ms2.ms_esp &&
      fpstack_equal ms1.ms_fpstack ms2.ms_fpstack &&
      ms1.ms_cap == ms2.ms_cap &&
      (match ms1.ms_cc, ms2.ms_cc with
	CCnoinfo,CCnoinfo -> true
      |	CCcmp(c11,c12),CCcmp(c21,c22) -> c11 == c21 && c12 == c22
      |	CCtest(c11,c12),CCtest(c21,c22) -> c11 == c21 && c12 == c22
      |	_,_ -> false)
  | Cmsjoin(c11,c12),Cmsjoin(c21,c22) -> c11 == c21 && c12 == c22
  | Chptr(is1,copt1,cvopt1),Chptr(is2,copt2,cvopt2) ->
      (match copt1,copt2 with
	None,None -> true
      |	Some c1,Some c2 -> c1 == c2
      |	_,_ -> false) &&
      (match cvopt1,cvopt2 with
	None,None -> true
      |	Some(c1,v1),Some(c2,v2) -> c1 == c2 && v1 == v2
      |	_,_ -> false) &&
      let rec loop is1 is2 = 
	match is1, is2 with
	  [],[] -> true
	| i1::is1,i2::is2 -> i1 = i2 && (loop is1 is2)
	| _,_ -> false 
      in loop is1 is2
  | Cfield(c1,v1),Cfield(c2,v2) -> c1 == c2 && v1 == v2
  | Cprod cs1,Cprod cs2 -> equal_cons cs1 cs2
  | Csum cs1,Csum cs2 -> equal_cons cs1 cs2
  | Carray(c11,c12), Carray(c21,c22) -> c11 == c21 && c12 == c22
  | Csing c1, Csing c2 -> c1 == c2
  | Csptr c1, Csptr c2 -> c1 == c2
  | Cempty,Cempty -> true
  | Ccons(c11,c12), Ccons(c21,c22) -> c11 == c21 && c12 == c22
  | Cappend(c11,c12), Cappend(c21,c22) -> c11 == c21 && c12 == c22
  | Clog(l1,cs1), Clog(l2,cs2) -> (l1 = l2) && (equal_cons cs1 cs2)
  | Cif(c11,c12),Cif(c21,c22) -> c11 == c21 && c12 == c22
  | Cname c1,Cname c2 -> c1 == c2
  | Ccap d1,Ccap d2 -> 
      Dict.fold_dict 
	(fun x c b -> b && (Dict.member d2 x) && ((Dict.lookup d2 x) == c)) 
	d1 (Dict.fold_dict (fun x _ b -> b && Dict.member d1 x) d2 true)
  | Cjoin cs1,Cjoin cs2 -> equal_cons cs1 cs2
  | Ctagof c1,Ctagof c2 -> c1 == c2
  | Ctmpl _,Ctmpl _ -> false
  | Ctptr x,Ctptr y -> id_eq x y
  | Ctrgn _,Ctrgn _ -> false
  | Csubst _,Csubst _ -> false
  | Cr(c1),Cr(c2) -> c1 == c2
  | Ctypeof l1,Ctypeof l2 -> id_eq l1 l2
  | _,_ -> false
and id_eq x y = (id_compare x y) = 0
and equal_cons cs1 cs2 = 
  match cs1,cs2 with
    [],[] -> true
  | c1::cs1,c2::cs2 -> c1 == c2 && (equal_cons cs1 cs2)
  | _,_ -> false
and kind_eq k1 k2 = 
  match k1.rkind,k2.rkind with
    Kbyte s1,Kbyte s2 -> s1 = s2
  | Ktype,Ktype -> true
  | Kmem,Kmem -> true
  | Kmemi i1,Kmemi i2 -> i1 =$ i2
  | Kstack,Kstack -> true
  | Kint,Kint -> true
  | Kbool,Kbool -> true
  | Karrow(k11,k12),Karrow(k21,k22) -> (kind_eq k11 k21) && (kind_eq k12 k22)
  | Kprod ks1,Kprod ks2 -> kinds_eq ks1 ks2
  | Kname,Kname -> true
  | Kcap,Kcap -> true
  | Kms,Kms -> true
  | Ksum ks1,Ksum ks2 -> kinds_eq ks1 ks2
  | Kvar x1,Kvar x2 -> id_eq x1 x2
  | Kmu (sc1,x1), Kmu (sc2,x2) -> 
      (id_eq x1 x2) && 
      let rec sc_eq sc1 sc2 = 
	match sc1, sc2 with
	  [],[] -> true
	| (x1,k1)::t1,(x2,k2)::t2 -> 
	    (id_eq x1 x2) && (kind_eq k1 k2) && (sc_eq t1 t2)
	| _,_ -> false
      in sc_eq sc1 sc2
  | _,_ -> false
and kinds_eq k1s k2s =
  match k1s,k2s with
    [],[] -> true
  | k1::k1s,k2::k2s -> (kind_eq k1 k2) && (kinds_eq k1s k2s)
  | _,_ -> false
;;

module ConHash = 
  Hashtbl.Make(
  struct 
    type t = rcon 
    let equal = rcon_equal
    let hash = rcon_hash
  end)
;;

(* this should be a prime *)
let initial_con_hash_table_size = 101;;
let con_hash_table : con ConHash.t =
  ConHash.create initial_con_hash_table_size;;
let hash_clear() = 
  begin
    ConHash.clear con_hash_table
  end;;
let hash_find = ConHash.find con_hash_table;;
let hash_mem = ConHash.mem con_hash_table;;
let hash_add = ConHash.add con_hash_table;;
let defcon rc =
  try 
    hash_find rc 
  with Not_found -> 
    let con = { rcon = rc; con_state = NotNorm; freevars = None; 
		abbrev = None; hash = rcon_hash rc
	      }
    in hash_add rc con; con
;;
let empty_id_set = Set.empty id_compare;;
let empty_freevars = Some(empty_id_set,empty_id_set);;
let prcon rc = 
  let con = defcon rc in 
  con.con_state <- Normalized;
  con.freevars <- empty_freevars;
  con
;;
let wcon rc = 
  let con = defcon rc in
  con.con_state <- WeakHead;
  con
;;

let cempty_cap = wcon (Ccap (Dict.empty id_compare));;

let ccinfo_map f cc =
  match cc with
    CCnoinfo -> cc
  | CCcmp (c1,c2) -> CCcmp (f c1,f c2)
  | CCtest (c1,c2) -> CCtest (f c1,f c2)
;;
let ccinfo_app f cc =
  match cc with
    CCnoinfo -> ()
  | CCcmp (c1,c2) -> f c1; f c2; ()
  | CCtest (c1,c2) -> f c1; f c2; ()
;;

(* Machine states *)

let ms_empty =
  { ms_eax = None; ms_ebx = None; ms_ecx = None; ms_edx = None;
    ms_esi = None; ms_edi = None; ms_ebp = None; ms_esp = None;
    ms_fpstack=fpstack_empty;
    ms_cc=CCnoinfo; ms_save_cc=CCnoinfo; ms_cap=cempty_cap }
;;
let ms_map f ms =
  let mapopt copt = 
    match copt with
      None -> None
    | Some c -> Some(f c)
  in 
  { ms_eax = mapopt ms.ms_eax; ms_ebx = mapopt ms.ms_ebx;
    ms_ecx = mapopt ms.ms_ecx; ms_edx = mapopt ms.ms_edx;
    ms_esi = mapopt ms.ms_esi; ms_edi = mapopt ms.ms_edi;
    ms_ebp = mapopt ms.ms_ebp; ms_esp = mapopt ms.ms_esp;
    ms_fpstack = ms.ms_fpstack;
    ms_cc = ccinfo_map f ms.ms_cc;
    ms_save_cc = ccinfo_map f ms.ms_save_cc;
    ms_cap = f ms.ms_cap
  } 
;;
let ms_get_cap ms = ms.ms_cap
;;
let ms_app f ms =
  let app copt = 
    match copt with
      None -> ()
    | Some c -> f c in
  app ms.ms_eax; app ms.ms_ebx; app ms.ms_ecx; app ms.ms_edx;
  app ms.ms_esi; app ms.ms_edi; app ms.ms_ebp; app ms.ms_esp;
  f (ms_get_cap ms);
  ccinfo_app f ms.ms_cc
;;
let ms_get_reg ms r = 
  let copt = 
    match r with
      Eax -> ms.ms_eax
    | Ebx -> ms.ms_ebx
    | Ecx -> ms.ms_ecx
    | Edx -> ms.ms_edx
    | Esi -> ms.ms_esi
    | Edi -> ms.ms_edi
    | Ebp -> ms.ms_ebp
    | Esp -> ms.ms_esp
    | _ -> failwith "virtual registers unimplemented"
  in match copt with
    None -> raise Dict.Absent
  | Some c -> c
;;
let ms_set_reg ms r c = 
  match r with
    Eax -> {ms with ms_eax = (Some c)}
  | Ebx -> {ms with ms_ebx = (Some c)}
  | Ecx -> {ms with ms_ecx = (Some c)}
  | Edx -> {ms with ms_edx = (Some c)}
  | Esi -> {ms with ms_esi = (Some c)}
  | Edi -> {ms with ms_edi = (Some c)}
  | Ebp -> {ms with ms_ebp = (Some c)}
  | Esp -> {ms with ms_esp = (Some c)}
  | _ -> failwith "virtual registers unimplemented"
;;
let ms_set_regs ms rcs = 
  List.fold_left (fun ms (r,c) -> ms_set_reg ms r c) ms rcs
;;
let ms_del_reg ms r = 
  match r with
    Eax -> {ms with ms_eax = None}
  | Ebx -> {ms with ms_ebx = None}
  | Ecx -> {ms with ms_ecx = None}
  | Edx -> {ms with ms_edx = None}
  | Esi -> {ms with ms_esi = None}
  | Edi -> {ms with ms_edi = None}
  | Ebp -> {ms with ms_ebp = None}
  | Esp -> {ms with ms_esp = None}
  | _ -> failwith "virtual registers unimplemented"
;;
let ms_del_regs ms rl = List.fold_left ms_del_reg ms rl
;;
let ms_map_reg f ms = 
  let mapopt copt = 
    match copt with
      None -> None
    | Some c -> Some(f c)
  in 
  { ms with 
    ms_eax = mapopt ms.ms_eax; ms_ebx = mapopt ms.ms_ebx;
    ms_ecx = mapopt ms.ms_ecx; ms_edx = mapopt ms.ms_edx;
    ms_esi = mapopt ms.ms_esi; ms_edi = mapopt ms.ms_edi;
    ms_ebp = mapopt ms.ms_ebp; ms_esp = mapopt ms.ms_esp;
  } 
;;
let ms_app_reg f ms = 
  let app r copt = 
    match copt with
      None -> ()
    | Some c -> f r c;() in
  app Eax ms.ms_eax; app Ebx ms.ms_ebx; app Ecx ms.ms_ecx; app Edx ms.ms_edx;
  app Esi ms.ms_esi; app Edi ms.ms_edi; app Ebp ms.ms_ebp; app Esp ms.ms_esp
;;
let ms_fold_reg f ms a = 
  let fopt r copt a = 
    match copt with
      None -> a
    | Some c -> f r c a in
  let a = fopt Esp ms.ms_esp a in
  let a = fopt Ebp ms.ms_ebp a in
  let a = fopt Edi ms.ms_edi a in
  let a = fopt Esi ms.ms_esi a in
  let a = fopt Edx ms.ms_edx a in
  let a = fopt Ecx ms.ms_ecx a in
  let a = fopt Ebx ms.ms_ebx a in
  let a = fopt Eax ms.ms_eax a in
  a
;;
let ms_get_fpstack ms = ms.ms_fpstack;;
let ms_set_fpstack ms fps = { ms with ms_fpstack = fps };;
let ms_get_cc ms = ms.ms_cc;;
let ms_set_cc ms cc = { ms with ms_cc=cc; ms_save_cc=ms.ms_cc };;
let ms_restore_cc ms = ms.ms_cc <- ms.ms_save_cc;;

let ms_set_cap ms c = { ms with ms_cap=c} ;;

(* "merges" to machine states -- this is only one possible definition but
 * it seems reasonable to me (JGM):  the registers from ms2 overwrite any
 * bindings of registers in ms1.  The cc, and save_cc fields are
 * overwritten only if they contain information.  The capability fields
 * are joined.  The latter is the only one that concerns me as it means
 * that for sure ms_join is not idempotent...
 *)
let ms_join ms1 ms2 = 
  let jopt copt1 copt2 = 
    match copt2 with
      None -> copt1
    | Some _ -> copt2 in
  let eax = jopt ms1.ms_eax ms2.ms_eax in
  let ebx = jopt ms1.ms_ebx ms2.ms_ebx in
  let ecx = jopt ms1.ms_ecx ms2.ms_ecx in
  let edx = jopt ms1.ms_edx ms2.ms_edx in
  let esi = jopt ms1.ms_esi ms2.ms_esi in
  let edi = jopt ms1.ms_edi ms2.ms_edi in
  let ebp = jopt ms1.ms_ebp ms2.ms_ebp in
  let esp = jopt ms1.ms_esp ms2.ms_esp in
  let fps = 
    if fpstack_isempty ms2.ms_fpstack then ms1.ms_fpstack 
    else ms2.ms_fpstack in
  let cc = match ms2.ms_cc with CCnoinfo -> ms1.ms_cc | _ -> ms2.ms_cc in
  let save_cc = 
    match ms2.ms_save_cc with CCnoinfo -> ms1.ms_save_cc | _ -> ms2.ms_save_cc
  in let cap = defcon(Cjoin [ms1.ms_cap;ms2.ms_cap]) in
  { ms_eax = eax; ms_ebx = ebx; ms_ecx = ecx; ms_edx = edx;
    ms_esi = esi; ms_edi = edi; ms_ebp = ebp; ms_esp = esp;
    ms_fpstack = fps;
    ms_cc      = cc;
    ms_save_cc = save_cc;
    ms_cap     = cap
  } 

let pcbytes s = prcon (Cprim (PCbytes s));;
let cbyte8 = pcbytes Byte8;;
let cbyte4 = pcbytes Byte4;;
let cbyte2 = pcbytes Byte2;;
let cbyte1 = pcbytes Byte1;;

let pcfloat32 = prcon (Cprim PCfloat32);;
let pcfloat64 = prcon (Cprim PCfloat64);;

let pcjunk i = prcon (Cprim (PCjunk i));;
let pcjunk4  = prcon (Cprim (PCjunkbytes Byte4));;
let pcjunkbytes sc  = prcon (Cprim (PCjunkbytes sc));;
let pcint  i = prcon (Cprim (PCint i));;
let pctrue   = prcon (Cprim PCtrue);;
let pcfalse  = prcon (Cprim PCfalse);; 

let cvar v =
  let con = defcon (Cvar v) in
  (match con.freevars with
    None ->  (con.con_state <- Normalized;
	      con.freevars <- Some(empty_id_set, Set.singleton id_compare v))
  | _ -> ());
  con
;;
let clam v k c = wcon (Clam (v,k,c));;
let capp c1 c2 = defcon (Capp (c1,c2));;
let ctuple cs = wcon (Ctuple cs);;
let cproj c i = defcon (Cproj (i,c));;
(* ---- LX ---- *)
let cinj i c k = wcon (Cinj(i,c,k));;
let ccase c i cs = defcon (Ccase (c,i,cs));;
let cfold k c = wcon (Cfold (k,c));;
let cpr j l = wcon (Cpr (j,l));;
let cvoid k = wcon (Cvoid k);;
(* let cprnat f k z n s = wcon (Cprnat (f,k,z,n,s));; *)
(* -- end LX -- *)
let clab l = prcon (Clab l);;
let crec vkcs = wcon (Crec vkcs);;
let cforall i k c = wcon (Cforall (i,k,c));;
let cexist i k c = wcon (Cexist (i,k,pctrue,c));;
let cexistp i k c1 c2 = wcon (Cexist (i,k,c1,c2));;
let cms ms = wcon (Cms ms);;
let cmsjoin c1 c2 = defcon (Cmsjoin(c1,c2));;
let ccode c = wcon (Ccode c);;
let ccode_ms ms = ccode (cms ms);;
let ccode_l rcs = ccode_ms (ms_set_regs ms_empty rcs);;
let ccode_l_fps rcs fps = 
  ccode_ms (ms_set_fpstack (ms_set_regs ms_empty rcs) fps);;
let ccode_l_cap rcs cap = 
  ccode_ms (ms_set_cap (ms_set_regs ms_empty rcs) cap);;
let ccode_l_cap_fps rcs cap fps =
  ccode_ms (ms_set_fpstack (ms_set_cap (ms_set_regs ms_empty rcs) cap) fps);;
let chptr is co tco = wcon (Chptr (is,co,tco));;
let cptr c = wcon (Chptr ([],Some c,None));;
let cfield c v = wcon (Cfield (c,v));;
let cprod cs = defcon (Cprod cs);;
let cprod_b cs = cptr (cprod cs);;
let csum cs = wcon (Csum cs);;
let carray cl ce = wcon (Carray (cl,ce));;
let csing c = wcon (Csing c);;
let carray_s v ce =
  let cv = cvar v in
  cexist v kint (cprod_b [cfield (csing cv) Read;carray cv ce])
;;
let csptr c = wcon (Csptr c);;
let cempty = prcon Cempty;;
let ccons c1 c2 = wcon (Ccons (c1,c2));;
let cappend c1 c2 = defcon (Cappend (c1,c2));;
(* Cyclone *)
let ctmpl (c,copt,xc1,xc2) = wcon (Ctmpl(c,copt,xc1,xc2));;
let ctptr x = prcon (Ctptr x);;
let ctrgn (c,copt,l) = wcon (Ctrgn(c,copt,l));;
(* Cyclone end *)
let csubst c s = defcon (Csubst(c,s));;
let cr c = wcon (Cr c);;
let ccap d = wcon (Ccap d);;
let cjoin cs = defcon(Cjoin cs);;
let cname x = wcon(Cname x);;
let ctagof c = wcon (Ctagof c);;
let ctypeof l = wcon (Ctypeof l);;

let cif c1 c2 = defcon (Cif (c1,c2))
let clog log cs = defcon (Clog (log,cs))

(* integer kind *)
let cadd cs = clog Cadd cs;;
let csub c1 c2 = clog Csub [c1;c2];;
let cmuls i c2 = clog Cmuls [pcint i;c2];;
let cmulu i c2 = clog Cmulu [pcint i;c2];;

(* boolean kind *)
let cand cs = clog Cand cs;;
let cor cs = clog Cor cs;;
let cnot c1 = clog Cnot [c1];;
let cimplies c1 c2 = clog Cimp [c1;c2] 
let ciff c1 c2 = clog Ciff [c1;c2]
let clts c1 c2 = clog Clts [c1;c2]
let cltu c1 c2 = clog Cltu [c1;c2]
let cltes c1 c2 = clog Cltes [c1;c2]
let clteu c1 c2 = clog Clteu [c1;c2]
let ceq c1 c2 = cand [clteu c1 c2;clteu c2 c1]
let cne c1 c2 = cor [cltu c1 c2;cltu c2 c1]
let cgts c1 c2 = clts c2 c1
let cgtu c1 c2 = cltu c2 c1
let cgtes c1 c2 = cltes c2 c1
let cgteu c1 c2 = clteu c2 c1

let min_pointer_integer = int_to_int32 4096;;
let is_non_pointer_integer i =
  (land32 i i32_3 <>$ i32_0) or i<$min_pointer_integer
;;

(* Cyclone : FMS *)
(* alias capability utility functions for cons. *)

let rec cap_get_name con id =
  match con.rcon with
    Ccap d -> (try Some (Dict.lookup d id) with Dict.Absent -> None)
  | Cjoin cs -> 
      begin
	let rec loop cs = 
	  match cs with
	    [] -> None
	  | hd::tl -> (match cap_get_name hd id with None -> loop tl | x -> x)
	in
	loop cs
      end
  | _ -> None
;;

let rec cap_factor con : 
    ((identifier,alias_info*con) Dict.dict option) * (con list)=
  match con.rcon with
    Ccap d -> (Some d,[])
  | Cjoin cs ->
      begin
	let fcs = List.map cap_factor cs in
	let gen (d1,cs1) (d2,cs2) = 
	  let d' = 
	    match (d1,d2) with
	      (Some d1,Some d2) -> Some (Dict.update d1 d2)
	    | (Some d1,_) -> Some d1
	    | (_,Some d2) -> Some d2
	    | _ -> None
	  in
	  let cs' = cs1@cs2 in
	  (d',cs')
	in
	List.fold_left gen (None,[]) fcs
      end
  | _ -> (None,[con])

let cap_set_name con id info =
  match con.rcon with
    Ccap d -> Some (ccap (Dict.insert d id info))
  | Cjoin _ ->
      begin
	let (d_opt,cs) = cap_factor con in
	match d_opt with
	  None -> Some (cjoin ((ccap (Dict.singleton id_compare id info))::cs))
	| Some d -> Some (cjoin ((ccap (Dict.insert d id info)) :: cs))
      end
  | Cvar _ -> Some (cjoin [ccap (Dict.singleton id_compare id info);con])
  |  _ -> None
;;
    
(**********************************************************************)
(* Instructions *)
type annotate = (* Added by Dan *)
    Con        of con
  | AReg       of reg
  | StackTail  of reg * int
  | StackSlice of reg * int * int * con


(* various coercions that only affect the type of a value/reg/path/etc *)
type coercion =
    Pack of con * con  	 (* abstract a type: first con is hidden,
	      		    second con is existential *)
  | Tapp of annotate     (* instantiate type var *)
  | Roll of con        	 (* introduce recursive type *)
  | Unroll             	 (* eliminate recursive type *)
  | Tosum of con       	 (* coerce record/tag to a sum *)
  | Fromsum            	 (* coerce a unary-sum to a record *)
  | RollTosum of con   	 (* combined Tosum followed by Roll *)
  | Toarray of int32*int*con
                         (* coerce record to an array/vector
                            (offset,depth,element type) *)
  | Slot of int32*int32  (* coerce stack slot to junk *)
  | Subsume of con       (* subsumption *)
  | Forgetname           (* used to forget aliasing information for MayAlias
			  * names. *)
  | Prove                (* used to prove precondition on code. ie: eliminate
			  * Cif form *)
  | VirtLabel of identifier
	                 (* used only internally by the assembler and dis-
			  * assembler.  VirtLabel(x) allows the disassembler
			  * to disambiguate multiple labels that occur in
			  * the same position in the object file.
			  *)

type 'a coerce = 'a * coercion list

(* Operands for most instructions *)
type genop =
    Immed of int32
  | Reg of reg
  | Addr of identifier
  | Prjr of reg coerce * int32 * (scale * reg) option
  | Prjl of identifier coerce * int32 * (scale * reg) option

type condition = 
    Above | AboveEq | Below | BelowEq | Eq | Greater | GreaterEq | Less
  | LessEq | NotEq | NotOverflow | NotSign | Overflow | ParityEven
  | ParityOdd | Sign

let negate_condition c = 
  match c with
    Above -> BelowEq
  | AboveEq -> Below
  | Below -> AboveEq
  | BelowEq -> Above
  | Eq -> NotEq
  | Greater -> LessEq
  | GreaterEq -> Less
  | Less -> GreaterEq
  | LessEq -> Greater
  | NotEq -> Eq
  | NotOverflow -> Overflow
  | NotSign -> Sign
  | Overflow -> NotOverflow
  | ParityEven -> ParityOdd
  | ParityOdd -> ParityEven
  | Sign -> NotSign
;;

type arithbin = Adc | Add | And | Imul2 | Or | Sbb | Sub | Xor
type arithun = Dec | Inc | Neg | Not
type arithmd = Div | Idiv | Imul1 | Mul
type arithsr = Rcl | Rcr | Rol | Ror | Sal | Sar | Shl | Shr

type conv = Cbw | Cdq | Cwd | Cwde;;

type mallocarg = 
    Mbytes of scale
  | Mprod of mallocarg list
  | Mbytearray of scale * int32;;

(* Operations that never take arguments *)
type fpnoargs = 
    F2xm1 | Fabs | Fchs | Fclex | Fnclex | Fcompp | Fucompp | Fcos | Fdecstp 
  | Fincstp | Finit | Fninit  | Fld1 | Fldz | Fldpi | Fldl2e | Fldl2t 
  | Fldlg2 | Fldln2 | Fnop | Fpatan | Fprem | Fprem1 | Fptan | Frndint 
  | Fscale | Fsin | Fsincos | Fsqrt | Ftst | Fwait | Fxam | Fxtract 
  | Fyl2x | Fyl2xp1

(* Floating point instruction argument configurations *)
type fpargs =
    FPstack of int           (* ST(i) *)
  | FPstack2 of bool * int   (* true => ST, ST(i); false => ST(i),ST *)
  | FPgenop of scale * genop (* Explicit memory or register operand *)
    
(* Operations that take arguments *)
type fpsomeargs =
(* generic binary instructions *)
    Fadd | Fcom | Fdiv | Fdivr | Fmul | Fsub | Fsubr | Fucom | Fxch
(* integer instructions *)
  | Fiadd | Ficom | Ficomp | Fidiv | Fidivr | Fimul | Fisub | Fisubr
(* instructions that pop an argument *)
  | Faddp | Fcomp | Fdivp | Fdivrp | Fmulp | Fsubp | Fsubrp | Fucomp
(* unary load and store instructions *)
  | Fst | Fstp | Fist | Fistp | Fld | Fild
(* change fp register tag to empty *)
  | Ffree
(* comparison operations that write condition codes to main unit *)
(* implemented only on the pentium pro and better processors *)
  | Fcomi | Fcomip | Fucomi | Fucomip
(* Store Status Word *)
  | Fstsw | Fnstsw
(* unimplemented: 
   (* Load and store control word *)
   | Fldcw | Fstcw | Fnstcw
   (* Load environment state *)
   | Fldenv | Fldenvw | Fldenvd 
   | Fstenv | Fstenvw | Fstenvd | Fnstenv | Fnstenvw | Fnstenvd
   (* Restore and Save coprocessor state: *)
   | Frstor | Frstorw | Frstord 
   | Fsave | Fsavew | Fsaved | Fnsave | Fnsavew | Fnsaved
   (* Other *)
   | Fbstp
*)

(* This is a subset of the x86 32-bit instructions that we might want to
 * cover.  Does not include floating-point support yet.
 *)
type instruction = 
    ArithBin of arithbin * genop * genop
	                        (* binary arithmetic operation *)
  | ArithUn of arithun * genop  (* unary arithmetic operation *)
  | ArithMD of arithmd * genop  (* multiply/division *)
  | ArithSR of arithsr * genop * int32 option (* None = ECX, shift/rotate *)
  | Bswap of reg                (* toggle endianess *)
  | Call of genop coerce        (* push return addr, jump to label *)
  | Clc                      	(* clear carry flag *)
  | Cmc                      	(* toggle carry flag *)
  | Cmovcc of condition * reg * genop coerce
	                        (* conditional move *)
  | Cmp of (genop coerce) * (genop coerce) (* compare *)
  | Conv of conv                (* various 8/16/32 -> 16/32/64 ops *)
  | Imul3 of reg * genop * int32(* signed multiply 3 arg form *)
  | Int of int8               	(* interrupt:  system call *)
  | Into                        (* interrupt if overflow set *)
  | Jcc of condition * identifier coerce * instruction list option
	                        (* jump on condition *)
	                        (* instructions are no-ops but coerce the context *)
  | Jecxz of identifier coerce * instruction list option
                                (* jump if ECX is zero *)
	                        (* instructions are no-ops but coerce the context *)
  | Jmp of genop coerce      	(* jump *)
  | Lahf                     	(* move flags into Eax (exc. overflow) *)
  | Lea of reg * genop          (* move effective address into register *)
  | Loopd of identifier coerce * bool option
                                (* decrement ECX and if result nonzero jump
				   if bool present jump only if
				     nonzero Z flag equals the boolean *)
  | Mov of genop * (genop coerce)
                                (* move, load, store *)
  | Movpart of bool * genop * reg_part * genop * reg_part
	                        (* Move/zero extend/sign/extend/trunc part word
                                   to another part word.  One genop must be a
                                   register. *)
  | Nop                      	(* no-op *)
  | Pop of genop             	(* stack pop *)
  | Popad                    	(* pop all registers (32-bit) *)
  | Popfd                    	(* pop eflags *)
  | Push of genop coerce     	(* push onto stack *)
  | Pushad                   	(* push all registers (32-bit) *)
  | Pushfd                   	(* push eflags *)
  | Rdtsc                       (* Read time stamp counter in EDX:EAX *)
  | Retn of int32 option       	(* return "near" (i.e., doesn't touch CS) *)
  | Sahf                     	(* move ah into flags (exc. overflow) *)
  | Setcc of condition * genop	(* set dest=1/0 if condition is true/false *)
  | Shld of genop * reg * int32 option (* None = ECX, shift 64 *)
  | Shrd of genop * reg * int32 option (* None = ECX, shift 64 *)
  | Stc                      	(* set carry flag *)
  | Test of genop * genop   	(* test *)
  | Xchg of genop * reg         (* exchange *)
(* operations specific to x86tal *)
  | Coerce of genop coerce 
    (* coerce an object.  The object's location is a "path" off of a register*)
  | CoerceName of identifier coerce
    (* coerce an object indirectly through a name *)
  | Comment of string
  | Fallthru of con list  
    (* only valid when preceeding a label L.  effectively, 
     * a jmp L[c1,...,cn]. *)
  | Malloc of identifier * int32 * (mallocarg option)
    (* Malloc(x,i,m) allocates an object of i bytes returning the pointer
     * in eax, and wrecking all other registers but ebx.  Upon return,
     * x is added as a Unique pointer to an object described by mallocarg.
     * If the mallocarg is not present, then we assume a tuple of 32-bit
     * words (i.e., i/4).  Upon return, eax has type Cname(x) so that 
     * it may be initialized.  
     *)
  | Proof of (identifier * con list) list
    (* a list of proof rules that coerce the context *)
  | Unpack of identifier * reg * genop coerce
    (* effectively a move *)
  | Sunpack of identifier * genop
    (* mov to and from a stack slot *)
  | Nameobj of identifier * genop
    (* genop specifies a value in a register or memory.  We introduce
     * a new Kname (identifier) and replace the type of the object with 
     * Cname(identifier).  The identifier is assigned MayAlias in the 
     * capability.   Used to refine the type of something where we may
     * make multiple copies of it.
     *)
  | ForgetUnique of identifier
     (* ForgetUnique(x) assumes x is a unique Kname in the current capability.
      * Changes x to MayAlias which allows objects of type Cname(x) to be
      * coerced to the type that the capability assigns x.
      *)
  | RemoveName of identifier
     (* Removes the name from the current capability. Note that Cname(x) 
      * will still be a valid constructor -- you just won't be able to do
      * much with it.  *)
(* Floating Point Instructions *)
  | FPnoargs of fpnoargs
  | FPsomeargs of fpsomeargs * fpargs
(* Cyclone *)
  | CgStart of identifier * con
  | CgDump of reg * identifier * reg * identifier
  | CgHole of reg * identifier * identifier
  | CgHoleJmp of identifier * identifier coerce
  | CgHoleJcc of condition * identifier * identifier coerce * instruction list option
  | CgFill of reg * reg * identifier * identifier * reg
  | CgFillJmp of reg * reg * identifier * identifier * reg * identifier * identifier
  | CgFillJcc of reg * reg * identifier * identifier * reg * identifier * identifier
  | CgForget of identifier * identifier
  | CgEnd of reg
(* LX Instructions *)
  | Letprod of identifier list * con 
  | Letroll of identifier * con
  | Vcase of int32 * con * identifier * genop coerce 
(* end LX *)

(* Notes on incompleteness:

+ No floating point support.
+ No BCD ops - who wants them?
+ No string ops - ditto.
+ No ENTER/LEAVE - ditto.
+ No BOUND - ditto.
+ No system ops - we're supporting user space application only for now.
+ No concurrentcy ops (xadd, cmpxchg, lock, ...)
   - not supporting concurrency yet.
+ No bit ops - not sure if these are useful.
+ No XLAT - ditto.
+ No CPUID.
+ No far ops - supporting flat model only.

*)

type code_block = identifier * con option * instruction vector

(* Cyclone *)
type template = identifier * con * code_block list
(* End Cyclone *)

(**********************************************************************)
(* Static Data *)

(* There are some other things we could add here such as exnnames *)

type data_item =
    Dlabel of identifier coerce
  | Dbytes of string
  | D2bytes of int16
  | D4bytes of int32 coerce
  | Drep of rep_item * string option ref
  | Dfloat32 of f32
  | Dfloat64 of f64
  | Djunk
  | Dup
  | Ddown
;;

type data_block = identifier * int32 * con option * (data_item list) coerce

(**********************************************************************)
(* Compilation Units *)

type kind_abbrev = identifier * kind;;     (* LX *)
type con_abbrev = identifier * con;;

type int_con = identifier * kind * int_con_def
and int_con_def = AbsCon | BoundCon of con | ConcCon of con;;

type tal_int =
    { int_abbrevs : con_abbrev vector;
      int_kindabbrevs : kind_abbrev vector;  (* LX *)
      int_cons : int_con vector;
      int_vals : (identifier * con) vector
    }
;;

type tal_int_type = {
    it_cons : int_con list;
    it_vals : (identifier * con) list
  } 
;;

type con_block = (identifier * kind * con);;

type tal_imp = 
    { imp_abbrevs : con_abbrev vector;
      imp_kindabbrevs : kind_abbrev vector; (* LX *)
      con_blocks : con_block vector;
      code_blocks : code_block vector;
      data_blocks : data_block vector;
(* Cyclone *)
      templates : template vector
(* End Cyclone *)
    } 
;;

type int_ref = 
    Int_filename of string
  | Int_data of string * tal_int

(* Parser produces a pre-module that hasn't read the interfaces in
   yet. We convert a tal_pre_mod into a tal_mod by reading in the files. *)
type tal_pre_mod =
    { import_refs : int_ref vector;
      export_refs : int_ref vector;
      pre_imp     : tal_imp;
    } 

type tal_mod =
    { imports : tal_int vector;
      exports : tal_int vector;
      imp     : tal_imp;
    } 

(* Return true if instruction has no run-time significance *)
let is_virtual_instruction i =
  match i with
    Coerce _ -> true
  | CoerceName _ -> true
  | Comment _ -> true
  | Proof _ -> true
  | Unpack (_,r,(Reg r',cs)) when r = r' -> true
  | Nameobj (_,_) -> true
  | ForgetUnique _ -> true
  | RemoveName _ -> true
  | Fallthru _ -> true
(* LX *)
  | Vcase _ -> true
  | Letprod _ -> true
  | Letroll _ -> true
(* end LX *)
  | _ -> false

(* EOF: x86tal.ml *)
