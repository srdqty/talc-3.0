(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* tal.mli
 * 
 * This is a fairly complete specification of the abstract syntax for
 * a typed version of 32-bit (flat-model) iNTEL 80386, Pentium, Pentium Pro,
 * and/or Pentium II assembly language.  
 *
 *
 *)

(**********************************************************************)
(* Miscellanous stuff *)

open Utilities;;
open Numtypes;;
open Identifier;;

(* Used during verification for templates.  Template labels are relative,
   top-level labels are absolute. Names chosen not to conflict with objfile. *)
type mode = Abs | Rel

type scale = Byte1 | Byte2 | Byte4 | Byte8
val scale_to_int32 : scale -> int32
val scale_to_int   : scale -> int
type reg = Eax | Ebx | Ecx | Edx | Esi | Edi | Ebp | Esp | Virt of identifier
val compare_regs : reg -> reg -> int

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
  | Kint                  (* integer "sort" for array indices *)
  | Kbool                 (* boolean kind *) 
  | Karrow of kind * kind (* functions from constructors to constructors *)
  | Kprod of kind list    (* tuples of constructors *)
  | Kname                 (* "names" used for alias information *)
  | Kcap                  (* capabilities for alias information *)
  | Kms                   (* machine states *)
(* LX *)
  | Ksum of kind list     (* sum constructors *)
  | Kvar of identifier    (* for recursive kinds *)
  | Kmu of  kmu_schema * identifier  (* recursive kinds *)
(* end LX *)
and kmu_schema = (identifier * kind) list 
and kind = 
   { mutable rkind : rkind;                            (* the raw kind *)
      mutable freekindvars : identifier Set.set option; (* free kind vars *)
      mutable kabbrev : identifier option
    } 

val defkind : rkind -> kind
(* constructors for various for kinds *)
val kbyte : scale -> kind
val ktype : kind
val kmemi : int32 -> kind
val kmem : kind
val kstack : kind
val kint : kind
val kbool : kind
val karrow : kind -> kind -> kind
val kprod : kind list -> kind
val ksum : kind list -> kind
val kunit : kind
val kvar : identifier  -> kind 
val kmu : kmu_schema -> identifier -> kind
val kname : kind
val kcap : kind
val kms : kind 

val k4byte : kind;; (* 32 bit values *)
(**********************************************************************)
(* Type Constructors *)


(* primitive constructors *)
type primcon = 
    PCbytes of scale      (* : Kbyte s *)
  | PCfloat32             (* : Kbyte 4 *)
  | PCfloat64             (* : Kbyte 8 *)
  | PCjunk  of int32      (* : junk of kind Tm i *)
  | PCjunkbytes of scale  (* : junk of kind Kbyte s *)
  | PCint   of int32      (* : Kint  *)
  | PCtrue                (* : Kbool *)
  | PCfalse               (* : Kbool *)

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

(* floating point register may be empty, full, or in an unknown state. *)
type fpreg = FPempty | FPfull | FPany
(* floating point stack *)
type fpstack

(* type constructors *)
type con_state = NotNorm | Normalized | WeakHead
type rcon = 
(* the language portion of con's *)
    Cvar of identifier                  (* type variables *)
  | Clam of identifier * kind * con     (* functions *)
  | Capp of con * con                   (* function application *)
  | Ctuple of con list                  (* tuples of constructors *)
  | Cproj of int * con                  (* tuple projection *)
(* ---- LX ---- *)
  | Cinj of int * con * kind              (* intro form for sum constructors *)
  | Ccase of con * identifier * con list  (* elim form for sum constructors *)
  | Cfold of kind * con                   (* intro for recursive cons *)
  | Cpr of identifier * (identifier * identifier * kind * identifier * kind * con) list
       (* primitive recursion
	  a set of mututally recursive functions, indexed by the third identifier *)
  | Cvoid of kind 
(*  | Cprnat of identifier * kind * con * identifier * con *)
(* -- end LX -- *)

(* the "type" portion of con's *)
  | Clab of identifier                  (* a generative, globally-scoped 
					 * type.  See Glew & Morrisett's
					 * MTAL paper for description. *)
  | Cprim of primcon                    (* see above *)
  | Crec of (identifier * kind * con) list  (* mutually recursive con's *)
  | Cforall of identifier * kind * con      (* forall-polymorphism *)
  | Cexist of identifier * kind * con * con (* Exists[i:k such that c1].c2 *)
  | Ccode of con                         (* Kms -> K4byte *)
  | Cms of machine_state                  (* describes an entry point in 
					     * a hunk of code.  The machine
					     * state is described below and
					     * is in essence a pre-condition
					     * that must be satisfied before
					     * control may be transferred to
					     * this code. *)
  | Cmsjoin of con * con
	(* constructor on the right overwrites register bindings on the left *)
  | Chptr of int32 list*con option*(con*variance) option
	(* A (potential) pointer to a heap-allocated object
	 * (i.e., a Cprod, Csum, Carray, etc.)
	 * In reality, an object with this type can be any one of
	 * the integers listed (in the range [0..4095]) or a pointer
	 * to a heap-allocated object of kind Kmem described by the
	 * con option.  The con*variance option is a potential "tag"
	 * as described in Glew's ICFP paper.  NEAL:  fill in some 
	 * details here.  
	 *)
  | Cfield of con*variance              (* a "field" in a struct type *)
  | Cprod of con list                   (* a struct -- made of fields *)
  | Csum of con list                    (* a disjoint union -- the cons must
					 * be Cprod's where the first field
					 * is a unique singleton int. *)
  | Carray of con*con        		(* Kint,Kmem -> Kmem *)
  | Csing of con             		(* Kint -> K4byte *)
(* the "stack" portion of con's *)
  | Csptr of con                        (* Kstack -> K4byte *)
  | Cempty
  | Ccons of con * con                  (* Kmem/Kbyte,Kstack -> Kstack *)
  | Cappend of con * con                (* Kstack,Kstack -> Kstack *)
(* the arithmetic and logical portion of the con's *)
  | Clog of log * con list              (* logical operator applied to args *)
  | Cif of con * con                    (* if c1:bool then c2 else void *)
(* Alias and capability portion of con's:
   For various reasons, we need to be able to track certain aliasing
   relationships among values.  To do so, we use "names" (identifiers
   of kind Kname) to statically name a dynamic location.  If both Eax and
   Ebx have type Cname(x) where x is a name, then we know that Eax and
   Ebx contain the same value.  We separately keep track of the type
   of x in a data structure called the capability.  For instance, the
   capability might say that at a control-flow point, x has type 
   ^*[B4,B4] indicating that Eax and Ebx contain the same pointer to
   a pair of 4-byte integers.  In addition, the capability tracks
   whether x is a "unique" location or whether x may have aliases.
   Unique names' types can change.  For instance, if x is unique, then
   we can write Ebx into the first field of Eax and x will now have
   the (recursive) type ^*[Cname(x),B4].  (Allocation returns unique
   names and intialization changes the type of the name.)  In this
   respect, unique names are like linear types, but the level of 
   indirection allows us to have multiple copies of the value.  If x
   is not unique, then it may have aliases and thus we cannot change
   the type of x arbitrarily.  However, we can refine the type of x
   (if for instance, x is a disjoint union of some sort.)  The ability
   to refine the type of a location (as opposed to a register/stack slot/
   etc.) allows us to simultaneously refine the type of a bunch of copies
   of some object.  If in the example above, we try to call a function
   where Eax and Ebx are supposed to have type ^*[B4,B4] (and not Cname(x))
   then if x is unique, the call will fail.  This prevents one from hiding
   an alias to a supposedly unique location  That is, when x is unique,
   a value of type Cname(x) cannot just be coerced to ^*[B4,B4].  However
   if x may alias, then a value of type Cname(x) may be coerced to 
   the type that x has, thereby "forgetting" the aliasing relationships
   with other values.  
 *)
  | Cname of con
  | Ccap of (identifier,(alias_info*con)) Dict.dict
  | Cjoin of con list
  | Ctagof of con    (* :Kname -> K4byte the tag of a sum value *)
(* Cyclone *)
  | Ctmpl of con * con option
        * (identifier * con) list * (identifier * con) list
    (* Code template: pre-condition * post-condition option * 
       (labels * con) list * (holes * con) list *)
  | Ctptr of identifier
    (* Pointer into code-generation region -- identifier is a type-variable
       branding the region and template within it pointed to. *)
  | Ctrgn of con * con option
        * (identifier * (identifier * con) list * (identifier * con) list) list
    (* Code-generation region: pre-condition * post-condition option *
       (branded template) list
       where a branded template is the same as a Ctmpl without 
       the pre- and post- conditions and with a unique identifier specifying 
       a particular template within the region. *)
(* End Cyclone *)
(* an explicit substitution *)
  | Csubst of con * esubst
(* Enil - empty substitution, Es replace var with con, Eo left-to-composition*)
(* reflecting type representation code into the type-checker. *)
  | Cr of rep_item
(* the type hack portion*)
  | Ctypeof of identifier

and esubst = Enil | Es of identifier * con | Eo of esubst * esubst
and con = 
  { mutable rcon     : rcon;   (* "raw" constructor *)
    mutable con_state : con_state;   
    mutable freevars : (identifier Set.set * identifier Set.set) option;
    mutable hash    : int;
    mutable abbrev : identifier option
} 

and machine_state
and ccinfo =
    CCnoinfo
  | CCcmp of con*con
  | CCtest of con*con
and rep_item = RCon of con | RKind of kind | RLabel of identifier

;;

(* the empty floating point stack            *)
val fpstack_empty       : fpstack
(* true if argument is the empty stack       *)
val fpstack_isempty     : fpstack -> bool
(* return true if two fpstacks are equal     *)
val fpstack_equal       : fpstack -> fpstack -> bool
(* return true if two fpstacks are leq       *)
val fpstack_leq         : fpstack -> fpstack -> bool

val fpstack_get_fpreg   : fpstack -> int -> fpreg
val fpstack_set_fpreg   : fpstack -> int -> fpreg -> fpstack

(* true if i is an initialized register      *)
(* if not 0 <= i <= 7 then raise invalid_arg *)
val fpstack_inrange     : fpstack -> int -> bool
(* true if register i is empty               *)
val fpstack_isempty_reg : fpstack -> int -> bool
(* true if register i is full                *)
val fpstack_isfull_reg  : fpstack -> int -> bool
(* set the register i to full                *)
val fpstack_init_reg    : fpstack -> int -> fpstack
(* set the register i to empty               *)
val fpstack_free_reg    : fpstack -> int -> fpstack
(* make the status of the register any. *)
val fpstack_hide_reg    : fpstack -> int -> fpstack
(* create a stack of height i                *)
(* if not 0 <= i <= 8 then raise invalid_arg *)
val fpstack_create      : int -> fpstack

(* Adjust the stack by pushing/popping the number of floats indicated by int.
 * Positive int arguments are pops, negative ints are pushes. 
 * Call first arg on stack overflow/underflow errors.
 * Push or pop a maximum of 2 elements at a time. Raise invalid_arg otherwise.
 *)
val fpstack_adjust      : (string -> unit) -> fpstack -> int -> fpstack

(* Bump the top-of-stack pointer by a max of 2 in either direction.  
 * Has the effect of rotating empty or filled stack slots with respect to
 * the top of the stack.
 * Positive int arguments rotate in the direction of popping, negative in the
 * direction of pushing. 
 * Do *not* fill or empty stack slots while rotating.  
 * Do *not* check for overflow or underflow.
 *)
val fpstack_rotate      : fpstack -> int -> fpstack

(* Hash Consing *)
val hash_clear : unit -> unit
val hash_find : rcon -> con
val hash_mem : rcon -> bool

(* default way to build a constructor -- always safe *)
val defcon : rcon -> con
(* call only if you know that the constructor is "primitive" meaning
 * no free variables and fully normalize. *)
val prcon  : rcon -> con
(* call only if you know that the constructor is already in weak-head-
 * normal form. *)
val wcon   : rcon -> con

val pcbytes : scale -> con
val cbyte8 : con
val cbyte4 : con
val cbyte2 : con
val cbyte1 : con

val pcfloat32 : con
val pcfloat64 : con

val pcjunk : int32 -> con
val pcjunk4 : con
val pcjunkbytes : scale -> con
val pcint : int32 -> con
val pctrue : con
val pcfalse : con

val cvar : identifier -> con
val clam : identifier -> kind -> con -> con
val capp : con -> con -> con
val ctuple : con list -> con
val cproj : con -> int -> con
(* ---- LX ---- *)
val cinj : int -> con -> kind -> con
val ccase : con -> identifier -> con list -> con
val cfold : kind -> con  -> con
val cpr : identifier ->
(identifier * identifier * kind * identifier * kind * con) list  -> con
val cvoid : kind -> con
(* val cprnat : identifier -> kind -> con -> identifier -> con -> con *)
(* -- end LX -- *)
val clab : identifier -> con
val crec : (identifier * kind * con) list -> con
val cforall : identifier -> kind -> con -> con
val cexist : identifier -> kind -> con -> con (* standard exists i:k.c *)
val cexistp : identifier -> kind -> con -> con -> con (* E[v:k such that c1].c2 *)
val ccode : con -> con
val ccode_ms : machine_state -> con
val cms : machine_state -> con
val cmsjoin : con -> con -> con
val ccode_l : (reg * con) list -> con
val ccode_l_fps : (reg * con) list -> fpstack -> con
val ccode_l_cap : (reg * con) list -> con -> con (* second con is capability *)
val ccode_l_cap_fps : (reg * con) list -> con -> fpstack -> con
val chptr : int32 list -> con option -> (con*variance) option -> con
val cptr : con -> con
val cfield : con -> variance -> con
val cprod : con list -> con
val cprod_b : con list -> con   (* Boxed product *)
val csum : con list -> con
val carray : con -> con -> con
val carray_s : identifier -> con -> con  (* E[v:Sint].^*[S(v)^r,array(v,c)] *)
val csing : con -> con
val csptr : con -> con
val cempty : con
val ccons : con -> con -> con
val cappend : con -> con -> con
val csubst : con -> esubst -> con
val cr : rep_item -> con 
(* Cyclone *)
val ctmpl : (con * con option * (identifier * con) list * (identifier * con) list) -> con
val ctptr : identifier -> con
val ctrgn : (con * con option * (identifier * (identifier * con) list * (identifier * con) list) list) -> con
(* Cyclone end *)
val csubst : con -> esubst -> con
val cif : con -> con -> con
val clog : log -> con list -> con
val cempty_cap : con
val ccap : (identifier,(alias_info * con)) Dict.dict -> con
val cjoin : con list -> con
val cname : con -> con
val ctagof : con -> con
val ctypeof : identifier -> con
val ms_empty : machine_state;;
val ms_map : (con -> con) -> machine_state -> machine_state;;
val ms_app : (con -> unit) -> machine_state -> unit;;
val ms_join : machine_state -> machine_state -> machine_state;;

(* integer and logical portion of cons *)
val cadd : con list -> con
val csub : con -> con -> con
val cmuls : int32 -> con -> con
val cmulu : int32 -> con -> con

val cand : con list -> con
val cor : con list -> con
val cnot : con -> con
val cimplies : con -> con -> con
val ciff : con -> con -> con
val clts : con -> con -> con
val cltu : con -> con -> con
val cltes : con -> con -> con
val clteu : con -> con -> con
val ceq : con -> con -> con
val cne : con -> con -> con
val cgts : con -> con -> con
val cgtu : con -> con -> con
val cgtes : con -> con -> con
val cgteu : con -> con -> con

(* the register portion of the machine state *)
val ms_get_reg : machine_state -> reg -> con;; (* throws Dict.Absent *)
val ms_set_reg : machine_state -> reg -> con -> machine_state;;
val ms_set_regs : machine_state -> (reg * con) list -> machine_state;;
val ms_del_reg : machine_state -> reg -> machine_state;;
val ms_del_regs : machine_state -> reg list -> machine_state;;
val ms_map_reg : (con -> con) -> machine_state -> machine_state;;
val ms_app_reg : (reg -> con -> 'a) -> machine_state -> unit;;
val ms_fold_reg : (reg -> con -> 'a -> 'a) -> machine_state -> 'a -> 'a;;

(* the floating point portion of the machine state *)
val ms_get_fpstack : machine_state -> fpstack;;
val ms_set_fpstack : machine_state -> fpstack -> machine_state;;

(* The condition code portion of the machine state *)
val ms_get_cc : machine_state -> ccinfo;;
val ms_set_cc : machine_state -> ccinfo -> machine_state
    (* ms_set_cc saves the old ccinfo which can be restored by ms_restore_cc *)
;;
val ms_restore_cc : machine_state -> unit;; (* destructive *)

(* the capability and alias portion of the machine state *)
val ms_get_cap : machine_state -> con;;
val ms_set_cap : machine_state -> con -> machine_state;;

(* Cyclone : FMS *)
(* alias capability utility functions for cons. *)
(* warning: These functions know nothing about abbreviations and do not care
 about well-formedness.  They just look for capability types at the top level. 
*)

(* in a list of joins takes the first dictionary containing the variable. *)
val cap_get_name : con -> identifier -> (alias_info * con) option
(* combines all the dictionaries in a join, so that a name will not be
   inadvertently duplicated. *)
val cap_set_name : con -> identifier -> (alias_info * con) -> con option

(* Cyclone End *)
(* Pointer integers *)
val min_pointer_integer : int32;;
val is_non_pointer_integer : int32 -> bool;;

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
	         
type 'a coerce = 'a * coercion list (* (r,[c1;...;cn]) is c1(...cn(r)...) *)

(* Operands for most instructions *)
type genop =
    Immed of int32
  | Reg of reg
  | Addr of identifier
  | Prjr of reg coerce * int32 * (scale * reg) option
  | Prjl of identifier coerce * int32 * (scale * reg) option

(* Note: Above/Below are unsigned, Greater/Less are signed *)
type condition = 
    Above | AboveEq | Below | BelowEq | Eq | Greater | GreaterEq | Less
  | LessEq | NotEq | NotOverflow | NotSign | Overflow | ParityEven
  | ParityOdd | Sign

val negate_condition : condition -> condition;;

type arithbin = 
    Adc 
  | Add   (* (Un)signed Add *)
  | And   (* Bitwise And *)
  | Imul2 (* Signed Multiply, 2 operands *)
  | Or    (* Bitwise Or *)
  | Sbb   
  | Sub   (* (Un)signed Subtract *)
  | Xor   (* Bitwise Xor *)
type arithun = 
    Dec    
  | Inc 
  | Neg   (* 0 - op *)
  | Not   (* 1's complement *)
type arithmd = 
    Div   (* Unsigned *)
  | Idiv  (* Signed divide. Quotient -> Eax, Remainder -> Edx *) 
  | Imul1 (* Signed multiply. Result -> Edx Eax *)
  | Mul   (* Unsigned *)
type arithsr = 
    Rcl 
  | Rcr 
  | Rol 
  | Ror 
  | Sal   (* Signed *) 
  | Sar   (* Signed *)
  | Shl   (* Unsigned *)
  | Shr   (* Unsigned *)

type conv = Cbw | Cdq | Cwd | Cwde;;

type mallocarg = 
    Mbytes of scale
  | Mprod of mallocarg list
  | Mbytearray of scale * int32;;

(* Floating point operations that never take arguments *)
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
  | FPgenop of scale * genop (* Memory or register operand *)
    
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
   Fldcw | Fstcw | Fnstcw
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
  | ArithSR of arithsr * genop * int32 option (* NONE = ECX, shift/rotate *)
  | Bswap of reg                (* toggle endianess *)
  | Call of genop coerce        (* push return addr, jump to label *)
  | Clc                      	(* clear carry flag *)
  | Cmc                      	(* toggle carry flag *)
  | Cmovcc of condition * reg * genop coerce
	                        (* conditional move *)
  | Cmp of (genop coerce) * (genop coerce) (* compare *)
  | Conv of conv                (* various 8/16/32 -> 16/32/64 ops *)
  | Imul3 of reg * genop * int32(* signed multiply 3 arg form *)
  | Int of int8                	(* interrupt:  system call *)
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
	                        (* Move (zero-extend(t)/sign-extend(f))
				   part word to another part word. 
				   if part1=part2 then just move.
				   One genop must be a register. *)
  | Nop                      	(* no-op *)
  | Pop of genop             	(* stack pop *)
  | Popad                    	(* pop all registers (32-bit) *)
  | Popfd                    	(* pop eflags *)
  | Push of genop coerce     	(* push onto stack *)
  | Pushad                   	(* push all registers (32-bit) *)
  | Pushfd                   	(* push eflags *)
  | Rdtsc                       (* read time-stamp counter into EDX:EAX *)
  | Retn of int32 option       	(* return "near" (i.e., doesn't touch CS) *)
  | Sahf                     	(* move ah into flags (exc. overflow) *)
  | Setcc of condition * genop	(* set dest=1/0 if condition is true/false *)
  | Shld of genop * reg * int32 option (* NONE = ECX, shift 64 *)
  | Shrd of genop * reg * int32 option (* NONE = ECX, shift 64 *)
  | Stc                      	(* set carry flag *)
  | Test of genop * genop    	(* test *)
  | Xchg of genop * reg         (* exchange *)
(* operations specific to x86tal *)
  | Coerce of genop coerce 
    (* coerce an object.  The object's location is a "path" off of a register*)
  | CoerceName of identifier coerce
    (* coerce a named object *)
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
    (* mov to and from stack slot *)
  | Nameobj of identifier * genop
    (* genop specifies a value in a register or memory.  We introduce
     * a new Kname (identifier) and replace the type of the object with 
     * Cname(identifier).  The identifier is assigned MayAlias in the 
     * capability.   Used to refine the type of something where we may
     * make multiple copies of it. *)
  | ForgetUnique of identifier
     (* ForgetUnique(x) assumes x is a unique Kname in the current capability.
      * Changes x to MayAlias which allows objects of type Cname(x) to be
      * coerced to the type that the capability assigns x. *)
  | RemoveName of identifier
     (* Removes the name from the current capability. Note that Cname(x) 
      * will still be a valid constructor -- you just won't be able to do
      * much with it.  *)
(* Floating Point Instructions *)
  | FPnoargs of fpnoargs
  | FPsomeargs of fpsomeargs * fpargs
(* Cyclone *)
  | CgStart of identifier * con
    (* Start a new code-generation region.  This region will be named by the
       identifier and start with pre- and post-condition con.  The region
       will be put in EAX *)
  | CgDump of reg * identifier * reg * identifier
    (* Copy a template into a cg-region (branding it with ty-var, and
       putting a pointer to the copy in cg-ptr)
       cg-region * ty-var * cg-ptr * template-label
       *)
  | CgHole of reg * identifier * identifier
    (* Template instruction: (register to fill) * template-label * hole-label *)
  | CgHoleJmp of identifier * identifier coerce
    (* Template instruction: template-label * (hole-name coerced) *)
  | CgHoleJcc of condition * identifier * identifier coerce * instruction list option
    (* Template instruction: Same as CgHoleJmp with addition of condition. *)
  | CgFill of reg * reg * identifier * identifier * reg
    (* Fill a CgHole.
       cg-region * cg-ptr * template-label * hole-label * fill-value *)
  | CgFillJmp of reg * reg * identifier * identifier * reg * identifier * identifier
    (* Fill a CgHoleJmp.
       cg-region * cg-ptr * hole-template-label * hole-label * 
       target-template-label * target-label *)
  | CgFillJcc of reg * reg * identifier * identifier * reg * identifier * identifier
    (* Same as CgFillJmp *)
  | CgForget of identifier * identifier
    (* Forget a template in a cg-region once it has been filled.
       cg-region-name * cg-ptr-tyvar *)
  | CgEnd of reg
    (* Given a cg-region with no holes, turn it into a code pointer. *)
(* LX Instructions *)
  | Letprod of identifier list * con 
  | Letroll of identifier * con
  | Vcase of int32 * con * identifier * genop coerce 
(* end LX *)

val is_virtual_instruction : instruction -> bool
(* End Cyclone *)

(* Notes on incompleteness:

+ No floating point support.
+ No BCD ops - who wants them?
+ No string ops - ditto.
+ No ENTER/LEAVE - ditto.
+ No BOUND - ditto.
+ No system ops - we're supporting user space applications only for now.
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

(* A datablock is one of the following:
 *   Exnname:
 *     Dexnname
 *     Cannot coerce
 *   Tuple:
 *     (Dlabel | Dbytes | D2bytes | D4bytes | Djunk)^*
 *     Which may then be coerced.
 *)

(* int represents alignment in bytes -- defaults to 4. *)
type data_block = identifier * int32 * con option * (data_item list) coerce

(**********************************************************************)
(* Compilation Units *)

type kind_abbrev = identifier * kind;; (* LX *)

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

type con_block = identifier * kind * con;;

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

(* EOF: x86tal.mli *)
