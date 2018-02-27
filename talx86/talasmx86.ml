(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Richard Samuels, David Walker       *)
(*     September 1998, all rights reserved.                           *)
(**********************************************************************)

(* Talasmx86
 * x86 TAL assembler, generic stuff is in Talasm
 *)

(* Changes:
   SCW Nov/99
   - added type/kind representations to static data

   SCW Oct/99
   - added LoadCode instruction that loads and typechecks code 

   RLS 3/30/99: 
   - Added support for Movpart.

   RLS 3/3/99:
   - Added Cyclone features.
   - Changed malloc_macro to use an absolute call, Cyclone crashes otherwise
   - Removed exception macros for new TAL definitions.
   - asm_env.a_symdict changed to asm_env.a_r_symdict, which is a symbol_dict
     ref instead of a symbol_dict, to allow Cyclone hole macros to function.

   RLS 11/15/98:
   Added signed_immed_len and signed_immed_len_0 to distinguish
   the lengths of immeds that can be signed or unsigned from those that
   are always signed. Replaced immed_len with signed_immed_len in
   length_of_instr (push) and length_of_operand because 1-byte immeds
   were being used in some places instead of 4-byte immeds.

   RLS 11/10/98:
   Specialized length_of_instr for Imul2, since it was giving the wrong
   length for the special kinds of Imul2 instructions.
*)

open Numtypes;;
open Identifier;;
open Tal;;
open Objfile;;

(* Cyclone *)
open Cyclone;;
(* End Cyclone *)


(* needs Stringchan, but does not open *)

(***** asm_env *****)

(* NG: This really belongs somewhere else but the dependancies are all
 *     screwed up, so for now, it it here.
 *)

(* asm_env
 * A structure containing the variables needed to assemble instructions.
 * This is used by asm.ml and SHOULD NOT CHANGE between implementations.
 *)

type asm_env = 
    { a_schan : Stringchan.string_chan;  (* The output channel. *)
      a_r_symdict : symbol_dict ref;     (* The symbol dictionary. *)
      mutable a_hole_syms: symbol list;  (* Hole symbols in code block. *)
      a_secnum : int;		      (* The number of the section. *)
      mutable a_pos_offset : int;     (* How real pos differs from expected.*)
      mutable a_r_reloclist : relocation list ref (*Relocations are put here.*)
    }
;;

(***** x86 Specific Functions *****)

(***** Types *****)

(* operand_record: a record specifying a register and a regMem operand.
   Produced by sort_operands. *)
type operand_record = {
    op_direction : int;		(* The value of the direction flag. *)
    op_regOp : reg;			(* The register operand. *)
    op_regMemOp : genop		(* The memory or 2nd register
				   operand.*)
  }
;;

(***** Macros *****)

let coerce_macro (reg_coerce) = []
and coerceName_macro (nc) = []
and comment_macro (str) = []
and fallthru_macro (conlist)= []
and proof_macro _ = []
and nameobj_macro _ = []
and forgetunique_macro _ = []
and removename_macro _ = []
and sunpack_macro _ = []
and vcase_macro  = []
and letprod_macro  = []
and letroll_macro  = []

(* RLS 3/2/99: 
   Changed (Call gc_malloc_label) to mov then absolute call for Cyclone *)

and malloc_macro (id,size, maopt) = 
   [  (Push (Immed size, []));
      (Mov (Reg Eax, (Addr gc_malloc_label, []))); (* Changed for cyclone *)
      (Call (Reg Eax, []));
      (ArithBin (Add, Reg Esp,Immed i32_4));
      (Test (Reg Eax, Reg Eax));
      (Jcc (Eq, (out_of_memory_error_label, []), None))  ]

and unpack_macro (var, dstreg, varval_coerce) =
   [  (Mov (Reg dstreg, varval_coerce))  ]

and gettla_macro (reg, (offset,coerce)) = 
  [   (Mov (Reg reg, (Prjl((taltla_label,[]),offset,None),[])))
  ] 

and settla_macro (offset, (reg, coerce)) = 
  [   (Mov (Prjl((taltla_label,[]),offset,None),(Reg reg,[])))
  ] 
;;

(********************** INSTRUCTION LENGTHS (PREPARSE) **********************)

(***** Immediate value lengths *****)

(* scale_of_regpart
   Returns a scale specifying the number of bits contained in a regpart. 
   We do this so that RPl and RPh look the same. *)

let scale_of_regpart rp = 
   match rp with
      RPe -> Byte4
    | RPx -> Byte2
    | RPh -> Byte1
    | RPl -> Byte1
;;


(* immed_is_unsigned_byte
   Returns TRUE if a value is an unsigned byte, in 0..255. *)
let immed_is_unsigned_byte n =
   ((n >=$ i32_0) && (n <=$ i32_255))
;;

(* immed_is_signed_byte
   Returns TRUE if a value is a signed byte, in -128..127. *)
let n128 = int_to_int32 (-128);;
let p127 = int_to_int32 127;;
let immed_is_signed_byte n =
   ((n >=$ n128) && (n <= p127))
;;

(* immed_is_byte
   Returns TRUE if a value that can be signed or unsigned is a byte.
   -128..127 is a signed byte; 0..255 is an unsigned byte, so a signed
   or unsigned byte is in -128..255. *)

let immed_is_byte n =
   ((n >=$ n128) && (n <=$ i32_255))
;;

(* immed_is_word
   Returns TRUE if the value is not a byte. *)

let immed_is_word n = (not (immed_is_byte n));;

(* immed_len
   Returns the number of bytes in an immediate value that can be
   signed or unsigned. *)

let immed_len i =
   if (immed_is_byte i) then 1 else 4
;;

(* immed_len_0 i
   Same as immed_len i, but returns 0 if i is 0. *)

let immed_len_0 i =
   if (i =$ i32_0) then 0 else (immed_len i)
;;

(* signed_immed_len
   Returns the number of bytes in a signed immediate value. *)

let signed_immed_len i =
   if (immed_is_signed_byte i) then 1 else 4
;;

(* signed_immed_len_0 i
   Same as signed_immed_len i, but returns 0 if i is 0. *)

let signed_immed_len_0 i =
   if (i =$ i32_0) then 0 else (signed_immed_len i)
;;

let scale_index_len opt =
  match opt with
    None -> 0
  | Some _ -> 1
;;

(***** Instruction lengths *****)

(*** Individual instructions ***)

let length_of_mov_array = 7;;

(* length_of_operand : genop -> int
   Returns the length of any general operand.
   If the operand involves a relocation table entry, !numrelocs is incremented. *)

let length_of_operand operand =
   match operand with
      (Reg r) -> 0			(* Registers are already dealt with. *)
    | (Immed i) -> (signed_immed_len i)	(* Immediates are 8 or 32 bits. *)
    | (Addr a) -> 4			(* Address is max. 4 bytes. *)
    | (Prjl ((l, _), disp, opt)) ->	(* Prjl labels are 4 bytes. *)
	4 + scale_index_len opt
    | (Prjr ((Esp, _), disp, Some (_,Esp))) ->
	failwith "length_of_operand:  Esp cannot be index register in scaled-indexed addressing"
    | (Prjr ((Esp,_), disp, _)) ->	(* Prjr Esp requires a SIB. *)
	 1 + (signed_immed_len_0 disp)
    | (Prjr ((Ebp, _), disp, opt)) ->	(* Prjr Ebp can't use 0-byte disp. *)
	 (signed_immed_len disp + scale_index_len opt)
    | (Prjr ((r, _), disp, opt)) ->	(* Prjr's are 0, 8 or 32 bits. *)
	 (signed_immed_len_0 disp + scale_index_len opt)
;;

(* length_of_fpargs : FPargs -> int *)
let length_of_fpargs args =
  match args with
    FPstack _ | FPstack2 _ -> 0
  | FPgenop (s,g) -> length_of_operand g

(* length_of_macro : instruction list -> int
   Returns the summed length of all instructions in the macro. *)

let rec length_of_macro macro_instrs =
   match macro_instrs with
      [] -> 0
    | (instr :: tail) ->
	 (length_of_instr instr) + (length_of_macro tail)

(* length_of_instr : instruction -> int
   Returns the instruction's maximum length.

   Always remember to add 1 byte for the opcode and 1 byte
   for the mod;reg;r/m byte. *)

and length_of_instr instr =
   match instr with
   | ArithBin (Imul2, (Reg regop), ((Immed _) as immedtag)) ->
	 (* Here, give the same length as Imul3 regop, regop, immed *)
       2 + (length_of_operand (Reg regop)) + (length_of_operand immedtag)
	 
   | ArithBin (Imul2, (Reg dstreg), srcop) ->
	 (* Here, Imul2 needs a special extra byte. *)
       3 + (length_of_operand srcop)
	 
   | ArithBin (operation, op1, op2) ->
       2 + (length_of_operand op1) + (length_of_operand op2)
	 
   | ArithMD (operation, src) ->
       2 + (length_of_operand src)
   | ArithSR (operation, src, None) ->
       2 + (length_of_operand src)
   | ArithSR (operation, src, Some immed) ->
       2 + (length_of_operand src) + 1
	 
   | ArithUn (Dec, (Reg regop)) ->
       1
   | ArithUn (Inc, (Reg regop)) ->
       1
   | ArithUn (operation, op) ->
       2 + (length_of_operand op)
	 
   | Bswap (reg) ->
       2
	 
   | Call ((Addr a), _) ->
       1 + (length_of_operand (Addr a))
   | Call (op, _) ->
       2 + (length_of_operand op)
	 
   | Clc -> 1
   | Cmc -> 1
	 
   | Cmovcc (cond, regdst, (src, _)) ->
       3 + (length_of_operand src)
	 
   | Cmp ((op1,_), (op2,_)) ->
       2 + (length_of_operand op1) + (length_of_operand op2)
	 
   | Conv conv ->
       1
	 
   | Imul3 (dstreg, srcop, immedop) ->
       2 + (length_of_operand srcop) + (signed_immed_len immedop)
	 
   | Int (intnum) -> 2
   | Into -> 1
	 
   | Jcc (cond, (labl, _), _) ->
       2 + (length_of_operand (Addr labl))
	 
   | Jecxz (labl, _) -> 2
	 
   | Jmp ((Addr labl), _) ->
       1 + (length_of_operand (Addr labl))
   | Jmp (op, _) ->
       2 + (length_of_operand op)
	 
   | Lahf -> 1
	 
   | Lea (dstReg, memsrc) ->
       2 + (length_of_operand memsrc)
	 
   | Loopd ((labl, _), _) -> 2
	 
   | Mov (Reg dstreg, (Immed _, _)) ->
       1 + 4
   | Mov (dstOp, (Immed _, _)) ->
       2 + (length_of_operand dstOp) + 4
   | Mov (op1, (op2, _)) ->
       	2 + (length_of_operand op1) + (length_of_operand op2)
   | Movpart (zx, dstop, rp1, srcop, rp2) ->
	if (scale_of_regpart rp1) <> (scale_of_regpart rp2) then
	  (match dstop with
	    Reg _ ->
	      3 + (length_of_operand srcop) + (if rp1 = RPx then 1 else 0)
	  | _ -> invalid_arg "Movpart: destination must be a register if sizes are different")
	else
	  (match dstop,srcop with
	    Reg _,_ -> 
	      2 + (length_of_operand srcop) + (if rp1 = RPx then 1 else 0)
	  | _,Reg _ ->
	      2 + (length_of_operand dstop) + (if rp1 = RPx then 1 else 0)
	  | _,_ -> invalid_arg "Movpart: one of dst/src must be register")

   | Nop -> 1
	 
   | Pop (Reg reg) -> 1
   | Pop (op) ->
       2 + (length_of_operand op)
	 
   | Popad -> 1
   | Popfd -> 1
	 
   | Push ((Reg reg), _) -> 1
   | Push (Immed immed, _) ->
       1 + (signed_immed_len immed)
   | Push ((Addr addr), _) ->
       5
   | Push (op, _) ->
       2 + (length_of_operand op)
	 
   | Pushad -> 1
   | Pushfd -> 1
   | Rdtsc -> 2
   | Retn None -> 1
   | Retn (Some i) when i=$i32_0 -> 1
   | Retn (Some _) -> 3
   | Sahf -> 1
   | Setcc (cond, op) ->
       3 + (length_of_operand op)
	 
   | Shld (op, reg, None) ->
       3 + (length_of_operand op)
   | Shld (op, reg, (Some _)) ->
       3 + (length_of_operand op) + 1
   | Shrd (op, reg, None) ->
       3 + (length_of_operand op)
   | Shrd (op, reg, (Some _)) ->
       3 + (length_of_operand op) + 1
	 
   | Stc -> 1
	 
   | Test (op1, op2) ->
       2 + (length_of_operand op1) + (length_of_operand op2)
	 
   | Xchg (regmemop, regop) ->
       2 + (length_of_operand regmemop)
	 
	 
(***** Macros *****)
   | Coerce reg_coerce ->
       (length_of_macro (coerce_macro reg_coerce))
   | CoerceName nc ->
       (length_of_macro (coerceName_macro nc))
   | Comment str ->
       (length_of_macro (comment_macro str))
   | Fallthru conlist ->
       (length_of_macro (fallthru_macro conlist))
   | Malloc (id,size, maopt) -> 
       (length_of_macro (malloc_macro (id,size,maopt)))
   | Proof pf -> 
       (length_of_macro (proof_macro pf))
   | Unpack (var, dstreg, (Reg varreg, _))
       when (compare_regs dstreg varreg)=0 ->
       0
   | Unpack (var, dstreg, varval_coerce) ->
       (length_of_macro (unpack_macro (var, dstreg, varval_coerce)))
   | Sunpack (v,g) ->
       (length_of_macro (sunpack_macro (v,g)))
   | Nameobj (x,gop) -> 
       (length_of_macro (nameobj_macro (x,gop)))
   | ForgetUnique x ->
       (length_of_macro (forgetunique_macro x))
   | RemoveName x ->
       (length_of_macro (removename_macro x))

(* Floating Point *)
   | FPnoargs x ->
       (match x with Fwait -> 1 | Fclex -> 3 | Finit -> 3 |  _ -> 2)
   | FPsomeargs (x,args) ->
       (match x with Fstsw -> 1 | _ -> 0) + 2 + length_of_fpargs args

(* Cyclone *)
   | CgStart (arg1,arg2) ->
	(length_of_macro (cgstart_macro arg1 arg2))
   | CgDump (cg_reg,tv, reg, lab) ->
	(length_of_macro (cgdump_macro cg_reg tv reg lab))
   | CgHole (reg, labeloftmpl, labelofhole) ->
	(length_of_macro (cghole_macro reg labeloftmpl labelofhole))
   | CgHoleJmp (tmpllab, (holelab, crc)) ->
	(length_of_macro (cgholejmp_macro tmpllab holelab))
   | CgHoleJcc (cc, tmpllab, (holelab,crc), _) ->
	(length_of_macro (cgholejcc_macro cc tmpllab holelab))
   | CgFill (cg_reg,reg1, labeloftmpl, labelofhole, reg2) ->
       (length_of_macro (cgfill_macro cg_reg reg1 i32_0 i32_0 reg2)) + 4
	   (* We add 4 because (hole_pos-tmpl_pos) could be up to 4 bytes *)
   | CgFillJmp (cg_reg,holereg, htmplab, holelab, targreg, ttmplab, targlab) ->
	(length_of_macro (cgfilljmp_macro cg_reg holereg i32_0 i32_0
			       targreg i32_0 i32_0)) + 8
	   (* Label arith. used twice *)
   | CgFillJcc (cg_reg, holereg, htmplab, holelab, targreg, ttmplab, targlab) ->
	(length_of_macro (cgfilljcc_macro cg_reg holereg i32_0 i32_0
			       targreg i32_0 i32_0)) + 8
	   (* Label arith. used twice *)
   | CgForget (tv1,tv2) -> 0
   | CgEnd reg ->
	(length_of_macro (cgend_macro reg))
(* End Cyclone *)
(* LX *)
   | Vcase _ -> (length_of_macro (vcase_macro))
   | Letprod _  -> (length_of_macro (letprod_macro ))
   | Letroll _ -> (length_of_macro (letroll_macro ))
(* end LX *)

 (*  | _ -> (invalid_arg "length_of_instr: Unknown instruction or bad operands")
  *)
;;

(***** Data length *****)
let deSome s = 
   match s with 
      Some s -> s 
    | None -> failwith "BUG - talasmx86.ml - Drep not verified"

let length_of_data_item ditem =
   match ditem with
    | Dlabel (_, _) -> 4
    | Dbytes (str) -> (String.length str)
    | D2bytes (_) -> 2
    | D4bytes (_) -> 4
    | Drep (ri,str) -> (String.length (deSome(!str))) + 4 (* for size *)
    | Dfloat32 _ -> 4
    | Dfloat64 _ -> 8
    | Djunk -> 4
    | Dup -> 0
    | Ddown -> 0
;;

(********************* INSTRUCTION ENCODING (ASSEMBLY) **********************)

(***** Registers *****)

let encode_register = function
 | Eax -> 0x0
 | Ecx -> 0x1
 | Edx -> 0x2
 | Ebx -> 0x3
 | Esp -> 0x4
 | Ebp -> 0x5
 | Esi -> 0x6
 | Edi -> 0x7
 | _ -> (failwith "encode_register: invalid register code")
;;

let encode_condition cond =
   match cond with
      Above ->      0x7
    | AboveEq ->    0x3
    | Below ->      0x2
    | BelowEq ->    0x6
    | Eq ->         0x4
    | Greater ->    0xF
    | GreaterEq ->  0xD
    | Less ->       0xC
    | LessEq ->     0xE
    | NotEq ->      0x5
    | NotOverflow-> 0x1
    | NotSign ->    0x9
    | Overflow ->   0x0
    | ParityEven -> 0xA
    | ParityOdd ->  0xB
    | Sign ->       0x8
;;

let encode_arith_bin_opcode = function
   Adc -> 0x2
 | Add -> 0x0
 | And -> 0x4
 | Or ->  0x1
 | Sbb -> 0x3
 | Sub -> 0x5
 | Xor -> 0x6
 | _ -> failwith "encode_arith_bin_opcode: Imul2 was used."
;;

let encode_arith_md_subopcode = function
   Div -> 0x6
 | Idiv -> 0x7
 | Imul1 -> 0x5
 | Mul -> 0x4
;;

let encode_arith_sr_subopcode = function
   Rol -> 0x0
 | Ror -> 0x1
 | Rcl -> 0x2
 | Rcr -> 0x3
 | Shl -> 0x4
 | Sal -> 0x4
 | Shr -> 0x5
 | Sar -> 0x7
;;

let encode_arith_un_opcode = function
   Dec -> 0xFE
 | Inc -> 0xFE
 | Neg -> 0xF6
 | Not -> 0xF6
;;

let encode_arith_un_subopcode = function
   Dec -> 0x1
 | Inc -> 0x0
 | Neg -> 0x3
 | Not -> 0x2
;;

let encode_conv = function
   Cbw -> 0x98
 | Cdq -> 0x99
 | Cwd -> 0x99
 | Cwde -> 0x98
;;

let encode_scale = function
   Byte1 -> 0x0
 | Byte2 -> 0x1
 | Byte4 -> 0x2
 | Byte8 -> 0x3
;;

let encode_eltsize es =
  if es =$ i32_1 then 0x0
  else if es =$ i32_2 then 0x1
  else if es =$ i32_4 then 0x2
  else if es =$ i32_8 then 0x3
  else invalid_arg "Talasmx86.encode_eltsize"
;;

(***** Flags and such *****)

let sign_extend_flag = function
   true -> 0x2
 | false -> 0x0
;;

let word_flag = function
   true -> 0x1
 | false -> 0x0
;;

(* Use the sign-extend bit to indicate 8-bit immediate values. *)
let sign_extend_immed n =
   (sign_extend_flag (immed_is_signed_byte n))
;;

(***** Writing *****)

let write_char env c = Stringchan.put_char env.a_schan c;;

let write_byte env n =
  Stringchan.put_char env.a_schan (int32_to_1byte n)
;;

let write_byte' env n =
  Stringchan.put_char env.a_schan(Char.chr (n land 255))
;;

let write_2bytes env n =
  let (b1,b2) = int32_to_2bytes n in
  Stringchan.put_char env.a_schan b1;
  Stringchan.put_char env.a_schan b2
;;

let write_4bytes env n =
  let (b1,b2,b3,b4) = int32_to_4bytes n in
  Stringchan.put_char env.a_schan b1;
  Stringchan.put_char env.a_schan b2;
  Stringchan.put_char env.a_schan b3;
  Stringchan.put_char env.a_schan b4
;;

let write_f32 env f32 =
  let bytes = f32_to_bytes f32 in
  if String.length bytes != 4 then (invalid_arg "write_f32");
  for i = 0 to 3 do
    write_byte' env (int_of_char bytes.[i]);
  done
;;

let write_f64 env f64 =
  let bytes = f64_to_bytes f64 in
  if String.length bytes != 8 then (invalid_arg "write_f64");
  for i = 0 to 7 do
    write_byte' env (int_of_char bytes.[i]);
  done

(* write_immed env immed
   Writes an immediate value as an 8-bit or 32-bit value. *)

let write_immed env immed =
   if (immed_is_signed_byte immed) then
      (write_byte env immed)
   else
      (write_4bytes env immed)
;;

(* write_immed_0 env immed
   Same, but doesn't write if immed is 0. *)

let write_immed_0 env immed =
   if (immed <>$ i32_0) then
      (write_immed env immed)
;;

(***** Labels & Relocations *****)

(* add_reloc
   Side effects: Inserts a new relocation entry into the relocation table. *)
let add_reloc env rel scale ident addend public =
   try
      let reloc = (make_reloc (Stringchan.get_mark env.a_schan) rel scale
		 	(lookup_symbol (!(env.a_r_symdict)) ident) 
			addend public) in
      (env.a_r_reloclist := reloc :: !(env.a_r_reloclist))
   with
      Dict.Absent ->
	 (invalid_arg ("label '" ^ (id_to_string ident) ^ "' absent"))
;;

(* write_label_addend
   Side effects: Writes a placeholder and creates a relocation in r_reloclist
     to label labl, with relativity rel and the given addend.*)
let write_label_addend env rel labl addend =
   (add_reloc env rel Byte4 labl addend false);
   (write_4bytes env i32_0)
;;

(* write_label
   Side effects: Writes a label with a 0 addend and creates a relocation. *)
let write_label env rel labl =
   (write_label_addend env rel labl i32_0)
;;


(* write_8bit_label
   Side effects: Writes an 8-bit label with a 0 addend and creates an
   internal relocation. *)
let write_8bit_label env labl =
   (add_reloc env Relative Byte1 labl i32_0 false);
   (write_byte env i32_0)
;;

(* write_public_label_addend
   Side effects: writes a label and forces it to be public. *)
let write_public_label_addend env rel labl addend =
   (add_reloc env rel Byte4 labl addend true);
   (write_4bytes env i32_0)
;;

(* label_is_close
   Returns TRUE if the label can be jumped to with an 8-bit jump; i.e.,
   if the label is in the current section and within 127 bytes of the current
   location. *)
let label_is_close env labl =
   try
      let current_pos = 2 + (Stringchan.get_mark env.a_schan) in
      let sym = (lookup_symbol (!(env.a_r_symdict)) labl) in
      if (sym.sym_section = env.a_secnum) then
	 (if (sym.sym_offset <= current_pos) then
	    ((current_pos - sym.sym_offset) < 128)
	 else
	    (((sym.sym_offset - env.a_pos_offset) - current_pos) <= 130) )
      else false
   with
      Dict.Absent -> false
;;


(* Cyclone *)

(* is_template_symbol
   Should return true only when the symbol given is in a template. 
   For now, returns true for everything. *)

let cyc_is_template_symbol sym = 
   (* XXX *)(* Unimplemented *)
   true
;;


(* cyc_label_position
   Given a label (identifier), returns its position as an int32. 
   The label must be in a template section, else Invalid_arg is raised. *) 

let rec cyc_label_position env l = 
   let sym = (lookup_symbol (!(env.a_r_symdict)) l) in
   if (cyc_is_template_symbol sym) then 
      (int_to_int32 sym.sym_offset) (* Should probably look at value? *)
   else
      invalid_arg 
	 "cyc_label_position: label is not in a template section!"
;;

(* update_symbol_pos copied from Talasm.ml -- keep in sync. *)
let update_symbol_pos symbol newpos =
   if symbol.sym_section = 0 then
      ((failwith "update_symbol_pos: symbol is external.");
	 0)
   else
      let pos_offset = symbol.sym_offset - newpos in
      symbol.sym_offset <- newpos;
      pos_offset
;;

(* cyc_insert_hole_symbol
   Inserts the symbol for a hole label into the symbol dictionary. *)

let cyc_insert_hole_symbol env hole_label = 
  let realpos = Stringchan.get_mark env.a_schan in
  let hole_sym = lookup_symbol !(env.a_r_symdict) hole_label in
  (update_symbol_pos hole_sym realpos);
  env.a_hole_syms <- (hole_sym :: env.a_hole_syms);
  ()
;;

(* End Cyclone *)


(***** Operand writing *****)

(* sort_operands (operand1, operand2)
   Returns an operand_record specifying which direction the operation is in,
   and which are the memory and register operands. *)

let reg_to_reg = 0x2
and reg_to_mem = 0x0
and mem_to_reg = 0x2
;;

let sort_operands = function
   (Reg regOp1, Reg regOp2) ->
      {op_direction=reg_to_reg; op_regOp=regOp1; op_regMemOp=(Reg regOp2)}
 | (Reg regOp1, memOp2) ->
      {op_direction = mem_to_reg; op_regOp = regOp1; op_regMemOp = memOp2}
 | (memOp1, Reg regOp2) ->
      {op_direction = reg_to_mem; op_regOp = regOp2; op_regMemOp = memOp1}
 | (memOp1, memOp2) -> failwith "sort_operands: two memory operands."
;;

(* modregrm mod reg rm
   Encodes a mod;reg;r/m byte given the three arguments. *)

let modregrm modv reg rm =
   ((modv lsl 6) lor (reg lsl 3) lor rm)
;;

(* sib scale ind base
   Encodes a sib byte. Its format is the same as the mod;reg;r/m byte. *)

let sib = modregrm;;

(* write_regmem_operand env regOperand regmemop
   Combines the previously determined regOperand with the mod;;r/m values
   for the register/memory operand, then writes the mod;reg;r/m field
   and the operand's displacement, if relevant.

   If any labels are used in this instruction, an entry is made for them
   in the relocation table. *)

let write_regmem_operand env regOperand regmemop =
   match regmemop with
					(* Second register operand *)
      Reg regOp2 ->
	 (write_byte' env (modregrm  0x3 regOperand (encode_register regOp2)))

					(* Indirect memory operand *)
    | Prjr ((baseReg, _), disp, None) ->
	 let modValue =
	    if (disp =$ i32_0  &&  baseReg <> Ebp) then 0x0
	    else if (immed_is_signed_byte disp) then 0x1
	    else 0x2 in
	 (write_byte' env (modregrm  modValue regOperand
			       (encode_register baseReg)));
	 if baseReg = Esp then		(* Need SIB for writing Esp as base *)
	   (write_byte' env (sib  0x0 (encode_register Esp)
			      (encode_register Esp)));
	 if baseReg = Ebp then
	    (write_immed env disp)	(* 0 offset with Ebp: extra byte *)
	 else
	    (write_immed_0 env disp)
                                         (* [baseReg + scale*index + disp] *)
  | Prjr ((baseReg, _), disp, Some (scale, index)) ->
      let modValue =
	if disp =$ i32_0 && baseReg <> Ebp then 0x0
	else if (immed_is_signed_byte disp) then 0x1
	else 0x2 in
      let rmValue = 0x4 in
      write_byte' env (modregrm modValue regOperand rmValue);
      (* Write SIB byte *)
      if index = Esp then 
	failwith "write_regmem_operand: index cannot be Esp in Prjr scaled indexed";
      write_byte' env (sib 
			 (encode_scale scale) 
			 (encode_register index) 
			 (encode_register baseReg));
      (* Write displacement *)
      if baseReg = Ebp then
      	write_immed env disp  (* 0 offset with Ebp: extra byte *)
      else
	write_immed_0 env disp

					(* Direct memory operand + offset *)
    | Prjl ((labl, _), disp, None) ->
	 (write_byte' env (modregrm  0x0 regOperand 0x5));
	 (write_label_addend env Absolute labl disp);

                                        (* mem + scale*index + displacement *)
  | Prjl ((labl, _), disp, Some (scale, index)) ->
      if index = Esp then
	failwith "write_regmem_operand: index cannot be Esp in scaled indexed";
      write_byte' env (modregrm  0x0 regOperand 0x4);
      write_byte' env (sib (encode_scale scale) (encode_register index) 0x5);
      write_label_addend env Absolute labl disp;

  | Addr labl ->
      invalid_arg "write_regmem_operand: used Addr."
  | Immed int ->
      invalid_arg "write_regmem_operand: used Immed."
;;

(* write_operands env (operands: operand_record)
   Writes the register operand and the regmem operand
   from the data structure. *)

let write_operands env operands =
   (write_regmem_operand env (encode_register operands.op_regOp)
	 operands.op_regMemOp)
;;


(***** Floating Point Instructions and Operands *****)

let fp_error s = invalid_arg ("fp assembler error: "^s)

(* specify stack element *)
let fp_sti i = 
  match i with
    0 -> 0x0
  | 1 -> 0x1
  | 2 -> 0x2
  | 3 -> 0x3
  | 4 -> 0x4
  | 5 -> 0x5
  | 6 -> 0x6
  | 7 -> 0x7
  | _ -> invalid_arg "fp_sti: bad stack offset"

(* write fwait instruction *)
let fwait env () = write_byte' env 0x9B

(* instructions with no arguments
 * Instruction encodings from Intel Arch. Software Developer's Manual, 
 * volume 2, section 3 
 *)
let write_fpnoargs env x =
  let w byte1 byte2 = write_byte' env byte1; write_byte' env byte2 in
  match x with
    F2xm1   -> w 0xD9 0xF0
  | Fabs    -> w 0xD9 0xE1
  | Fchs    -> w 0xD9 0xE0
  | Fclex   -> fwait env (); w 0xDB 0xE2
  | Fnclex  -> w 0xDB 0xE2
  | Fcompp  -> w 0xDE 0xD9
  | Fucompp -> w 0xDA 0xE9
  | Fcos    -> w 0xD9 0xFF
  | Fdecstp -> w 0xD9 0xF6
  | Fincstp -> w 0xD9 0xF7
  | Finit   -> fwait env (); w 0xDB 0xE3
  | Fninit  -> w 0xDB 0xE3
  | Fld1    -> w 0xD9 0xE8
  | Fldz    -> w 0xD9 0xEE
  | Fldpi   -> w 0xD9 0xEB
  | Fldl2e  -> w 0xD9 0xEA
  | Fldl2t  -> w 0xD9 0xE9
  | Fldlg2  -> w 0xD9 0xEC
  | Fldln2  -> w 0xD9 0xED
  | Fnop    -> w 0xD9 0xD0
  | Fpatan  -> w 0xD9 0xF3
  | Fprem   -> w 0xD9 0xF8
  | Fprem1  -> w 0xD9 0xF5
  | Fptan   -> w 0xD9 0xF2
  | Frndint -> w 0xD9 0xFC 
  | Fscale  -> w 0xD9 0xFD
  | Fsin    -> w 0xD9 0xFE
  | Fsincos -> w 0xD9 0xFB
  | Fsqrt   -> w 0xD9 0xFA
  | Ftst    -> w 0xD9 0xE4
  | Fwait   -> fwait env ()
  | Fxam    -> w 0xD9 0xE5
  | Fxtract -> w 0xD9 0xF4
  | Fyl2x   -> w 0xD9 0xF1
  | Fyl2xp1 -> w 0xD9 0xF9

(* write: byte1 mod/threebits/rm [sib] [disp] 
 * where sib and disp are optional
 *)
let write_fpmodrm env byte1 threebits g =
  write_byte' env byte1;
  match g with
    Reg _ | Addr _ | Immed _ -> fp_error "write_fpmodrm: not memory operand"
  | Prjr _ | Prjl _ -> write_regmem_operand env threebits g
  
(* write: byte1 byte2+i *)
(* Abbreviation from Intel Arch. Software Developer's Manual, Vol 2, pg. 3-2 *)
let write_fpsti env byte1 byte2 i =
  write_byte' env byte1;
  write_byte' env (byte2 + i)

(* write integer-memory operation *)
let write_fpintmem env args opcode opname =
  match args with
    FPgenop (s,g) -> 
      if s = Byte4 then
	write_fpmodrm env 0xDA opcode g
      else if s = Byte2 then
	write_fpmodrm env 0xDE opcode g
  | _ -> fp_error (opname^": bad scale")

(* write real-memory operation *)
let write_fprealmem env s g opcode opname =
  if s = Byte4 then 
    write_fpmodrm env 0xD8 opcode g
  else if s = Byte8 then
    write_fpmodrm env 0xDC opcode g
  else
    fp_error (opname^": bad scale")

(* write a binary operation that pops its argument *)
let write_fppop env args byte opname =
  match args with
    FPstack2 (b,i) -> 
      if b then fp_error (opname^": 2nd arg must be ST")
      else write_fpsti env 0xDE byte i
  | _ -> fp_error (opname^": bad args")
  
(* Instruction encodings from Intel Arch. Software Developer's Manual, 
 * volume 2, section 3 *)
let write_fpsomeargs env op args =
  let w b = write_byte' env b in
  match op with
    Fadd -> 
      (match args with
	FPstack _ -> fp_error "fadd: bad args"
      |	FPstack2 (b,i) ->
	  if b then write_fpsti env 0xD8 0xC0 i (* ST <- ST + ST(i)    *)
	  else write_fpsti env 0xDC 0xC0 i      (* ST(i) <- ST(i) + ST *)
      |	FPgenop (s,g) -> write_fprealmem env s g 0x0 "fadd")
  | Fcom -> 
      (match args with
	FPstack i -> fp_error "fcom: binary operator"
      |	FPstack2 (b,i) -> write_fpsti env 0xD8 0xD0 i
      |	FPgenop (s,g) -> write_fprealmem env s g 0x2 "fcom")
  | Fdiv ->
      (match args with
	FPstack _ -> fp_error "fadd: bad args"
      |	FPstack2 (b,i) ->
	  if b then write_fpsti env 0xD8 0xF0 i
	  else write_fpsti env 0xDC 0xF8 i
      |	FPgenop (s,g) -> write_fprealmem env s g 0x6 "fdiv")
  | Fdivr ->
      (match args with
	FPstack _ -> fp_error "fdivr: bad args"
      |	FPstack2 (b,i) ->
	  if b then write_fpsti env 0xD8 0xF8 i
	  else write_fpsti env 0xDC 0xF0 i
      |	FPgenop (s,g) -> write_fprealmem env s g 0x7 "fdivr")
  | Fmul ->
      (match args with
	FPstack _ -> fp_error "fmul: bad args"
      |	FPstack2 (b,i) ->
	  if b then write_fpsti env 0xD8 0xC8 i
	  else write_fpsti env 0xDC 0xC8 i
      |	FPgenop (s,g) -> write_fprealmem env s g 0x1 "fdivr")
  | Fsub  ->
      (match args with
	FPstack _ -> fp_error "fsub: bad args"
      |	FPstack2 (b,i) ->
	  if b then write_fpsti env 0xD8 0xE0 i
	  else write_fpsti env 0xDC 0xE8 i
      |	FPgenop (s,g) -> write_fprealmem env s g 0x4 "fdivr")
  | Fsubr ->
      (match args with
	FPstack _ -> fp_error "fsubr: bad args"
      |	FPstack2 (b,i) ->
	  if b then write_fpsti env 0xD8 0xE8 i
	  else write_fpsti env 0xDC 0xE0 i
      |	FPgenop (s,g) -> write_fprealmem env s g 0x5 "fdivr")
  | Fucom ->
      (match args with
	FPstack  i -> write_fpsti env 0xDD 0xE0 i
      |	FPstack2 _ -> fp_error "fucom: not a binary operator"
      |	FPgenop  _ -> fp_error "fucom: no memory operator")
  | Fxch ->
      (match args with
	FPstack  i -> write_fpsti env 0xD9 0xC8 i
      |	FPstack2 _ -> fp_error "fxch: not a binary operator"
      |	FPgenop  _ -> fp_error "fxch: no memory operator")	

  | Fiadd  -> write_fpintmem env args 0x0 "fiadd"
  | Ficom  -> write_fpintmem env args 0x2 "ficom"
  | Ficomp -> write_fpintmem env args 0x3 "ficomp"
  | Fidiv  -> write_fpintmem env args 0x6 "fdiv"
  | Fidivr -> write_fpintmem env args 0x7 "fidivr"
  | Fimul  -> write_fpintmem env args 0x1 "fimul"
  | Fisub  -> write_fpintmem env args 0x4 "fisub"
  | Fisubr -> write_fpintmem env args 0x5 "fisubr"

  | Faddp  -> write_fppop env args 0xC0 "faddp"
  | Fcomp  ->
      (match args with
	FPstack _ -> fp_error "fcomp: bad args"
      |	FPstack2 (b,i) -> write_fpsti env 0xD8 0xD8 i
      |	FPgenop (s,g) -> write_fprealmem env s g 0x3 "fcomp")
  | Fdivp  -> write_fppop env args 0xF8 "fdivp"
  | Fdivrp -> write_fppop env args 0xF0 "fdivrp"
  | Fmulp  -> write_fppop env args 0xC8 "fmulp"
  | Fsubp  -> write_fppop env args 0xE8 "fsubp" 
  | Fsubrp -> write_fppop env args 0xE0 "fsubrp"
  | Fucomp ->
      (match args with
	FPstack  i -> write_fpsti env 0xDD 0xE8 i
      |	FPstack2 _ -> fp_error "fucomp: not a binary operator"
      |	FPgenop  _ -> fp_error "fucomp: no memory operands")
  | Fst -> 
      (match args with
	FPstack i -> write_fpsti env 0xDD 0xD0 i
      |	FPstack2 _ -> fp_error "fst: one operand only"
      |	FPgenop (s,g) -> 
	  if s = Byte4 then
	    write_fpmodrm env 0xD9 0x2 g
	  else if s = Byte8 then
	    write_fpmodrm env 0xDD 0x2 g
	  else
	    fp_error "fst: bad scale")
  | Fstp ->
      (match args with
	FPstack i -> write_fpsti env 0xDD 0xD8 i
      |	FPstack2 _ -> fp_error "fstp: one operand only"
      |	FPgenop (s,g) -> 
	  if s = Byte4 then
	    write_fpmodrm env 0xD9 0x3 g
	  else if s = Byte8 then
	    write_fpmodrm env 0xDD 0x3 g
	  else
	    fp_error "fstp: bad scale")
  | Fist ->
      (match args with
      	FPgenop (s,g) -> 
	  if s = Byte4 then
	    write_fpmodrm env 0xDB 0x2 g
	  else if s = Byte2 then
	    write_fpmodrm env 0xDF 0x2 g
	  else 
	    fp_error "fist: bad scale"
      |	_ -> fp_error "fist: bad args")
  | Fistp ->
      (match args with
      	FPgenop (s,g) -> 
	  if s = Byte4 then
	    write_fpmodrm env 0xDB 0x3 g
	  else if s = Byte2 then
	    write_fpmodrm env 0xDF 0x3 g
	  else
	    fp_error "fistp: bad scale"
      |	_ -> fp_error "fistp: bad args")
  | Fld -> 
      (match args with
	FPstack i -> write_fpsti env 0xD9 0xC0 i
      |	FPstack2 _ -> fp_error "fld: bad args"
      |	FPgenop (s,g) ->
	  if s = Byte4 then
	    write_fpmodrm env 0xD9 0x0 g
	  else if s = Byte8 then
	    write_fpmodrm env 0xDD 0x0 g
	  else
	    fp_error "fld: bad scale")
  | Fild ->
      (match args with
	FPgenop (s,g) -> 
	  if s = Byte4 then
	    write_fpmodrm env 0xDB 0x0 g
	  else if s = Byte8 then
	    write_fpmodrm env 0xDF 0x0 g
	  else
	    fp_error "fild: bad scale"
      |	_ -> fp_error "fild: memory args only")
  | Ffree ->
      (match args with
	FPstack i -> write_fpsti env 0xDD 0xC0 i
      |	_ -> fp_error "ffree: bad args")
  | Fcomi | Fcomip | Fucomi | Fucomip ->
      (match args with
	FPstack2 (b,i) ->
	  let byte1,byte2 = 
	    match op with 
	      Fcomi   -> 0xDB, 0xF0
	    | Fcomip  -> 0xDF, 0xF0
	    | Fucomi  -> 0xDB, 0xE8
	    | Fucomip -> 0xDF, 0xE8 
	    | _ -> failwith "impossible" in
	  write_fpsti env byte1 byte2 i
      |	_ -> fp_error "fp compare and set cc: bad arg")
  | Fstsw | Fnstsw ->
      if op = Fstsw then fwait env ();
      (match args with
	FPgenop (s,Reg Eax) -> write_byte' env 0xDF; write_byte' env 0xE0
      |	FPgenop (s,mem) -> write_fpmodrm env 0xDD 0x7 mem
      |	_ -> fp_error "fstsw/fnstsw: bad arg")

(***** Individual instructions *****)

let n1 = int_to_int32 (-1);;

let write_shift_double_instr env opcode operand reg shiftamt =
   begin
      (write_byte' env 0x0F);
      (write_byte' env opcode);
      (write_regmem_operand env (encode_register reg) operand);
      if (shiftamt <>$ n1) then
	 (write_byte env shiftamt);
   end
;;


(* write_mov_part_instr
   Writes a mov_part instruction. 
   Dave: One of dstop, srcop must be a register.  If src and dst are
   not the same size then dst must be a reg. *)

let write_mov_part_instr env zx dstop rp1 srcop rp2 =
   let translate_regpart reg rp =  (* AH, BH, CH, DH are encoded specially. *)
      if rp <> RPh then reg
      else begin match reg with
	 Eax -> Esp		(* AH *)
       | Ecx -> Ebp		(* CH *)
       | Ebx -> Edi		(* BH *)
       | Edx -> Esi		(* DH *)
       | _ -> invalid_arg "write_mov_part_instr: Not an 8-bit register"
      end in
   let translate_regpart_of_genop op rp = 
      match op with
	 Reg r -> Reg (translate_regpart r rp)
       | x -> x
   in 
   let dst_is_reg,regOp,memOp =
     match dstop,srcop with
       Reg dstreg,src -> true,dstreg,src
     | dst,Reg srcreg -> false,srcreg,dst
     | _,_ -> invalid_arg "write_mov_part_instr: either src or dst must be reg"
   in
   let is_word = (not (rp2 = RPh or rp2 = RPl)) in (* 8-bit src? No word bit *)
   if rp1 = RPx then 
      (write_byte' env 0x66);		(* 16-bit dest? Use opsize prefix *)
   if (scale_of_regpart rp1) <> (scale_of_regpart rp2) then 
      begin				(* Diff. sizes? Write Movsx | Movzx *)
	 if not dst_is_reg then
	   invalid_arg "write_mov_part_instr: if src is reg then src and dst must be the same size";
	 let sx_flag = (if zx then 0x00 else 0x08) in
	 (write_byte' env 0x0F);		
	 (write_byte' env ((0xB6 lor sx_flag) lor (word_flag is_word)));
      end 
   else					(* Same size? Write a Mov *)
     begin
       let operand_direction =
	 if dst_is_reg then mem_to_reg
	 else reg_to_mem in
       (write_byte' env (0x88 lor (operand_direction lor (word_flag is_word))))
     end;
   (write_regmem_operand env
      (encode_register (translate_regpart regOp rp1))
      (translate_regpart_of_genop memOp rp2))
;;
 


(* write_mov_array
   Writes a mov instruction to or from an array based in a register.
   Offset in array is [arrayreg + scale*index + offset]*)

let write_mov_array env direction eltsize srcdstreg arrayreg indexreg offset =
  if eltsize =$ i32_2 then
    failwith "Talasmx86.write_mov_array - needs 16 bit operand prefix"
  else if eltsize <>$ i32_1 & eltsize <>$ i32_4 then
    invalid_arg "Talasmx86.write_mov_array - element size not 1,2,4";
  let use_word = not (eltsize =$ i32_1) in
  write_byte' env ((0x88 lor direction) lor (word_flag use_word));
  write_byte' env (modregrm 0x2 (encode_register srcdstreg) 0x4);
  write_byte' env (sib (encode_eltsize eltsize) (encode_register indexreg)
		     (encode_register arrayreg));
  write_4bytes env offset
;;

let decode_array_op genop =
  match genop with
    Prjr ((r,_),offset,None) -> (r,offset)
  | Prjr ((r,_),offset,Some _) -> 
      failwith "Talasmx86.decode_array_op - variable array op not allowed in macro"
  | Prjl ((_,_),_,_) ->
      failwith "Talasmx86.decode_array_op - array ops on labels unimplemented"
  | _ -> invalid_arg "Talasmx86.decode_array_op"
;;

(* write_macro
   Writes a list of instructions as a macro. *)

let rec write_macro env instrs =
   (List.iter (write_instr env) instrs)

(***** Write_instr *****)

(* write_instr env instr
   Writes the entire instruction to the buffer. *)

and write_instr env instr =
   match instr with

      ArithBin (Imul2, Reg regop, Immed immed) ->
	 (* Imul2 reg, immed is the same as Imul3 reg, reg, immed *)
	 (write_byte' env (0x69 lor (sign_extend_immed immed)));
	 (write_regmem_operand env (encode_register regop) (Reg regop));
	 (write_immed env immed);

    | ArithBin (Imul2, (Reg dstreg), srcop) ->
	 (write_byte' env 0x0F);
	 (write_byte' env 0xAF);
	 (write_regmem_operand env (encode_register dstreg) srcop);

    | ArithBin (operation, op1, Immed immed) ->
	 (write_byte' env (0x80 lor (sign_extend_immed immed)
			       lor (word_flag true)));
	 (write_regmem_operand env (encode_arith_bin_opcode operation) op1);
	 (write_immed env immed);

    | ArithBin (operation, op1, op2) ->
	 let operands = (sort_operands (op1, op2)) in
	 (write_byte' env (((encode_arith_bin_opcode operation) lsl 3)
       		          lor (operands.op_direction
                          lor  (word_flag true))));
	 (write_operands env operands);


    | ArithMD (operation, src) ->
	 (* One-operand multiply / divide.
	    Multiply / divide EAX by src, and put result or quotient/remainder
            in EAX/EDX. *)
	 (write_byte' env (0xF6 lor (word_flag true)));
	 (write_regmem_operand env (encode_arith_md_subopcode operation) src);

    | ArithSR (operation, src, None) ->
	 (* Shift/Rotate using ECX as shift amount *)
	 (write_byte' env (0xD2 lor (word_flag true)));
	 (write_regmem_operand env (encode_arith_sr_subopcode operation) src);

    | ArithSR (operation, src, (Some immed)) ->
	 (* Shift/Rotate using 8-bit immed as shift amount *)
	 (write_byte' env (0xC0 lor (word_flag true)));
	 (write_regmem_operand env (encode_arith_sr_subopcode operation) src);
	 (write_immed env (land32 immed i32_255));

    | ArithUn (Dec, (Reg regop)) ->
	 (write_byte' env (0x48 lor (encode_register regop)));
    | ArithUn (Inc, (Reg regop)) ->
	 (write_byte' env (0x40 lor (encode_register regop)));
    | ArithUn (operation, op) ->
	 (write_byte' env ((encode_arith_un_opcode operation)
			       lor (word_flag true)));
	 (write_regmem_operand env (encode_arith_un_subopcode operation) op);

(* Bswap reg *)
    | Bswap (reg) ->
	 (write_byte' env 0x0F);
	 (write_byte' env (0xC8 lor (encode_register reg)));

(** Call **)
(* We only use near calls, in the flat memory model. *)

(* Call addr *)
    | Call (Addr labl, _) ->
	 (write_byte' env 0xE8);
	 (write_label env Relative labl);

(* Call reg/mem *)
    | Call (op, _) ->
	 (write_byte' env 0xFF);
	 (write_regmem_operand env 0x2 op);

    | Clc ->
	 (write_byte' env 0xF8);

    | Cmc ->
	 (write_byte' env 0xF5);

    | Cmovcc (cond, regdst, (src, _)) ->
	 (* Conditional move regdst<-memsrc or regdst<-regsrc *)
	 (write_byte' env 0x0F);
	 (write_byte' env (0x40 lor (encode_condition cond)));
	 (write_regmem_operand env (encode_register regdst) src);

    | Cmp ((op1,_), (Immed immed,_)) ->
	 (write_byte' env (0x80 lor ((sign_extend_immed immed)
                                          lor (word_flag true))));
	 (write_regmem_operand env 0x7 op1);
	 (write_immed env immed);

    | Cmp ((op1,_), (Addr addr,_)) ->
         (write_byte' env (0x80 lor ((sign_extend_flag false) 
                                         lor (word_flag true))));
         (write_regmem_operand env 0x7 op1);
         (write_public_label_addend env Absolute addr i32_0);

    | Cmp ((op1,_), (op2,_)) ->
	 let operands = (sort_operands (op1, op2)) in
	 (write_byte' env ((0x38 lor (operands.op_direction
       			        lor (word_flag true)))));
	 (write_operands env operands);

    | Conv (conv) ->
	 (write_byte' env (encode_conv conv));

    | Imul3 (dstreg, srcop, immedop) ->
	 (* dstreg <- srcop * immedop *)
	 (write_byte' env (0x69 lor (sign_extend_immed immedop)));
	 (write_regmem_operand env (encode_register dstreg) srcop);
	 (write_immed env immedop);

    | Int (intnum) ->
	 (write_byte' env 0xCD);
	 (write_char env (int8_to_byte intnum));

    | Into ->
	 (write_byte' env 0xCE);

    | Jcc (cond, (labl, _), _) when (label_is_close env labl) ->
	 (write_byte' env (0x70 lor (encode_condition cond)));
	 (write_8bit_label env labl);

    | Jcc (cond, (labl, _), _) ->
	 (write_byte' env 0x0F);
	 (write_byte' env (0x80 lor (encode_condition cond)));
	 (write_label env Relative labl);

    | Jecxz ((labl, _),_) when (label_is_close env labl) ->
	 (write_byte' env 0xE3);
	 (write_8bit_label env labl);
    | Jecxz ((labl, _),_) ->
	 invalid_arg "jecxz requires a close label (8-bit offset).";

(* Jmp addr -- See notes on Call. *)
    | Jmp (Addr labl, _) when (label_is_close env labl) ->
	 (write_byte' env 0xEB);
	 (write_8bit_label env labl);

    | Jmp (Addr labl, _) ->
	 (write_byte' env 0xE9);
	 (write_label env Relative labl);

(* Jmp reg/mem *)
    | Jmp (op, _) ->
	 (write_byte' env 0xFF);
	 (write_regmem_operand env 0x4 op);


    | Lahf ->
	 (write_byte' env 0x9F);

    | Lea (dstReg, memsrc) ->
	 (write_byte' env 0x8D);
	 (write_regmem_operand env (encode_register dstReg) memsrc);
	 (* Note: does not check error cases (memsrc must be a mem operand) *)

				(* ECX--; loop if ECX != 0 *)
    | Loopd ((labl, _), None) when (label_is_close env labl) ->
	 (write_byte' env 0xE2);
	 (write_8bit_label env labl);
				(* ECX--; loop if ECX != 0 && Zflag == flag *)
    | Loopd ((labl, _), Some flag) when (label_is_close env labl) ->
	 (write_byte' env (0xE0 lor (if flag then 0x01 else 0x0)));
	 (write_8bit_label env labl);

    | Loopd (_,_) ->
	invalid_arg "loopd"


(* Mov reg, immed -- 4-byte simm *)
    | Mov ((Reg dstReg), (Immed immed, _)) ->
	 (write_byte' env (0xB0 lor
			    (((word_flag true) lsl 3) lor
			       (encode_register dstReg))));
	 (write_4bytes env immed)

(* Mov mem, immed|tag -- 4-byte simm *)
    | Mov (dst, (Immed immed, _)) ->
	 (write_byte' env (0xC6 lor (word_flag true)));
	 (write_regmem_operand env 0x0 dst);
	 (write_4bytes env immed);

(* Mov reg, immed_addr *)
    | Mov ((Reg dstReg), ((Addr addr), _)) ->
	 (write_byte' env (0xB0 lor
			    (((word_flag true) lsl 3) lor
			       (encode_register dstReg))));
	 (write_public_label_addend env Absolute addr i32_0);

(* Mov mem, immed_addr *)
    | Mov (dst, ((Addr addr), _)) ->
	 (write_byte' env (0xC6 lor (word_flag true)));
	 (write_regmem_operand env 0x0 dst);
	 (write_public_label_addend env Absolute addr i32_0);

(* Mov reg/mem, reg/mem *)
    | Mov (dst, (src, _)) ->
	 let operands = (sort_operands (dst, src)) in
	 (write_byte' env (0x88 lor (operands.op_direction
			       lor (word_flag true))));
	 (write_operands env operands);

    | Movpart (zx, dstop, rp1, srcop, rp2) ->
	 (write_mov_part_instr env zx dstop rp1 srcop rp2);
    | Nop ->
	 (write_byte' env 0x90);

    | Pop (Reg reg) ->
	 (write_byte' env (0x58 lor (encode_register reg)));
    | Pop (op) ->
	 (write_byte' env 0x8F);
	 (write_regmem_operand env 0x0 op);

    | Popad ->
	 (write_byte' env 0x61);

    | Popfd ->
	 (write_byte' env 0x9D);

    | Push ((Reg regOp), _) ->
	 (write_byte' env (0x50 lor (encode_register regOp)));
    | Push (Immed immed, _) ->
	 (write_byte' env ((0x68
		  lor (sign_extend_immed immed))));
	 (write_immed env immed)
    | Push ((Addr addr), _) ->
	 (write_byte' env 0x68);
	 (write_public_label_addend env Absolute addr i32_0)

    | Push (op, _) ->
	 (write_byte' env 0xFF);
	 (write_regmem_operand env 0x6 op);

    | Pushad ->
	 (write_byte' env 0x60);

    | Pushfd ->
	 (write_byte' env 0x9C);
    | Rdtsc -> (write_byte' env 0x0F; write_byte' env 0x31)
    | Retn None ->
	 (write_byte' env 0xC3);
    | Retn (Some i) when i =$ i32_0 ->
	 (write_byte' env 0xC3);
    | Retn (Some stuff) ->
	 (write_byte' env 0xC2);
	 (write_2bytes env stuff);

    | Sahf ->
	 (write_byte' env 0x9E);

    | Setcc (cond, op) ->
	 (write_byte' env 0x0F);
	 (write_byte' env (0x90 lor (encode_condition cond)));
	 (write_regmem_operand env 0x0 op);

    | Shld (op, reg, None) ->
	 (write_shift_double_instr env 0xA5 op reg n1);
    | Shld (op, reg, (Some shiftamt)) ->
	 (write_shift_double_instr env 0xA4 op reg shiftamt);
    | Shrd (op, reg, None) ->
	 (write_shift_double_instr env 0xAD op reg n1);
    | Shrd (op, reg, (Some shiftamt)) ->
	 (write_shift_double_instr env 0xAC op reg shiftamt);

    | Stc ->
	 (write_byte' env 0xF9);

    | Test (op1, Immed immed) ->
	 (write_byte' env (0xF6 lor (word_flag true)));
	 (write_regmem_operand env 0x0 op1);
	 (write_immed env immed);
    | Test (op1, op2) ->
	 let operands = (sort_operands (op1, op2)) in
	 (write_byte' env (0x84 lor (word_flag true)));
	 (write_operands env operands);
	 (* Note: I don't know what to do with Test reg,mem --
	    it seems as if it should
	    be reversed. MASM just encodes it as Test mem, reg. See book. *)

    | Xchg (regmemop, regop) ->
	 (write_byte' env (0x86 lor (word_flag true)));
	 (write_regmem_operand env (encode_register regop) regmemop);

(***** Macros *****)

	 (* Coerce register : no code value. *)
    | Coerce (reg_coerce) ->
	 (write_macro env (coerce_macro reg_coerce));
    | CoerceName nc ->
	(write_macro env (coerceName_macro nc));

	 (* Comment : no code value. *)
    | Comment (str) ->
	 (write_macro env (comment_macro str));

	 (* Fallthru : no code value. Only valid when preceding a label. *)
    | Fallthru (conlist) ->
	 (write_macro env (fallthru_macro conlist));

	 (* Malloc : allocate item of size i into register r *)
    | Malloc (x, size, mallocarg) ->
	 (write_macro env (malloc_macro (x, size, mallocarg)))

    | Proof pf -> 
	(write_macro env (proof_macro pf))

	 (* Unpack : effectively a move, if registers are different. *)
    | Unpack (var, dstreg, (Reg varreg, _)) when dstreg = varreg ->
	 ();
    | Unpack (var, dstreg, varval_coerce) ->
	 (write_macro env (unpack_macro (var, dstreg, varval_coerce)))
    | Sunpack (v,g) ->
	(write_macro env (sunpack_macro (v,g)))

    | Nameobj (x,gop) ->
	(write_macro env (nameobj_macro (x,gop)))
    | ForgetUnique x -> 
	(write_macro env (forgetunique_macro x))
    | RemoveName x -> 
	(write_macro env (removename_macro x))

(* Floating Point *)
    | FPnoargs x ->
	write_fpnoargs env x 
    | FPsomeargs (x,args) ->
	write_fpsomeargs env x args

(* Cyclone *)
    | CgStart (arg1,arg2) ->
	 (write_macro env (cgstart_macro arg1 arg2))
    | CgDump (cg_reg, tv, reg, lab) ->
	 (write_macro env (cgdump_macro cg_reg tv reg lab))
    | CgHole (reg, labeloftmpl, holelab) ->
	 (* Insert symbol for hole *)
	 (cyc_insert_hole_symbol env holelab);
	 (write_macro env (cghole_macro reg labeloftmpl holelab))
    | CgHoleJmp (tmpllab, (holelab, crc)) ->
	 (* Insert symbol for hole *)
	 (cyc_insert_hole_symbol env holelab);
	 (write_macro env (cgholejmp_macro tmpllab holelab))
    | CgHoleJcc (cc, tmpllab, (holelab,crc),_) ->
	 (* Insert symbol for hole *)
	 (cyc_insert_hole_symbol env holelab);
	 (write_macro env (cgholejcc_macro cc tmpllab holelab))
    | CgFill (cg_reg, reg1, labeloftmpl, labelofhole, reg2) ->
	(* -$ i32_2 is puts the hole back at the start of the hole
	   instruction.  Otherwise it points 2 in. See comments in
	   talasm.ml *)
	 let tmpl_pos = (cyc_label_position env labeloftmpl) in
	 let hole_pos = (cyc_label_position env labelofhole) in
	 (write_macro env (cgfill_macro cg_reg reg1 tmpl_pos hole_pos reg2))
    | CgFillJmp (cg_reg, 
		 holereg, htmplab, holelab, 
		 targreg, ttmplab, targlab) ->
	 let htmp_pos = (cyc_label_position env htmplab) in
	 let hole_pos = (cyc_label_position env holelab) in
	 let ttmp_pos = (cyc_label_position env ttmplab) in
	 let targ_pos = (cyc_label_position env targlab) in
	 (write_macro env 
	    (cgfilljmp_macro cg_reg holereg htmp_pos hole_pos targreg
	       ttmp_pos targ_pos))
    | CgFillJcc (cg_reg, 
		 holereg, htmplab, holelab, 
		 targreg, ttmplab, targlab) ->
	 let htmp_pos = (cyc_label_position env htmplab) in
	 let hole_pos = (cyc_label_position env holelab) in
	 let ttmp_pos = (cyc_label_position env ttmplab) in
	 let targ_pos = (cyc_label_position env targlab) in
	 (write_macro env 
	    (cgfilljcc_macro cg_reg holereg htmp_pos hole_pos targreg
	       ttmp_pos targ_pos))
    | CgForget (tv1,tv2) -> ()
    | CgEnd reg -> (write_macro env (cgend_macro reg))

(* End Cyclone *)

    | Vcase _ -> (write_macro env (vcase_macro ))
    | Letprod _ -> (write_macro env (letprod_macro ))
    | Letroll _ -> (write_macro env (letroll_macro )) 

   (* | _ -> (invalid_arg "write_instr: Unknown instruction or invalid args") *)
;;

(***** Data encoding *****)

(* write_data_item
   Side effects:
     - Writes a data item to the environment's schan.
     - Adds relocations to the env's relocation list. *)

let write_data_item env ditem =
   match ditem with
      Dlabel (labl, _) ->
	 (write_public_label_addend env Absolute labl i32_0)
    | Dbytes (bytes) ->
	 (Stringchan.put_string env.a_schan bytes)
    | D2bytes (bytes) ->
	 (write_2bytes env (int16_to_int32 bytes))
    | D4bytes (bytes, _) ->
	 (write_4bytes env bytes)
    | Drep (ri, str) -> 
	 let str = deSome (!str) in 
	 let len = String.length str in 
	 Stringchan.put_4bytes env.a_schan len;
	 Stringchan.put_string env.a_schan str
    | Dfloat32 f32 -> 
	 (write_f32 env f32)
    | Dfloat64 f64 ->
	 (write_f64 env f64)
    | Djunk ->
	 (write_4bytes env i32_0)
    | Dup -> ()
    | Ddown -> ()
;;

(* EOF: talasmx86.ml *)
