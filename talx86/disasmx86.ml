(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     August 1999, all rights reserved.                              *)
(**********************************************************************)
(* disasmx86.ml
 * 
 * Disassembles an object file.  Provides most of the guts of instruction
 * decoding.  
 *
 * Notes:
 *   

***** Relocation offsets *****
   In ELF, relocations for jumps are relative to PC, but Intel jumps are
   taken relative to PC+4.  As a quick fix, the elf.ml file applies an
   addend of -4 at each relative jump, as opposed to 0 in the COFF case.
   In disassembly we have to "undo" this so that things are logically
   the same.  Therefore, in read_addr we allow displacements of -4, 
   assuming that they were placed by elf.ml.
*****
*)   

open Utilities;;
open Numtypes;;
open Identifier;;
open Tal;;
open Disobjfile;;
open Printf;;

let fail objf str = 
  let (sect,addr) = objf.obj_get_pos() in
  let sect = string_of_int sect in
  let addr = string_of_int addr in
  failwith ("sec="^sect^",addr="^addr^": "^str)
;;

let warn objf str =
  let (sect,addr) = objf.obj_get_pos() in
  fprintf stderr "Warning sect=%d,addr=%d: %s\n" sect addr str 
;;

let int_to_scale i = 
  match i with
    0 -> Byte1
  | 1 -> Byte2
  | 2 -> Byte4
  | 3 -> Byte8
  | _ -> failwith "int_to_scale: bad int"
;;

(* set to false temporarily by special opcodes *)
let mode_32 = ref true;;

(* op_length ():  Gives the length of word instructions in this mode. *)
let op_length () = if !mode_32 then 4 else 2;;

(* assert_32
   Raises an exception if not in 32-bit mode. *)
let assert_32 objf = 
  if (not !mode_32) then (fail objf "Not in 32-bit mode!!!");;

(***** Flags & Codes *****)
let word_flag_bit = 0x1;;
let sign_extend_bit = 0x2;;
let direction_bit = 0x2;;

(* has_bit
   Returns TRUE if 'byte' has the given bit set. *)
let has_bit byte bit = ((byte land bit) <> 0);;

(* assert_wf
   Arg b : a byte.
   Raises an exception if b does not have its word_flag set. *)
let assert_wf objf b = 
   if (not (has_bit b word_flag_bit)) then 
     let b = string_of_int b in
     fail objf ("Word_flag not set!!!  byte="^b)
   else assert_32 objf;;

(* length_from_sext
   Given an opcode byte with a sign-extend bit, returns the length of 
   its signed operand. *)
let length_from_sext b = 
   if (has_bit b sign_extend_bit) then 1 else op_length()
;;

(* modregrm & sib
   Splits a byte into mod;reg;rm fields.
   Returns: (mod, reg, rm). *)
let modregrm b = ((b lsr 6) land 0x3, (b lsr 3) land 0x7, b land 0x7);;
let sib = modregrm;;

let decode_register = function
 | 0x0 -> Eax
 | 0x1 -> Ecx
 | 0x2 -> Edx
 | 0x3 -> Ebx
 | 0x4 -> Esp
 | 0x5 -> Ebp
 | 0x6 -> Esi
 | 0x7 -> Edi
 | _ -> (failwith "decode_register: bad register code")
;;

let decode_condition = function
 | 0x7 -> Above
 | 0x3 -> AboveEq
 | 0x2 -> Below
 | 0x6 -> BelowEq
 | 0x4 -> Eq
 | 0xF -> Greater
 | 0xD -> GreaterEq
 | 0xC -> Less 
 | 0xE -> LessEq
 | 0x5 -> NotEq
 | 0x1 -> NotOverflow
 | 0x9 -> NotSign
 | 0x0 -> Overflow
 | 0xA -> ParityEven
 | 0xB -> ParityOdd
 | 0x8 -> Sign
 | _ -> failwith "decode_condition: bad code."
;;

let decode_arithbin_opcode = function 
 | 0x2 -> Adc 
 | 0x0 -> Add
 | 0x4 -> And
 | 0x1 -> Or
 | 0x3 -> Sbb
 | 0x5 -> Sub
 | 0x6 -> Xor
 | _ -> failwith "decode_arithbin_opcode: bad code."
;;

let decode_arithsr_subopcode = function
 | 0x0 -> Rol
 | 0x1 -> Ror
 | 0x2 -> Rcl
 | 0x3 -> Rcr
 | 0x4 -> Shl
 | 0x5 -> Shr
 | 0x7 -> Sar
 | _ -> failwith "decode_arithsr_subopcode: bad code."
;;

(***** Operand Reading *****)

let read_num_of_length objf len = 
  match len with
    0 -> i32_0
  | 1 -> 
      let b = objf.obj_get_byte() in
      let w = (b lsl 23) asr 23 in
      int_to_int32(w)
  | 2 -> 
      let b_lo = objf.obj_get_byte() in
      let b_hi = objf.obj_get_byte() in
      let w0 = b_lo lor (b_hi lsl 8) in
      let w = (w0 lsl 15) asr 15 in
      int_to_int32(w)
  | 4 -> 
      let b_0 = objf.obj_get_byte() in
      let b_1 = objf.obj_get_byte() in
      let b_2 = objf.obj_get_byte() in
      let b_3 = objf.obj_get_byte() in
      (int_to_int32((b_3 lsl 24) lor (b_2 lsl 16) lor (b_1 lsl 8) lor b_0))
  | _ -> fail objf "read_num_of_length: bad length"
;;
      
(* read_immed_addr
   Reads an immediate operand of the given length (0, 1, 2, or 4 bytes).
   If there is no relocation associated with the immed, it is an Immed.
   If there is one and the displacement is 0, it is an Addr.
   If there is one and the displacement is non-0, it is an error.
 *)
let read_immed_addr objf len = 
  let labopt = objf.obj_get_reloc() in
  let i = read_num_of_length objf len in
  match labopt with
    None -> Immed i
  | Some lbl -> 
      if i =$ i32_0 then (Addr lbl) else
      fail objf "read_immed_addr: relocation address displacement found."
;;

(* Need to consider non-zero addends for ELF disassemblies since elf.ml
   added -4 to deal with Pentium addressing *)
let elf_addend = (int_to_int32 (-4))
let read_addr objf len = 
  let labopt = objf.obj_get_reloc() in
  let i = read_num_of_length objf len in
  match labopt with
    Some lbl -> 
      if (i =$ i32_0) ||
         (i =$ elf_addend) then 
	lbl 
      else
	fail objf "read_addr:  relocation address displacement found"
  | None -> 
      match objf.obj_get_labels (int32_to_int i) with
	[] -> fail objf "read_addr:  no relative address label found"
      |	lbl::rest -> lbl
;;

let read_ptr16 objf op_len  =
  let i = read_num_of_length objf 2 in (* read in 16-bit segment *)
  if (i =$ i32_0) then read_addr objf op_len
  else fail objf "read_ptr16_32: segment non-zero."
;;

(* Reads an immediate operand, verifying that there is no relocation *)
let read_immed objf len =
   match (read_immed_addr objf len) with
      Immed i -> Immed i
    | _ -> fail objf "read_immed: not an immediate."
;;

(* Reads a label and displacement operand -- used to form Prjl genop *)
let read_label_disp objf len = 
  let labopt = objf.obj_get_reloc() in
  let i = read_num_of_length objf len in
  match labopt with
    None -> fail objf "read_label_disp:  relocation not found."
  | Some lbl -> (lbl,i)
;;  

(* read_rm_operand_aux
   Reads a reg/mem operand.
   Precondition: File marker must be on byte following mod/reg/rm byte.
   Postcondition: File marker is on byte following the rm operand.
   Returns: an (int * genop) pair, where the int is the value of the
     reg field, and the genop is the operand indicated by the rm field
     and any subsequent fields. *)
let read_rm_operand_aux objf modb =
   let read_prjl () =
      let (lbl, disp) = read_label_disp objf (op_length()) in
      Prjl ((lbl, []), disp, None)
   in

   let get_disp modv = 
     let length = 
       (match modv with
	 0 -> 0
       | 1 -> 1
       | 2 -> op_length()
       | _ -> fail objf "get_disp: weird displacement!") in
     read_num_of_length objf length in

   let read_prjr (modv, rm) = 
      let disp = get_disp modv in
      Prjr (((decode_register rm), []), disp, None)
   in
    
   let (modv,reg,rm) = modregrm modb in
   if modv = 0x3 then
     (* covers last block of Mod/RM table *)
      (reg, Reg (decode_register rm))
   else if modv = 0x0 && rm = 0x5 then
     (* covers block 0, rm=101 *)
      (reg, read_prjl ())
   else if rm = 0x4 then
     (* covers situation when there's a sib *)
       let sibb = objf.obj_get_byte() in
       let (scale, index, base) = sib sibb in 
       let disp = get_disp modv in
       (* special case column marked with [*] *)
       if base = 0x5 then
	 if index = 0x4 then
	   begin
	     (* we can't figure out what the hell this is supposed to mean *)
	     if scale <> 0x0 then fail objf "index is 4 but scale is not 0";
	     fail objf "rm = 0x4, scale = 0x0, base = 0x5, index = 0x4"
	   end
	 else 
	   begin
	     if modv = 0x0 then
	       let disp = read_num_of_length objf 4 in
	       (reg, Prjr((decode_register index,[]),disp,None))
	     else 
	       (* this covers both cases when MOD=01 and MOD=10 since
		* we've already read the proper size displacement *)
	       (reg, Prjr((Ebp,[]),disp,Some(Byte1,decode_register index)))
	   end
       (* special case rows marked with none *)
       else if index = 0x4 then
	 begin
	   if scale <> 0 then fail objf "index is 4 but scale is not 0";
	   (reg, Prjr((decode_register base,[]),disp,None))
	 end
       else
	 (reg, Prjr((decode_register base,[]),disp,Some(int_to_scale scale,
						      decode_register index)))
   else 
     (* covers blocks 0, 1, and 2 when no sib nor disp32 special case *)
     (reg, read_prjr (modv, rm))
;;

(* Reads the mod/reg/rm byte and then processes as above *)
let read_rm_operand objf = read_rm_operand_aux objf (objf.obj_get_byte ());; 

(* read_rm_operands
   Reads 2 reg/mem operands and returns them in a pair, with their order
   based on the direction bit of the instruction's opcode. 

   Precondition: File marker must be on a mod/reg/rm byte. 
   Returns: an instruction pair (dstop, srcop). *)
let read_rm_operands objf b1 =
   let (regcode, memop) = (read_rm_operand objf) in
   let regop = Reg (decode_register regcode) in
   if (has_bit b1 direction_bit) then 
      (regop, memop)
   else
      (memop, regop)
;;

let make_signed_byte u = 
  if (u land 0x80) = 0 then u 
  else - (((lnot u) land 0xff) + 1)
;;

(* read_8bit_label
   Reads an 8-bit displacement and returns the (first) symbol that it 
   refers to. *)
let read_8bit_label objf =
   let disp = make_signed_byte (objf.obj_get_byte ()) in
   match objf.obj_get_labels disp with 
     [] -> fail objf "read_8bit_label: label not found at (%s)"
   | (lbl::_) -> lbl
;;
   
(***** Individual Instructions *****)
let arithbin_rm_rm objf b1 operation = 
   assert_wf objf b1;
   let (dst, src) = (read_rm_operands objf b1) in
   ArithBin (operation, dst, src)
;;

(***** Instruction groups *****)
let group_1 objf b1 = 
   assert_wf objf b1;
   let (subopcode,dst) = (read_rm_operand objf) in
   let immed = (read_immed_addr objf (length_from_sext b1)) in
   if subopcode = 0x7 then
      Cmp ((dst,[]), (immed,[]))
   else
      ArithBin ((decode_arithbin_opcode subopcode), dst, immed)
;;

let group_2 objf b1 = 
   assert_wf objf b1;
   let (subopcode,dst) = (read_rm_operand objf) in
   if (has_bit b1 0x10) then
      ArithSR ((decode_arithsr_subopcode subopcode), dst, None)
   else
      let shval = read_num_of_length objf 1 in
      ArithSR ((decode_arithsr_subopcode subopcode), dst, (Some shval))
;;

let group_3 objf b1 = 
   assert_wf objf b1;
   let (subopcode,op) = (read_rm_operand objf) in
   match subopcode with
    | 0x0 ->				(* Test op, immed *)
	 let immed = (read_immed objf (op_length())) in
	 Test (op, immed)
    | 0x2 ->
	 ArithUn (Not, op)
    | 0x3 -> 
	 ArithUn (Neg, op)
    | 0x4 -> 
	 ArithMD (Mul, op)
    | 0x5 -> 
	 ArithMD (Imul1, op)
    | 0x6 ->
	 ArithMD (Div, op)
    | 0x7 -> 
	 ArithMD (Idiv, op)
    | _ -> fail objf "group_3: bad subopcode."
;;


let group_5 objf b1 = 
   assert_wf objf b1;
   let crc = [] in
   let (subopcode,op) = (read_rm_operand objf) in
   match subopcode with
      0x0 ->
	 ArithUn (Inc, op)
    | 0x1 ->
	 ArithUn (Dec, op)
    | 0x2 -> 
	 Call (op, crc)
    | 0x4 ->
	 Jmp (op, crc)
    | 0x6 ->
	 Push (op, crc)
    | _ -> fail objf "group_5: Bad subopcode."
;;

(******* Floating point ******************)

(* All Floating Point operations begin with the prefix 11011
 * See Intel Architecture Software Developper's Manual, Volume 2
 * Used instruction descriptions from section 3.
 *)

let fp_unimplemented objf s = fail objf ("unimplemented FP: "^s)
let fp_unknown objf b1 b2 = 
  let s = (string_of_int b1)^" "^(string_of_int b2) in
  fail objf ("unknown FP: "^s)

(* The last 3 bits of the byte indicate a floating-point register r. 
 * Separate the byte b into (opcode,r) where b = opcode + r.
 *)
let fp_reg b = ((b lsr 3) lsl 3, b land 0x7);;

(* bits 2,3,4 (of a byte with bits 0,1,2,3,4,5,6,7) indicate part of an
 * opcode.  The bits 0,1 and 5,6,7 are the mod and rm fields respectively.
 * return bits 2,3,4
 *)
let fp_modrm b = (b lsr 3) land 0x7

(* disassemble the standard fp operation from its opcode *)
let fp_standardop opcode =
  match opcode with
    0x0 -> Fadd
  | 0x1 -> Fmul
  | 0x2 -> Fcom
  | 0x3 -> Fcomp
  | 0x4 -> Fsub
  | 0x5 -> Fsubr
  | 0x6 -> Fdiv
  | 0x7 -> Fdivr
  | _   -> invalid_arg "fp_standardop"

(* disassemble the integer variant from its opcode *)
let fp_intop opcode =
  match opcode with
    0x0 -> Fiadd
  | 0x1 -> Fimul
  | 0x2 -> Ficom
  | 0x3 -> Ficomp
  | 0x4 -> Fisub
  | 0x5 -> Fisubr
  | 0x6 -> Fidiv
  | 0x7 -> Fidivr
  | _   -> invalid_arg "fp_standardop"

(* return an instruction that's encoding uses the mod/rm fields *)
let make_fpmodrm objf b2 op scale =
  let (_,g) = read_rm_operand_aux objf b2 in
  FPsomeargs (op, FPgenop (scale,g))

(* return an instruction that takes 2 register arguments (ST and reg). 
 * if st_first then ST is the first (destination) register.
 *)
let make_fpregs op st_first reg =
  FPsomeargs (op, FPstack2 (st_first,reg))

(* return an instruction that takes a single register argument. *)
let make_fpreg op reg = FPsomeargs (op, FPstack reg)

(* Decode the Floating Point Instructions *)

(* 11011 000 *)
let floatD8 objf =
  let b2 = objf.obj_get_byte () in
      (* Check for register operations *)
      let (opcode,reg) = fp_reg b2 in
      match opcode with
	0xC0 -> make_fpregs Fadd  true reg
      |	0xD0 -> make_fpregs Fcom  true reg
      |	0xD8 -> make_fpregs Fcomp true reg
      |	0xF0 -> make_fpregs Fdiv  true reg
      |	0xF8 -> make_fpregs Fdivr true reg
      |	0xC8 -> make_fpregs Fmul  true reg
      |	0xE0 -> make_fpregs Fsub  true reg
      |	0xE8 -> make_fpregs Fsubr true reg
      |	_ ->
	  (* Check for operations (Fadd,Fcom,...) with mod/rm byte *)
	  let op = fp_standardop (fp_modrm b2) in
	  make_fpmodrm objf b2 op Byte4

(* 11011 001 *)
let floatD9 objf =
  let b2 = objf.obj_get_byte () in
  match b2 with
    0xF0 -> FPnoargs F2xm1
  | 0xE1 -> FPnoargs Fabs
  | 0xE0 -> FPnoargs Fchs
  | 0xFF -> FPnoargs Fcos
  | 0xF6 -> FPnoargs Fdecstp
  | 0xF7 -> FPnoargs Fincstp

  | 0xE8 -> FPnoargs Fld1
  | 0xE9 -> FPnoargs Fldl2t
  | 0xEA -> FPnoargs Fldl2e
  | 0xEB -> FPnoargs Fldpi
  | 0xEC -> FPnoargs Fldlg2
  | 0xED -> FPnoargs Fldln2
  | 0xEE -> FPnoargs Fldz

  | 0xD0 -> FPnoargs Fnop
  | 0xF3 -> FPnoargs Fpatan
  | 0xF8 -> FPnoargs Fprem
  | 0xF5 -> FPnoargs Fprem1
  | 0xF2 -> FPnoargs Fptan
  | 0xFC -> FPnoargs Frndint
  | 0xFD -> FPnoargs Fscale
  | 0xFE -> FPnoargs Fsin
  | 0xFB -> FPnoargs Fsincos
  | 0xFA -> FPnoargs Fsqrt
  | 0xE4 -> FPnoargs Ftst
  | 0xE5 -> FPnoargs Fxam
  | 0xF4 -> FPnoargs Fxtract
  | 0xF1 -> FPnoargs Fyl2x
  | 0xF9 -> FPnoargs Fyl2xp1

  | _ ->
      (* Check for register operations *)
      let (opcode,reg) = fp_reg b2 in
      match opcode with
	0xC0 -> make_fpreg Fld reg
      |	0xC8 -> make_fpreg Fxch reg
      |	_ ->
	  (* Check for operations with mod/rm byte *)
	  let opcode = fp_modrm b2 in
	  match opcode with
	    0x0 -> make_fpmodrm objf b2 Fld  Byte4
	  | 0x2 -> make_fpmodrm objf b2 Fst  Byte4
	  | 0x3 -> make_fpmodrm objf b2 Fstp Byte4
	  | 0x4 -> fp_unimplemented objf "Fldenv 2 bytes"
	  | 0x5 -> fp_unimplemented objf "Fldcw 2 bytes"
	  | 0x6 -> fp_unimplemented objf "Fstenv weird"
	  | 0x7 -> fp_unimplemented objf "Fstcw 2 bytes"
	  | _ -> fp_unknown objf 0xD9 b2

(* 11011 010 *)
let floatDA objf =
  let b2 = objf.obj_get_byte () in
  match b2 with
    0xE9 -> FPnoargs Fucompp
  | _ ->
	  (* Check for operations with mod/rm byte *)
	  let op = fp_intop (fp_modrm b2) in
	  make_fpmodrm objf b2 op Byte4

(* 11011 011 *)
let floatDB objf =
  let b2 = objf.obj_get_byte () in
  match b2 with
    0xE2 -> FPnoargs Fnclex
  | 0xE3 -> FPnoargs Fninit
  | _ ->
      (* Check for register operations *)
      let (opcode,reg) = fp_reg b2 in
      match opcode with
	0xF0 -> make_fpregs Fcomi  true reg
      |	0xE8 -> make_fpregs Fucomi true reg
      |	_ ->
	  (* Check for operations with mod/rm byte *)
	  let opcode = fp_modrm b2 in
	  match opcode with
	    0x0 -> make_fpmodrm objf b2 Fild  Byte4
	  | 0x2 -> make_fpmodrm objf b2 Fist  Byte4
	  | 0x3 -> make_fpmodrm objf b2 Fistp Byte4
	  | 0x5 -> fp_unimplemented objf "Fld 80-bit real"
	  | 0x7 -> fp_unimplemented objf "Fstp 80-bit real"
	  | _ -> fp_unknown objf 0xDB b2

(* 11011 100 *)
let floatDC objf =
  let b2 = objf.obj_get_byte () in
      (* Check for register operations *)
      let (opcode,reg) = fp_reg b2 in
      match opcode with
	0xC0 -> make_fpregs Fadd  false reg
      |	0xF8 -> make_fpregs Fdiv  false reg
      |	0xF0 -> make_fpregs Fdivr false reg
      |	0xC8 -> make_fpregs Fmul  false reg
      |	0xE8 -> make_fpregs Fsub  false reg
      |	0xE0 -> make_fpregs Fsubr false reg
      |	_ ->
	  (* Check for operations with mod/rm byte *)
	  let op = fp_standardop (fp_modrm b2) in
	  make_fpmodrm objf b2 op Byte8

(* 11011 101 *)
let floatDD objf =
  let b2 = objf.obj_get_byte () in
      (* Check for register operations *)
      let (opcode,reg) = fp_reg b2 in
      match opcode with
	0xC0 -> make_fpreg Ffree reg
      |	0xD0 -> make_fpreg Fst   reg 
      |	0xD8 -> make_fpreg Fstp  reg
      |	0xE0 -> make_fpreg Fucom reg
      |	0xE8 -> make_fpreg Fucomp reg
      |	_ ->
	  (* Check for operations with mod/rm byte *)
	  let opcode = fp_modrm b2 in
	  match opcode with
	    0x0 -> make_fpmodrm objf b2 Fld  Byte8
	  | 0x2 -> make_fpmodrm objf b2 Fst  Byte8
	  | 0x3 -> make_fpmodrm objf b2 Fstp Byte8
	  | 0x7 -> make_fpmodrm objf b2 Fnstsw Byte2
	  | 0x4 -> fp_unimplemented objf "Frstor"
	  | 0x6 -> fp_unimplemented objf "Fnsave"
	  | _ -> fp_unknown objf 0xDD b2

(* 11011 110 *)
let floatDE objf =
  let b2 = objf.obj_get_byte () in
  match b2 with
    0xD9 -> FPnoargs Fcompp
  | _ ->
      (* Check for register operations *)
      let (opcode,reg) = fp_reg b2 in
      match opcode with
	0xC0 -> make_fpregs Faddp  false reg
      |	0xF8 -> make_fpregs Fdivp  false reg
      |	0xF0 -> make_fpregs Fdivrp false reg
      |	0xC8 -> make_fpregs Fmulp  false reg
      |	0xE8 -> make_fpregs Fsubp  false reg
      |	0xE0 -> make_fpregs Fsubrp false reg
      |	_ ->
	  (* Check for operations with mod/rm byte *)
	  let op = fp_intop (fp_modrm b2) in
	  make_fpmodrm objf b2 op Byte2

(* 11011 111 *)
let floatDF objf =
  let b2 = objf.obj_get_byte () in
  match b2 with
    0xE0 -> FPsomeargs (Fnstsw, FPgenop (Byte2,Reg Eax))
  | _ ->
      (* Check for register operations *)
      let (opcode,reg) = fp_reg b2 in
      match opcode with
	0xF0 -> make_fpregs Fcomip  true reg
      |	0xE8 -> make_fpregs Fucomip true reg
      |	_ ->
	  (* Check for operations with mod/rm byte *)
	  let opcode = fp_modrm b2 in
	  match opcode with
	    0x0 -> make_fpmodrm objf b2 Fild  Byte2
	  | 0x2 -> make_fpmodrm objf b2 Fist  Byte2
	  | 0x3 -> make_fpmodrm objf b2 Fistp Byte2
	  | 0x5 -> make_fpmodrm objf b2 Fild  Byte4
	  | 0x7 -> make_fpmodrm objf b2 Fistp Byte8
	  | _ -> fp_unknown objf 0xDF b2

let float9B objf =
  (* If the following bytes don't match we should treat this as a Wait
     instruction.  We don't because obj doesn't have a peek method. *)
  let b2 = objf.obj_get_byte () in
  let b3 = objf.obj_get_byte () in
  match b2,b3 with
  | 0xDB,0xE3 -> FPnoargs(Finit)
  | 0xDB,0xE2 -> FPnoargs(Fclex)
  | _,_ -> fail objf ("Unknown FP instruction code: "^(string_of_int b2))

(******* The Big Instruction Table *******)

(* get_instr
   Reads and disassembles the next instruction or macro in the object file.
   Returns: a TAL instruction. *)

let rec get_instr objf = 
   let crc = [] in

   let b1 = objf.obj_get_byte () in
   match b1 with
      (* ArithBin *)
    | 0x00 | 0x01 | 0x02 | 0x03 ->	(* ArithBin Add, rm1, rm2 *)
	arithbin_rm_rm objf b1 Add
    | 0x10 | 0x11 | 0x12 | 0x13 ->	(* ArithBin Adc, rm1, rm2 *)
	arithbin_rm_rm objf b1 Adc
    | 0x20 | 0x21 | 0x22 | 0x23 ->	(* ArithBin And, rm1, rm2 *)
	arithbin_rm_rm objf b1 And
    | 0x30 | 0x31 | 0x32 | 0x33 ->	(* ArithBin Xor, rm1, rm2 *)
	arithbin_rm_rm objf b1 Xor
    | 0x08 | 0x09 | 0x0A | 0x0B ->	(* ArithBin Or,  rm1, rm2 *)
	arithbin_rm_rm objf b1 Or
    | 0x18 | 0x19 | 0x1A | 0x1B ->	(* ArithBin Sbb, rm1, rm2 *)
	arithbin_rm_rm objf b1 Sbb
    | 0x28 | 0x29 | 0x2A | 0x2B ->	(* ArithBin Sub, rm1, rm2 *)
	arithbin_rm_rm objf b1 Sub

	 (* ArithSR *)

					(* ArithUn Dec, reg *)
    | 0x48 | 0x49 | 0x4A | 0x4B | 0x4C | 0x4D | 0x4E | 0x4F ->
	assert_32 objf;
	ArithUn (Dec, (Reg (decode_register (b1 land 0x7))))
					(* ArithUn Inc, reg *)
    | 0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 ->   
	assert_32 objf;
	ArithUn (Inc, (Reg (decode_register (b1 land 0x7))))

	 (* C *)
    | 0xE8 ->				(* Call Addr *)
	Call (Addr(read_addr objf (op_length())), crc)
    | 0xF8 ->				(* Clc *)
	Clc
    | 0xF5 ->				(* Cmc *)
	Cmc
    | 0x38 | 0x39 | 0x3A | 0x3B ->	(* Cmp rm, rm *)
	assert_wf objf b1;
	let (op1, op2) = read_rm_operands objf b1 in
	Cmp ((op1,[]), (op2,[]))
    | 0x98 ->				(* Conv Cwde (Cwd if 16-bit mode) *)
	if !mode_32 then Conv (Cwde) else Conv (Cwd)
    | 0x99 ->				(* Conv Cdq (Cbw if 16-bit mode) *)
	if !mode_32 then Conv (Cdq) else Conv (Cbw)

	 (* I *)
    | 0x69 | 0x6B ->			(* Imul3 r1, r2, i (Imul2 if r1=r2)*)
	assert_32 objf;
	let (dstregcode, srcop) = read_rm_operand objf in
	let immedop = read_num_of_length objf (length_from_sext b1) in
	Imul3 ((decode_register dstregcode), srcop, immedop)
    | 0xCD ->				(* Int *)
	Int (int32_to_int8 (int_to_int32 (objf.obj_get_byte ())))
    | 0xCE ->				(* Into *)
	Into

	 (* J *)
					(* Jcc cond, label_8 *)
    | 0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75 | 0x76 | 0x77
    | 0x78 | 0x79 | 0x7A | 0x7B | 0x7C | 0x7D | 0x7E | 0x7F ->    
	let cond = (decode_condition (b1 land 0xF)) in
	let labl = (read_8bit_label objf) in
	Jcc (cond, (labl, crc),None)
	 
    | 0xE3 ->				(* Jecxz *)
	let labl = (read_8bit_label objf) in
	Jecxz ((labl, crc), None)

    | 0xEB ->				(* Jmp label_8 *)
	let labl = (read_8bit_label objf) in
	Jmp (Addr labl, crc)

    | 0xE9 ->				(* Jmp label_16|32 *)
	let labl = (read_addr objf (op_length())) in
	Jmp (Addr labl, crc)
	 
	 (* L *)
    | 0x9F ->				(* Lahf *)
	Lahf
    | 0x8D ->				(* Lea reg, mem *)
	assert_32 objf;
	let (dstcode, srcop) = (read_rm_operand objf) in
	Lea ((decode_register dstcode), srcop)
    | 0xE2 ->				(* Loopd label_8, None *)
	let labl = (read_8bit_label objf) in
	Loopd ((labl, crc), None)
	 
    | 0xE0 | 0xE1 ->			(* Loopd label_8, Some flag *)
	let labl = (read_8bit_label objf) in
	Loopd ((labl, crc), Some ((b1 land 0x1) <> 0))

	 (* M *)
					(* Mov reg, immed/addr *)
    | 0xB0 | 0xB1 | 0xB2 | 0xB3 | 0xB4 | 0xB5 | 0xB6 | 0xB7
    | 0xB8 | 0xB9 | 0xBA | 0xBB | 0xBC | 0xBD | 0xBE | 0xBF ->    
	assert_wf objf (b1 lsr 3);
	let dstreg = decode_register (b1 land 0x7) in
	let immed_addr = read_immed_addr objf (op_length()) in
	Mov (Reg dstreg, (immed_addr, crc))
	 
    | 0xC6 | 0xC7 ->			(* Mov mem, immed/addr *)
	assert_wf objf b1;
	let (dummy,dst) = read_rm_operand objf in
	if dummy <> 0x0 then (warn objf "Mov format incorrect");
	let immed_addr = read_immed_addr objf (op_length()) in
	Mov (dst, (immed_addr, crc))
	 
	 
    | 0x88 | 0x89 | 0x8A | 0x8B ->	(* Mov rm, rm *)
	if (has_bit b1 word_flag_bit) then
	  begin
	    (* assert_32(); JGM: could be prefixed with opsize 0x66? *)
	    let (dst, src) = read_rm_operands objf b1 in
	    Mov (dst, (src, crc))
	  end
	else (* Movpart; JGM: this is totally incomplete -- I'm working
	      * without a manual... *)
	  begin
	    let (dst0, src0) = read_rm_operands objf b1 in
	    let translate_genop genop = 
	      match genop with
		Reg Esp -> (Reg Eax,RPh)
	      |	Reg Ebp -> (Reg Ecx,RPh)
	      |	Reg Edi -> (Reg Ebx,RPh)
	      |	Reg Esi -> (Reg Edx,RPh)
	      |	Reg r   -> (Reg r  ,RPl)
	      |	_ ->       (genop  ,RPl) in
	    let (dst,reg_part1) = translate_genop dst0 in
	    let (src,reg_part2) = translate_genop src0 in
	    let signbit = false in
	    Movpart (false, dst, reg_part1, src, reg_part2)
	  end
	 (* N *)
    | 0x90 ->				(* Nop *)
	Nop

	 (* P *)
					(* Pop reg *)
    | 0x58 | 0x59 | 0x5A | 0x5B | 0x5C | 0x5D | 0x5E | 0x5F ->       
	assert_32 objf;
	Pop (Reg (decode_register (b1 land 0x7)))
    | 0x8F ->				(* Pop rm *)
	assert_32 objf;
	let (dummy,rm) = read_rm_operand objf in
	if dummy <> 0 then (warn objf "Pop: bad format");
	Pop rm
    | 0x61 ->				(* Popad *)
	 Popad
    | 0x9D ->				(* Popfd *)
	 Popfd
					(* Push reg *)
    | 0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x56 | 0x57 ->
	assert_32 objf;
	Push ((Reg (decode_register (b1 land 0x7))), crc)	 
    | 0x68 | 0x6A ->			(* Push immed/addr *)
	assert_32 objf;
	let imm_addr = read_immed_addr objf (length_from_sext b1) in
	Push (imm_addr, crc)
    | 0x60 ->				(* Pushad *)
	Pushad
    | 0x9C ->				(* Pushfd *)
	Pushfd

	 (* R *)
    | 0xC3 ->				(* Retn None (or Some 0) *)
	Retn (None)
    | 0xC2 ->				(* Retn Some immed_16 *)
	Retn (Some (read_num_of_length objf 2))

	 (* S *)
    | 0x9E ->				(* Sahf *)
	Sahf
    | 0xF9 ->				(* Stc *)
	Stc

	 (* T *)
    | 0x84 | 0x85 ->			(* Test rm1, rm2*)
	assert_wf objf b1;
	let (regcode, memop) = read_rm_operand objf in
	Test ((Reg (decode_register regcode)), memop)
	 
	 (* X *)
    | 0x86 | 0x87 ->			(* Xchg rm, reg *)
	assert_wf objf b1;
	let (regcode, memop) = read_rm_operand objf in
	Xchg (memop, (decode_register regcode))

	 (* Special codes *)
    | 0x66 ->				(* Operand Size prefix *)
	let old_mode = !mode_32 in
	mode_32 := not !mode_32;
	let instr = (get_instr objf) in
	mode_32 := old_mode;
	instr
	 
    | 0x67 ->				(* Address Size prefix *)
	fail objf "Address Size prefix used (32-bit addresses only)!!!"

	 (* Two-byte instructions beginning with 0F *)
    | 0x0F -> begin			(* Two-byte escape *)
	let b2 = (objf.obj_get_byte ()) in
	match b2 with
	    (* B *)
					(* Bswap reg *)
	  0xC8 | 0xC9 | 0xCA | 0xCB | 0xCC | 0xCD | 0xCE | 0xCF -> 
	    assert_32 objf;
	    Bswap (decode_register (b1 land 0x7))

	       (* C *)
					(* Cmovcc cond, reg, src *)
	| 0x40 | 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47
	| 0x48 | 0x49 | 0x4A | 0x4B | 0x4C | 0x4D | 0x4E | 0x4F ->    
	    assert_32 objf;
	    let cond = decode_condition (b2 land 0xF) in
	    let (regcode, src) = read_rm_operand objf in
	    Cmovcc (cond, (decode_register regcode), (src, crc))
	       
	       (* J *)
					(* Jcc cond, label_32 *)
	| 0x80 | 0x81 | 0x82 | 0x83 | 0x84 | 0x85 | 0x86 | 0x87
	| 0x88 | 0x89 | 0x8A | 0x8B | 0x8C | 0x8D | 0x8E | 0x8F ->    
	    let cond = decode_condition (b2 land 0xF) in
	    let labl = read_addr objf (op_length()) in
	    Jcc (cond, (labl, crc),None)
	       
	       (* M *)
	| 0xBE | 0xBF ->		(* Conv (Movsx ...) *)
	    let (dst, src0) = read_rm_operands objf b2 in
	    let translate_genop genop = 
	      match genop with
		Reg Esp -> (Reg Eax,RPh)
	      |	Reg Ebp -> (Reg Ecx,RPh)
	      |	Reg Edi -> (Reg Ebx,RPh)
	      |	Reg Esi -> (Reg Edx,RPh)
	      |	Reg r   -> (Reg r  ,RPl)
	      |	_ ->       (genop  ,RPl) in
	    let reg_part1 = if !mode_32 then RPe else RPx in
	    let (src,reg_part2) = translate_genop src0 in
	    Movpart (false, dst, reg_part1, src, reg_part2)
	| 0xB6 | 0xB7 ->		(* Conv (Movzx ...) *)
	    let (dst, src0) = read_rm_operands objf b2 in
	    let translate_genop genop = 
	      match genop with
		Reg Esp -> (Reg Eax,RPh)
	      |	Reg Ebp -> (Reg Ecx,RPh)
	      |	Reg Edi -> (Reg Ebx,RPh)
	      |	Reg Esi -> (Reg Edx,RPh)
	      |	Reg r   -> (Reg r  ,RPl)
	      |	_ ->       (genop  ,RPl) in
	    let reg_part1 = if !mode_32 then RPe else RPx in
	    let (src,reg_part2) = translate_genop src0 in
	    Movpart (true, dst, reg_part1, src, reg_part2)
	       (* R *)
	| 0x31 -> Rdtsc
	       (* S *)
					(* Setcc *)
	| 0x90 | 0x91 | 0x92 | 0x93 | 0x94 | 0x95 | 0x96 | 0x97
	| 0x98 | 0x99 | 0x9A | 0x9B | 0x9C | 0x9D | 0x9E | 0x9F ->    
	    assert_32 objf;
	    let cond = decode_condition (b2 land 0xF) in
	    let (dummy,op) = read_rm_operand objf in
	    if dummy <> 0 then (warn objf "Setcc: bad format");
	    Setcc (cond, op)

	| 0xA5 ->			(* Shld rm, reg, None *)
	    assert_32 objf;
	    let (regcode,rm) = read_rm_operand objf in
	    Shld (rm, (decode_register regcode), None)
	| 0xA4 ->			(* Shld rm, reg, Some amt *)
	    assert_32 objf;
	    let (regcode,rm) = read_rm_operand objf in
	    let shiftval = read_num_of_length objf 1 in
	    Shld (rm, (decode_register regcode), (Some shiftval))
	       
	| 0xAD ->			(* Shrd rm, reg, None *)
	    assert_32 objf;
	    let (regcode,rm) = read_rm_operand objf in
	    Shrd (rm, (decode_register regcode), None)
	| 0xAC ->			(* Shrd rm, reg, Some amt *)
	    assert_32 objf;
	    let (regcode,rm) = read_rm_operand objf in
	    let shiftval = read_num_of_length objf 1 in
	    Shrd (rm, (decode_register regcode), (Some shiftval))
	| 0xAF ->
	    assert_32 objf;
	    let (dstregcode,srcop) = read_rm_operand objf in
	    ArithBin(Imul2,Reg (decode_register dstregcode),srcop)
	| b -> 
	    fail objf ("Unknown 2-byte instruction code: "^(string_of_int b))
	      
    end (* End two-byte instructions *)


    | 0x80 | 0x81 | 0x82 | 0x83 ->	(* Immed group 1 *)
	group_1 objf b1
    | 0xD2 | 0xD3 | 0xC0 | 0xC1 ->	(* Shift group 2 *)
	group_2 objf b1
    | 0xF6 | 0xF7 ->			(* Unary group 3 *)
	group_3 objf b1
    | 0xFE | 0xFF ->			(* Inc/Dec group 4&5 *)
	group_5 objf b1

    (* Floating Point Instructions *)
    | 0xD8 -> floatD8 objf
    | 0xD9 -> floatD9 objf
    | 0xDA -> floatDA objf
    | 0xDB -> floatDB objf
    | 0xDC -> floatDC objf
    | 0xDD -> floatDD objf
    | 0xDE -> floatDE objf
    | 0xDF -> floatDF objf
    | 0x9B -> float9B objf

    | b -> 
	fail objf ("Unknown instruction code"^(string_of_int b))
;;
	 
let rec get_n_instrs objf n = 
   match n with 
      0 -> []
    | _ -> (get_instr objf) :: (get_n_instrs objf (n-1))
;;

