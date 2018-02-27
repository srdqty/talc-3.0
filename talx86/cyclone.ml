
(* cyclone.ml
   Contains macros used by Cyclone. *)

open Numtypes
open Tal
open Identifier

let fail s = 
  (Printf.eprintf "cyclone: %s\n" s; flush stderr; raise Gcdfec.Exit)

(* Result register will be clobbered.  fun_call should not clobber any
   callee_save registers (ie EBX, ESI, EDI, EBP, and restore ESP).
*)
let preserve_regs result_reg fun_call =
  let save = 
    match result_reg with
      Some Eax -> [     Ecx; Edx]
    | Some Ecx -> [Eax;      Edx]
    | Some Edx -> [Eax; Ecx     ]
    | _        -> [Eax; Ecx; Edx] in (* All other regs are callee-save. *)
  let pop = List.map (fun x -> Pop (Reg x)) save in
  let push = List.rev (List.map (fun x -> Push (Reg x, [])) save) in
  let mov_result = 
    match result_reg with
      Some Eax -> []
    | None     -> []
    | Some reg -> [Mov (Reg reg, (Reg Eax, []))] in
  (push @ fun_call @ mov_result @ pop)

let match_preserve_regs match_fun_call insts =
  let match_mov_result insts = 
    match insts with
    | Mov (Reg r, (Reg Eax, [])) :: tl -> (Some r,tl)
    | _ -> (None,insts) in (* Could be EAX too? *)
  let rec match_pop regs insts =
    match regs,insts with
    | r :: rtl,Pop (Reg r2) :: ptl when r=r2 ->  match_pop rtl ptl
    | [],_ -> insts 
    | _,_ -> fail "preserve_regs: Match pop failed." in
  let rec match_push i regs insts =
    match insts with
    | Push (Reg x,[]) :: tl when i<3 -> match_push (i+1) (x::regs) tl
    | _ -> 
	let (x,insts) = match_fun_call insts in
	let (y,insts) = match_mov_result insts in
	let    insts  = match_pop regs insts in
	(x,y,insts)
  in
  match_push 0 [] insts
;;
      

(* If we clobber a callee-save register then save that too. *)
let save_clobbered_reg reg instrs = 
  match reg with
    Eax | Ecx | Edx -> instrs (* Already saved. *)
  | _ -> ([Push (Reg reg,[])] @ instrs @ [Pop (Reg reg)])
    
let match_save_clobbered_reg match_rest insts =
  match insts with
  | Push (Reg r,[]) :: tl -> 
      let (x,insts) = match_rest tl in
      (match insts with
      |	Pop (Reg r2) :: tl when r=r2-> ((Some r,x),tl)
      |	_ -> fail "save_clobbered_reg: Missing pop in macro.")
  | _ -> 
      let (x,insts) = match_rest insts in
      ((None,x),insts)
;;

(***** Label Arithmetic *****)

(* assert_template_labels
   Should raise an error if any label in the list is _not_ in the
   template section. For now, does nothing. *)

let assert_template_labels env label_list =
   () (**)(* Unimplemented *)

(* Labels of helper C functions defined in runtime/cyclonelib.c *)
let cg_start_label = (id_of_string "_CG_start")
let cg_dump_label = (id_of_string "_CG_dump")
let cg_end_label = (id_of_string "_CG_end")
let cg_mark_label = (id_of_string "_CG_mark")
let external_addr = (id_of_string "_external_addr")

let cyclone_labels = [ (cg_start_label, cempty);
			(cg_dump_label, cempty);
			(cg_end_label, cempty);
			(cg_mark_label, cempty);
			(external_addr, cempty) ]

(***** Macros *****)


(* CGSTART tv, con
   Allocate a new code generation region and return it in EAX.
   The arguments tv and con are used by the verifier only. *)

let cgstart_macro tv con =
  preserve_regs (Some Eax) [ Mov (Reg Eax, (Addr cg_start_label, []));
			     Call (Reg Eax, [])  ]


let match_cgstart insts =
  let match_start insts =
    match insts with
    | Mov (Reg Eax, (Addr cgl, [])) ::
      Call (Reg Eax, []) :: tl ->
	if id_compare cgl cg_start_label = 0 then ((),tl) else
	fail "bad cgStart" 
    | _ -> fail "bad cgStart(2)" in
  let ((),ro,insts) = match_preserve_regs match_start insts in
  match ro with
  | None -> insts 
  | Some Eax -> insts
  | _ -> fail "bad cgStart(2)"
;;


(* CGDUMP cgReg, tv, reg, lab

   Dump the template at label lab into the code generation region
   given by the ref cell cgReg.  The offset from the beginning of the
   code in the cg region to the beginning of the copied template is
   put into register reg.  The type variable tv is used by the
   verifier only; it is a BINDING OCCURRENCE. *)

let cgdump_macro cgReg tv reg lab =
  preserve_regs (Some reg) 
    [ Push (Addr lab, []);
      Push (Reg cgReg,[]);
      Mov (Reg Eax, (Addr cg_dump_label, []));
      Call (Reg Eax, []);
      ArithBin (Add, (Reg Esp), (Immed i32_8))  ]
    
let match_cgdump lab' insts =
  let match_dump insts =
    match insts with
    | Push (Addr lab, [])::
      Push (Reg cgReg,[])::
      Mov (Reg Eax, (Addr cdl,[]))::
      Call (Reg Eax, [])::
      ArithBin (Add, Reg Esp, Immed i8) ::
      tl ->
	if (id_compare cdl cg_dump_label = 0 && 
	    i8 =$ i32_8 &&
	    id_compare lab' lab = 0) then (cgReg,tl) else
	  fail "Bad constants in cgDump macro."
    | _ -> fail "Bad instantiation of cgDump macro."
  in
  let (cgReg,result_reg,insts) = match_preserve_regs match_dump insts in
  match result_reg with 
  | None ->   (cgReg,Eax,insts)
  | Some r -> (cgReg,  r,insts)
;;


(* CGFILL cgReg, reg1, tmplLab, holeLab, reg2

   cgReg is a register pointing to a region ref,
   reg1 is a register holding the offset of a copy of a template,
   tmplLab is the label of the original template,
   holeLab is the label of the hole of the original template,
   reg2 is a register containing the value to stuff into the hole
     of the copy *)

let cgfill_macro cgReg reg1 tmplLab holeLab reg2 =
   let disp = (holeLab -$ tmplLab -$ i32_3) in
   preserve_regs None 
     (save_clobbered_reg reg1 
	[ ArithBin (Add, (Reg reg1), (Immed disp));
	  Push (Reg reg1, []);
	  Push (Reg reg2, []);
	  Push (Reg cgReg,[]);
	  Mov (Reg Eax, (Addr cg_mark_label, []));
	  Call (Reg Eax, []);
	  ArithBin (Add, (Reg Esp), (Immed (int_to_int32 12)))  ])


let match_cgfill tLoc hLoc insts =
  let fail x = fail ("Bad instantiation of cgFill macro" ^ x) in
  let disp' = (hLoc -$ tLoc -$ i32_3) in
  let match_fill insts =
    match insts with
    | ArithBin (Add, (Reg r1), (Immed disp)) ::
      Push (Reg r1', [])::
      Push (Reg r2, []) ::
      Push (Reg cgReg,[])::
      Mov (Reg Eax, (Addr cgl,[]))::
      Call (Reg Eax, [])::
      ArithBin(Add, Reg Esp, Immed i) :: 
      tl ->
	if not (disp =$ disp') then 
       
	  (let i = int32_to_int in
	  Printf.eprintf "disp = %d, disp' = %d\n" (i disp) (i disp');
	  Printf.eprintf "hLoc = %d, tLoc = %d\n" (i hLoc) (i tLoc); 
	  fail "2") else 
	if r1 <> r1' then fail "3" else
	if id_compare cgl cg_mark_label <> 0 then fail "4" else
	if not (i =$ (int_to_int32 12)) then fail "5" else 
	
	((cgReg,r1,r2),tl)
    | _ -> fail "0"
  in
  let ((r1o,(cgReg,r1,r2)),preserved_reg,insts) = 
    match_preserve_regs (match_save_clobbered_reg match_fill) insts in
  (match r1o with
  | Some r1' when r1' = r1 -> ()
  | None when r1 = Eax || r1 = Ecx || r1 = Edx -> ()
  | _ -> fail "1");
  if preserved_reg <> None then fail "2" else 
  (cgReg,r1,r2,insts)
    ;;
    
(* CGFILLJMP cgReg, holeReg, htmpLab, holeLab, targReg, ttmpLab, targLab

   The macro is used to fill the hole of an inter-template jump.

   cgReg is a register pointing to a region ref.
   holeReg is a register holding the offset of the copy of the
     template containing the hole to fill.
   htmpLab is the label of the original template with the hole.
   holeLab is the label of the hole in the original template.    
   targReg is a register holding the offset of the copy of the
     template containing the target.
   ttmpLab is the label of the original template with the target.
   targLab is the label of the target in the original template.

   The offset of the hole is calculated as follows:

     holeReg+(holeLab+1-htmpLab-4)=holeReg+(holeLab-htmpLab-3)

   1 is added for the JMP opcode, 4 is subtracted due to template length.

   The target address is calculated as follows:

     (targReg-holeReg)+(targLab-ttmpLab-4)-(holeLab-htmpLab-4+1+4)=
     (targReg-holeReg)+(targLab-ttmpLab)-(holeLab-htmpLab)-5

   4 is subtracted from each template due to template length.  Adding 1
   and 4 are due to the JMP opcode and address. *)


let cgfilljmp_macro cgReg holeReg htmpLab holeLab targReg ttmpLab targLab =
   [  (* Save the offset to the template copy with the target *)
      Push (Reg targReg, []);

      (* Put the fill value into targReg *)
      ArithBin (Sub, Reg targReg, Reg holeReg);
      ArithBin (Add,
                Reg targReg,
		Immed ((targLab -$ ttmpLab)
			 -$ (holeLab -$ htmpLab) -$ (int_to_int32 5)));

      (* Save the offset to the template copy with the hole *)
      Push (Reg holeReg, []);

      (* Get a *pointer* to the template copy with the hole *)
      ArithBin(Add,  
               Reg holeReg,
               Prjr((cgReg,[]),
                    i32_0,
                    None));
      ArithBin(Add,           (* This skips over fills, length, current *)
               Reg holeReg,
               Immed (int_to_int32 12));
   
      (* Fill the hole *)
      Mov (Prjr ((holeReg,[]),(holeLab -$ htmpLab -$ i32_3),None),
	     (Reg targReg,[]));

      (* Restore the offset to the template copy with the hole *)
      Pop (Reg holeReg);

      (* Restore the offset to the template copy with the target *)
      Pop (Reg targReg)
   ]

let match_cgfilljmp htLoc hLoc ttLoc tLoc insts =
  let fail x = fail ("Bad instantiation of cgFillJmp." ^x) in
  let c1' = (tLoc -$ ttLoc) -$ (hLoc -$ htLoc) -$ (int_to_int32 5) in
  let c4' = hLoc -$ htLoc -$ i32_3 in 
  match insts with
  | Push (Reg tr, []) ::
    ArithBin (Sub, Reg tr1, Reg hr)::
    ArithBin (Add, Reg tr2, Immed c1) ::
    Push (Reg hr1,[])::
    ArithBin (Add,Reg hr2,Prjr((cgReg,[]),c2,None))::
    ArithBin(Add,Reg hr3, Immed c3)::
    Mov (Prjr ((hr4,[]),c4,None), (Reg tr3,[])) ::
    Pop (Reg hr5) ::
    Pop (Reg tr4) :: 
    tl ->
      if tr <> tr1 || tr <> tr2 || tr <> tr3 || tr <> tr4 then fail "0" else
      if hr <> hr1 || hr <> hr2 || hr <> hr3 || hr <> hr4 || hr <> hr5 then
	fail "1" else
	if not (c1 =$ c1') then fail "2" else
	if not (c2 =$ i32_0) then fail "3" else
	if not (c3 =$ (int_to_int32 12)) then fail "4" else
	if not (c4 =$ c4') then fail "5" else
	(cgReg,hr,tr,tl)
  | _ -> fail "0"

(*
* CGFILLJCC holeReg, htmplab, holeLab, targReg, ttmpLab, targLab          *
*								          *
* This is the same as CGFILLMP, except a jcc instr filled in.           *

   cgReg is a register pointing to a region ref.
   holeReg is a register holding the offset of the copy of the
     template containing the hole to fill.
   htmpLab is the label of the original template with the hole.
   holeLab is the label of the hole in the original template.    
   targReg is a register holding the offset of the copy of the
     template containing the target.
   ttmpLab is the label of the original template with the target.
   targLab is the label of the target in the original template.

   The offset of the hole is calculated as follows:

     holeReg+(holeLab+2-htmpLab-4)=holeReg+(holeLab-htmpLab-2)

   2 is added for the JCC opcode, 4 is subtracted due to template length.

   The target address is calculated as follows:

     (targReg-holeReg)+(targLab-ttmpLab-4)-(holeLab-htmpLab-4+2+4)=
     (targReg-holeReg)+(targLab-ttmpLab)-(holeLab-htmpLab)-6

   4 is subtracted from each template due to template length.  Adding 2
   and 4 are due to the JCC opcode and address. *)

let cgfilljcc_macro cgReg holeReg htmpLab holeLab targReg ttmpLab targLab =
   [  (* Save the offset to the template copy with the target *)
      Push (Reg targReg, []);

      (* Put the fill value into targReg *)
      ArithBin (Sub, Reg targReg, Reg holeReg);
      ArithBin (Add,
                Reg targReg,
		Immed ((targLab -$ ttmpLab)
			 -$ (holeLab -$ htmpLab) -$ (int_to_int32 6)));

      (* Save the offset to the template copy with the hole *)
      Push (Reg holeReg, []);

      (* Get a *pointer* to the template copy with the hole *)
      ArithBin(Add,  
               Reg holeReg,
               Prjr((cgReg,[]),
                    i32_0,
                    None));
      ArithBin(Add,           (* This skips over fills, length, current *)
               Reg holeReg,
               Immed (int_to_int32 12));
   
      (* Fill the hole *)
      Mov (Prjr ((holeReg,[]),(holeLab -$ htmpLab -$ i32_2),None),
	     (Reg targReg,[]));

      (* Restore the offset to the template copy with the hole *)
      Pop (Reg holeReg);

      (* Restore the offset to the template copy with the target *)
      Pop (Reg targReg)
   ]

let match_cgfilljcc htLoc hLoc ttLoc tLoc insts =
  let fail x = fail ("Bad instantiation of cgFillJcc. " ^x) in
  let c1' = (tLoc -$ ttLoc) -$ (hLoc -$ htLoc) -$ (int_to_int32 6) in
  let c4' = (hLoc -$ htLoc -$ i32_2) in
  match insts with
  | Push (Reg tr,[])::
    ArithBin (Sub, Reg tr1, Reg hr)::
    ArithBin (Add, Reg tr2, Immed c1)::
    Push (Reg hr1, [])::
    ArithBin(Add, Reg hr2, Prjr((cgReg,[]),c2,None))::
    ArithBin(Add, Reg hr3, Immed c3)::
    Mov (Prjr ((hr4,[]),c4,None), (Reg tr3,[]))::
    Pop (Reg hr5)::
    Pop (Reg tr4) :: 
    tl ->
      if tr<>tr1 || tr<>tr2 || tr<>tr3 || tr<>tr4 then fail "1" else
      if hr<>hr1 || hr<>hr2 || hr<>hr3 || hr<>hr4 || hr<>hr5 then fail "2" else
      if not (c1 =$ c1') then fail "3" else
      if not (c2 =$ i32_0) then fail "4" else
      if not (c3 =$ (int_to_int32 12)) then fail "5" else
      if not (c4 =$ c4') then fail "6" else 
      (cgReg,hr,tr,tl)
  | _ -> fail "0"
	
(* CGEND reg

   Finish code generation for the region in reg: leave a pointer to
   the resulting function in register reg. *)
let cgend_macro reg =
  preserve_regs (Some reg)
    [ Push (Reg reg, []);
      Mov (Reg Eax, (Addr cg_end_label, []));
      Call (Reg Eax, []);
      ArithBin (Add, (Reg Esp), (Immed i32_4))  ]

(* The simple approach doesn't work because match_preserve_regs will eagerly
   match the first push in the macro body although it shouldn't.
*)
let match_cgend insts =
  let fail x = fail ("Bad instantiation of cgEnd." ^ x) in
  let rec last_push insts aux =
    match insts with
    | (Push (Reg _, []) as hd) :: 
      (Push (Reg _, []) :: _ as tl) -> (last_push tl (hd::aux))
    | Push (Reg r, []) :: tl -> (r,List.rev_append aux tl)
    | _ -> fail "0" in
  let (r,insts) = last_push insts [] in
  let match_end insts = 
    match insts with
    | Mov (Reg Eax, (Addr cel, [])) ::
      Call (Reg Eax, []) ::
      ArithBin (Add, Reg Esp, Immed i) :: tl ->
	if id_compare cel cg_end_label = 0 && i =$ i32_4 then ((),tl)
	else fail "1"
    | _ -> fail "2" in
  let (_,r2_opt,insts) = match_preserve_regs match_end insts in
  (match r,r2_opt with
  | Eax,None -> ()
  | _,Some r' when r=r' -> () 
  | _,_ -> fail "3");
  (r,insts)
;;

(* CGHOLE reg, tmplLab, holeLab			

   Declares a new hole, holeLab, in the template whose first
   instruction has label tmplLab.  The instruction will move the
   contents of the hole into register reg when the hole has been
   filled.

   holeLab is a BINDING OCCURRENCE. *)
(* XXX: eliminate the tmplLab argument, which appears for historical
   reasons *)

let cghole_macro reg tmplLab holeLab =
   (**) (* Bind holeLab to this instruction. *)
   (* XXX: it would be nicer to bind it to the location of the hole *)
   [ Mov (Reg reg, (Immed i32_0,[])) ]

let match_cghole insts =
  match insts with
  | Mov (Reg r, (Immed i,[])) :: tl  -> 
      if not (i =$ i32_0) then (Printf.eprintf "i = %d\n" (int32_to_int i);
				fail "Bad cgHole 1") else
	(r,tl)
  | hd::tl -> (print_string "Hole Instruction: ";
	       Talpp.print_instruction Format.std_formatter Talpp.std_options hd;
	       Format.print_newline(); 
	       fail "Bad cgHole 2")
  | [] -> fail "Bad cgHole 3"

(* CGHOLEJMP tmplLab, holeLab
 
   Same as CGHOLE, except the instruction with a hole is a JMP
   instruction instead of a MOV instruction. *)
(* XXX: eliminate the tmplLab argument, which appears for historical
   reasons *)

let cgholejmp_macro tmplLab holeLab =
   (**)(* Bind holeLab to this instr. *) 
   (* XXX: it would be nicer to bind it to the location of the hole *)
  [ Jmp (Addr external_addr, []) ]

let match_cgholejmp insts =
  match insts with
    Jmp (Addr ea,[]) ::tl ->
      if ea = external_addr then tl 
      else fail "bad cgHoleJmp(2)"
  | _ -> fail "bad cgHoleJmp"

(* CGHOLEJCC cc, tmplLab, holeLab
 
   Same as CGHOLE, except the instruction with a hole is a JCC
   instruction instead of a MOV instruction. *)
(* XXX: eliminate the tmplLab argument, which appears for historical
   reasons *)

let cgholejcc_macro cc tmplLab holeLab =
   (**)(* Bind holeLab to this instr *)
   (* XXX: it would be nicer to bind it to the location of the hole *)
   [ Jcc (cc, (external_addr,[]), None) ]

let match_cgholejcc insts =
  match insts with
  | Jcc (cc, (ea,[]), None):: 
    tl ->
      if ea = external_addr then (cc,tl)
      else fail "Bad cgHoleJcc(2)"
  | _ -> fail "Bad cgHoleJcc"
