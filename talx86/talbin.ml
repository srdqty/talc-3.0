(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, David Walker, Dan Grossman          *)
(*     August 1999, all rights reserved.                              *)
(**********************************************************************)
(* talbin.ml 
 *
 * Defines annotations for the binary representation of tal files and
 * operations for splitting out and merging annotations.  
 *)
open Utilities;;
open Numtypes;;
open Identifier;;
open Tal;;

let fail s = 
  (Printf.eprintf "talbin: %s\n" s; flush stderr; raise Gcdfec.Exit)
;;

(* Most of these should be self-explanatory except for the An_opi
 * constructors -- read the source for these to see how they attach
 * themselves to genops.  
 *)
type inst_annot = 
    An_none of int (* no annotation for next i instrs.  1 <= i <= 255 *)
  | An_op1 of coercion list 
  | An_op2 of coercion list * coercion list
  | An_op3 of coercion list * coercion list * coercion list
  | An_op4 of coercion list * coercion list * coercion list * coercion list
  | An_jcc of coercion list * inst_annot list
  | An_coerce of genop * coercion list
  | An_coercename of identifier * coercion list
  | An_fallthru of con list
  | An_malloc of identifier * (mallocarg option)
  | An_proof of (identifier * con list) list
  | An_unpacknomove of identifier * reg * coercion list
  | An_unpackmove of identifier * coercion list * coercion list
  | An_sunpack of identifier * genop
  | An_nameobj of identifier * genop
  | An_forgetunique of identifier
  | An_removename of identifier
(* LX *)
  | An_vcase of int32 * con * identifier * genop coerce 
  | An_letprod of identifier list * con
  | An_letroll of identifier * con 
(* end LX *)
(* Cyclone *)
  | An_cgstart of identifier * con
  | An_cgdump of identifier * identifier
  | An_cghole of identifier * identifier
  | An_cgholejmp of identifier * identifier coerce
  | An_cgholejcc of identifier * identifier coerce * inst_annot list
  | An_cgfill of identifier * identifier
  | An_cgfilljmp of identifier * identifier * identifier * identifier
  | An_cgfilljcc of identifier * identifier * identifier * identifier
  | An_cgforget of identifier * identifier
  | An_cgend
(* end Cyclone *)
type arep_item = An_con | An_kind | An_label
type data_annot = 
    An_dlabel of coercion list
  | An_dbytes of int    (* number of bytes *)
  | An_d2bytes
  | An_d4bytes of coercion list
  | An_dfloat32
  | An_dfloat64
  | An_djunk
  | An_dup
  | An_ddown
  | An_drep of arep_item
;;

exception Found_Hole of (inst_annot list * instruction list)

let print_inst_annot ia = 
  match ia with
    An_none i -> print_string "An_none "; print_int i
  | An_op1 _ -> print_string "An_op1"
  | An_op2 (_,_) -> print_string "An_op2"
  | An_op3 (_,_,_) -> print_string "An_op3"
  | An_op4 (_,_,_,_) -> print_string "An_op4"
  | An_jcc (_,_) -> print_string "An_jcc"
  | An_coerce _ -> print_string "An_coerce"
  | An_coercename _ -> print_string "An_coercename"
  | An_fallthru _ -> print_string "An_fallthru"
  | An_malloc _ -> print_string "An_malloc"
  | An_proof _ -> print_string "An_proof"
  | An_unpacknomove _ -> print_string "An_unpacknomove"
  | An_unpackmove _ -> print_string "An_unpackmove"
  | An_sunpack _ -> print_string "An_sunpack"
  | An_nameobj _ -> print_string "An_nameobj"
  | An_forgetunique _ -> print_string "An_forgetunique"
  | An_removename _ -> print_string "An_removename"
  (* LX *)
  | An_vcase _ -> print_string "An_vcase"
  | An_letprod _ -> print_string "An_letprod"
  | An_letroll _ -> print_string "An_letroll"
(* end LX *)
(* Cyclone *)
  | An_cgstart   _ -> print_string "An_cgstart"
  | An_cgdump    _ -> print_string "An_cgdump"
  | An_cghole    _ -> print_string "An_cghole"
  | An_cgholejmp _ -> print_string "An_cgholejmp"
  | An_cgholejcc _ -> print_string "An_cgholejcc"
  | An_cgfill    _ -> print_string "An_cgfill"
  | An_cgfilljmp _ -> print_string "An_cgfilljmp"
  | An_cgfilljcc _ -> print_string "An_cgfilljcc"
  | An_cgforget  _ -> print_string "An_cgforget"
  | An_cgend       -> print_string "An_cgend"
(* end Cyclone *)

;;

let noannot = An_none 1;;

(* return an annotation for an instruction *)
let rec inst_annot label_map i = 
  let gop_inner g = 
    match g with
      Immed _ -> []
    | Reg _ -> []
    | Addr x -> if label_map x then [VirtLabel x] else [] 
    | Prjr ((r,cs),_,_) -> cs
    | Prjl ((x,cs),_,_) -> if label_map x then (VirtLabel x :: cs) else cs in
  let unop gop = 
    match gop_inner gop with
      [] -> noannot
    | cs -> An_op1 cs in
  let cunop gop cs =
    match gop_inner gop,cs with
      [],[] -> noannot
    | [],cs -> An_op1 cs
    | cs1,cs2 -> An_op2(cs1,cs2) in
  let binop gop1 gop2 = 
    match gop_inner gop1, gop_inner gop2 with
      [],[] -> noannot
    | [],cs -> An_op1 cs
    | c1s,c2s -> An_op2(c1s,c2s) in
  let coerces c1 c2 c3 c4 =
    match c1,c2,c3,c4 with
      [],[],[],[] -> noannot
    | [],[],[],cs -> An_op1 cs
    | [],[],cs1,cs2 -> An_op2(cs1,cs2)
    | [],cs1,cs2,cs3 -> An_op3(cs1,cs2,cs3)
    | cs1,cs2,cs3,cs4 -> An_op4(cs1,cs2,cs3,cs4) in
  let cbinop gop1 c1 gop2 c2 = coerces (gop_inner gop1) c1 (gop_inner gop2) c2 
  in
  match i with
    ArithBin(_,gop1,gop2) -> binop gop1 gop2
  | ArithUn(_,gop) -> unop gop
  | ArithMD(_,gop) -> unop gop
  | ArithSR(_,gop,_) -> unop gop
  | Bswap _ -> noannot
  | Call (g,cs) -> cunop g cs
  | Clc -> noannot
  | Cmc -> noannot
  | Cmovcc (_,_,(g,c)) -> cunop g c
  | Cmp((g1,c1),(g2,c2)) -> cbinop g1 c1 g2 c2
  | Conv _ -> noannot
  | Imul3(_,g,_) -> unop g
  | Int _ -> noannot
  | Into -> noannot
  | Jcc(_,(x,c),iopt) -> 
      let annots = 
	match iopt with 
	  None -> [] 
	| Some is -> List.map (inst_annot label_map) is in
      (match c,annots,label_map x with
	[],[],false -> noannot 
      |	_,_,false -> An_jcc(c,annots)
      |	_,_,_ -> An_jcc(VirtLabel(x)::c,annots))
  | Jecxz((x,c),iopt) ->
      let annots = 
	match iopt with 
	  None -> [] 
	| Some is -> List.map (inst_annot label_map) is in
      (match c,annots,label_map x with
	[],[],false -> noannot 
      |	_,_,false -> An_jcc(c,annots)
      |	_,_,_ -> An_jcc(VirtLabel(x)::c,annots))
  | Jmp(g,c) -> cunop g c
  | Lahf -> noannot
  | Lea(_,g) -> unop g
  | Loopd((x,c),_) -> 
      (match c,label_map x with 
	[],false -> noannot 
      |	_,false -> An_op1 c
      |	_,_ -> An_op1 (VirtLabel(x)::c))
    (* JGM:  note that the first list of coercions is empty guaranteeing
       * that we cannot get an An_op4 annotation. *)
  | Mov(g1,(g2,c)) -> coerces [] (gop_inner g1) (gop_inner g2) c
  | Movpart(_,g1,_,g2,_) -> binop g1 g2
  | Nop -> noannot
  | Pop g -> unop g
  | Popad -> noannot
  | Popfd -> noannot
  | Push(g,c) -> cunop g c
  | Pushad -> noannot
  | Pushfd -> noannot
  | Rdtsc -> noannot
  | Retn _ -> noannot
  | Sahf -> noannot
  | Setcc(_,g) -> unop g
  | Shld(g,_,_) -> unop g
  | Shrd(g,_,_) -> unop g
  | Stc -> noannot
  | Test(g1,g2) -> binop g1 g2
  | Xchg(g,_) -> unop g
  | Coerce(g,c) -> An_coerce(g,c)
  | CoerceName(x,c) -> An_coercename(x,c)
  | Comment _ -> noannot
  | Fallthru(cs) -> An_fallthru(cs)
  | Malloc(x,_,mopt) -> An_malloc(x,mopt)
  | Proof xs -> An_proof xs
  | Unpack(x,r,(gop,c)) ->
      begin
	let a = An_unpackmove(x,gop_inner gop,c) in
	match gop with
	  Reg r' -> 
	    if r' = r then An_unpacknomove(x,r,c) else a
	| _ -> a
      end
  | Sunpack(i,g) -> An_sunpack(i,g)
  | Nameobj(x,g) -> An_nameobj(x,g)
  | ForgetUnique x -> An_forgetunique x
  | RemoveName x -> An_removename x
(* Floating Point *)
  | FPnoargs _ -> noannot
  | FPsomeargs (_,args) -> 
      (match args with
	FPstack _ | FPstack2 _ -> noannot
      |	FPgenop (_,g) -> unop g)
(* LX *)	  
  | Vcase (i,c,id,gc) -> An_vcase (i,c,id,gc)
  | Letroll (i,c) -> An_letroll (i,c)
  | Letprod (l,c) -> An_letprod(l,c)
(* end LX *)
(* Cyclone *)
  | CgStart(i,c)                    -> An_cgstart(i,c)
  | CgDump(r1,i1,r2,i2)             -> An_cgdump(i1,i2)
  | CgHole(r,i1,i2)                 -> An_cghole(i1,i2)
  | CgHoleJmp(i1,ic)                -> An_cgholejmp(i1,ic)
  | CgHoleJcc(c,i1,ic,iopt)              -> 
      let annots = 
	match iopt with 
	| None -> [] 
	| Some is -> List.map (inst_annot label_map) is in
      An_cgholejcc(i1,ic,annots)
  | CgFill(r0,r1,i1,i2,r2)          -> An_cgfill(i1,i2)
  | CgFillJmp(r0,r1,i1,i2,r2,i3,i4) -> An_cgfilljmp(i1,i2,i3,i4)
  | CgFillJcc(r0,r1,i1,i2,r2,i3,i4) -> An_cgfilljcc(i1,i2,i3,i4)
  | CgForget(i1,i2)                 -> An_cgforget(i1,i2)
  | CgEnd _                         -> An_cgend
(* end Cyclone *)
(*  | _ -> fail "unimplemented instruction" *)
;;

let found_hole = ref false;; (* used to ensure every hole has a label! *)

(* given a list of instruction annotations and a list of instructions,
 * merge them to produce a list of annotated instructions.  *)
let merge_inst_annots objf annots instrs = 
  let shared_label_dict = objf.Disobjfile.obj_get_shared_labels () in
  let labelmap x = 
    try
      Dict.lookup shared_label_dict x
    with Dict.Absent -> 
	begin 
	  Printf.eprintf "Label map: \n";
	  Dict.app_dict 
	    (fun l ls -> (Printf.eprintf "%s: " (id_to_string l);
			  List.iter (fun x->Printf.eprintf "%s " (id_to_string x)) ls;
			  Printf.eprintf("\n"))) 
	    shared_label_dict;
	  flush stderr;
	  fail ("Label " ^ (id_to_string x)^" is not in labelmap\n")
	end in
  (* replace a label with the intended one by merging away the VirtLabel
     coercion *)
  let merge_virtlabel a_found a_desired =
    if List.exists (fun x -> id_compare x a_desired = 0) (labelmap a_found) 
    then a_desired
    else fail "VirtAddress not shared." in
(*
    try List.nth (labelmap a_found) i
    with Dict.Absent -> 
(*      print_string("\n\t");
      print_string(id_to_string a);
*)
      fail "VirtAddress on non-shared address?"
    | _ ->
(*      List.iter (fun id -> print_string(id_to_string id)) (labelmap a);
      print_string " ";
      print_string (id_to_string a); print_string(" "); print_int i;
*)
      fail "merge_virtlabel: index too large"
  in
*)
  (* merge a coercion onto the inner operand of a memory genop *)
  let merge_inner g cs = 
    match cs,g with
      [],_ -> g
    | [VirtLabel(x)],Addr z -> Addr (merge_virtlabel z x)
    | (VirtLabel(x)::cs),(Prjl((z,_),i,sopt)) -> 
	Prjl((merge_virtlabel z x, cs),i,sopt)
    | _,Prjl((x,_),i,sopt) -> Prjl((x,cs),i,sopt)
    | _,Prjr((r,_),i,sopt) -> Prjr((r,cs),i,sopt)
    | _,Immed _ -> fail "merge_inner: Immed"
    | _,Reg   _ -> fail "merge_inner: Reg"
    | _,Addr  _ -> fail "merge_inner: Addr"
  in

  (* merge a single coercion with an instruction -- this is always the
   * right most, inner most genop *)    
  let merge_op1 instr cs = 
    match instr with
      ArithBin(ab,gop1,gop2) -> ArithBin(ab,gop1,merge_inner gop2 cs)
    | ArithUn(u,gop) -> ArithUn(u,merge_inner gop cs)
    | ArithMD(m,gop) -> ArithMD(m,merge_inner gop cs)
    | ArithSR(a,gop,x) -> ArithSR(a,merge_inner gop cs,x)
    | Call (g,_) -> Call(g,cs)
    | Cmovcc(x,y,(g,_)) -> Cmovcc(x,y,(g,cs))
    | Cmp(cg1,(g2,_)) -> Cmp(cg1,(g2,cs))
    | Imul3(x,g,y) -> Imul3(x,merge_inner g cs,y)
    | Jmp(g,_) -> Jmp(g,cs)
    | Lea(x,g) -> Lea(x,merge_inner g cs)
    | Loopd((x,_),w) -> 
      	(match cs with
	  VirtLabel(y)::cs -> 
	    Loopd((merge_virtlabel x y, cs),w)
      	| _ -> Loopd((x,cs),w))
    | Mov(g1,(g2,_)) -> Mov(g1,(g2,cs))
    | Movpart(x,g1,y,g2,z) -> Movpart(x,g1,y,merge_inner g2 cs,z)
    | Pop g -> Pop(merge_inner g cs)
    | Push(g,_) -> Push(g,cs)
    | Setcc(x,g) -> Setcc(x,merge_inner g cs)
    | Shld(g,x,y) -> Shld(merge_inner g cs,x,y)
    | Shrd(g,x,y) -> Shrd(merge_inner g cs,x,y)
    | Test(g1,g2) -> Test(g1,merge_inner g2 cs)
    | Xchg(g,x) -> Xchg(merge_inner g cs,x)
    | FPsomeargs(fp,FPgenop(sc,g)) -> FPsomeargs(fp,
						 FPgenop(sc,merge_inner g cs))
    | i -> 
      	(Talpp.print_instruction Format.std_formatter Talpp.std_options i;
	 Format.print_newline(); fail "merge_op1: bad instruction")
  in

  (* merge two coercions with an instruction -- either we have two genops
   * with inner coercions [binop] (isn't this impossible?) or we have a 
   * single coerced genop with an inner coercion [cunop] or we have
   * two genops with the right-most genop having both an inner and outer
   * coercion.
   *)
  let merge_op2 instr cs1 cs2 = 
    match instr with
      ArithBin(a,gop1,gop2) -> 
      	ArithBin(a,merge_inner gop1 cs1,merge_inner gop2 cs2)
    | Call(g,_) -> Call(merge_inner g cs1,cs2)
    | Cmovcc(x,y,(g,_)) -> Cmovcc(x,y,(merge_inner g cs1,cs2))
    | Cmp(cg1,(g2,_)) -> Cmp(cg1,(merge_inner g2 cs1,cs2))
    | Jmp(g,_) -> Jmp(merge_inner g cs1,cs2)
    | Mov(g1,(g2,_)) -> Mov(g1,(merge_inner g2 cs1,cs2))
    | Movpart(x,g1,y,g2,z) -> 
      	Movpart(x,merge_inner g1 cs1,y,merge_inner g2 cs2,z)
    | Push(g,_) -> Push(merge_inner g cs1,cs2)
    | Test(g1,g2) -> Test(merge_inner g1 cs1,merge_inner g2 cs2)
    | i -> 
      	(Talpp.print_instruction Format.std_formatter Talpp.std_options i;
	 Format.print_newline(); fail "merge_op2 bad instruction")
  in
  
  (* merge three coercions with an instruction -- this has to be a cbinop
   * so it must be Cmp or Mov. *)
  let merge_op3 instr cs1 cs2 cs3 = 
    match instr with
      Cmp((g1,_),(g2,_)) -> Cmp((g1,cs1),(merge_inner g2 cs2,cs3))
    | Mov(g1,(g2,_)) -> Mov(merge_inner g1 cs1,(merge_inner g2 cs2,cs3))
    | i ->  
      	(Talpp.print_instruction Format.std_formatter Talpp.std_options i;
	 Format.print_newline(); fail "merge_op3 bad instruction")
  in

  (* only cmp can generate this and only when the left-operand is a memory
   * operand with two coercions (guaranteeing that there's no innder coercion
   * for the right operand, but we don't encode this.)
   *)
  let merge_op4 instr cs1 cs2 cs3 cs4 = 
    match instr with
      Cmp((g1,_),(g2,_)) -> Cmp((merge_inner g1 cs1,cs2),
			      	(merge_inner g2 cs3,cs4))
    | _ -> 
      	(Talpp.print_instruction Format.std_formatter Talpp.std_options instr;
	 Format.print_newline(); fail "merge_op4 bad instruction")
  in

  let rec merge annots is accum =
    let label_offset i =
      try
	Dict.lookup objf.Disobjfile.obj_label_offset i
      with Dict.Absent ->
	(Printf.eprintf "Error looking up identifier: %s \n" (id_to_string i);
	 Printf.eprintf "Dictionary entries are:\n";
         Dict.app_dict
           (fun a b -> Printf.eprintf "%s\n" (id_to_string a))
           objf.Disobjfile.obj_label_offset;                
         flush stderr;
         fail "label lookup") in
    let cyc_check_hole h =
      if !found_hole then fail "cyc_check_hole called twice, no progress.\n";
      found_hole := true;
      let (sec,off) = label_offset h in
      let (sec_curr,off_curr) = objf.Disobjfile.obj_get_pos () in
      if sec = sec_curr && off = off_curr then () else
      (Printf.eprintf "Hole %s should be at (%d,%d), found at (%d,%d)\n"
	 (id_to_string h) sec off sec_curr off_curr;
       flush stderr;
       fail "cyc_check_hole: hole in wrong place.") in
(*
    if annots <> [] then (print_inst_annot (List.hd annots); 
			  print_string "\n") else ();
*)
    match annots, is with
      [],[] -> List.rev accum
    | (An_none n)::rest,is -> 
	if n < 0 then fail ("An_none "^string_of_int(n)) else
	let rec loop n is accum = 
	  match n,is with
	    0,_ -> merge rest is accum
	  | _,hd::tl -> loop (n-1) tl (hd::accum)
	  | _,[] -> fail ("An_none length mismatch")
	in loop n is accum
    | (An_op1(cs))::rest,i::is -> 
	merge rest is ((merge_op1 i cs)::accum)
    | (An_op2(cs1,cs2))::rest,i::is ->
	merge rest is ((merge_op2 i cs1 cs2)::accum)
    | (An_op3(cs1,cs2,cs3))::rest,i::is ->
	merge rest is ((merge_op3 i cs1 cs2 cs3)::accum)
    | (An_op4(cs1,cs2,cs3,cs4))::rest,i::is ->
	merge rest is ((merge_op4 i cs1 cs2 cs3 cs4)::accum)
    | (An_jcc(cs,annots2))::annots,(Jcc(cc,(x,_),_))::is ->
	let (id,cs) = 
	  (match cs with
	    (VirtLabel(y)::cs) -> (merge_virtlabel x y, cs)
	  | _ -> (x,cs)) in
	(match merge annots2 [] [] with
	  [] -> merge annots is ((Jcc(cc,(id,cs),None))::accum)
	| is2 -> merge annots is ((Jcc(cc,(id,cs),Some is2))::accum))
    | (An_jcc(cs,annots2))::annots,(Jecxz((x,_),_))::is ->
	let (id,cs) = 
	  (match cs with
	    (VirtLabel(y)::cs) -> (merge_virtlabel x y, cs)
	  | _ -> (x,cs)) in
	(match merge annots2 [] [] with
	  [] -> merge annots is ((Jecxz((id,cs),None))::accum)
	| is2 -> merge annots is ((Jecxz((id,cs),Some is2))::accum))
    | (An_coerce(g,cs))::rest,_ -> merge rest is (Coerce(g,cs)::accum)
    | (An_coercename(x,cs))::rest,_ ->
	merge rest is (CoerceName(x,cs)::accum)
    | (An_fallthru(cs))::rest,_ -> merge rest is (Fallthru(cs)::accum)
    | (An_malloc(x,mopt))::annots,
	(* NB:  this must match the malloc macro in talasmx86.ml *)
	(Push (Immed size,[]))::
	(Mov (Reg Eax, (Addr malloc_label, [])))::
	(Call (Reg Eax,[]))::
	(ArithBin (Add, Reg Esp,Immed i32))::
	(Test (Reg Eax, Reg Eax))::
	(Jcc (Eq, (out_of_memory_error_label, []), None))::insts ->
	  if id_compare malloc_label Objfile.gc_malloc_label = 0 &&
	     id_compare out_of_memory_error_label 
	       Objfile.out_of_memory_error_label = 0 &&
	    i32 = i32_4 then
	    merge annots insts ((Malloc(x,size,mopt))::accum)
	  else fail "merge: malloc -- bad instructions"
    | (An_proof(xs))::rest,_ -> merge rest is (Proof(xs)::accum)
    | (An_unpacknomove(x,r,cs))::rest,_ -> 
	merge rest is (Unpack(x,r,(Reg r,cs))::accum)
    | (An_unpackmove(x,cs1,cs2))::rest, (Mov (Reg r,(gop,_)))::insts ->
	merge rest insts ((Unpack(x,r,(merge_inner gop cs1,cs2)))::accum)
    | (An_sunpack(i,g))::rest,_    -> merge rest is (Sunpack(i,g)::accum)
    | (An_nameobj(x,g))::rest,_    -> merge rest is (Nameobj(x,g)::accum)
    | (An_forgetunique(x))::rest,_ -> merge rest is (ForgetUnique(x)::accum)
    | (An_removename(x))::rest,_   -> merge rest is (RemoveName(x)::accum)
(* LX *)
    | (An_vcase (i,c,id,gc))::rest,_ -> merge rest is (Vcase (i,c,id,gc)::accum)
    | (An_letprod (il,c))::rest,_    -> merge rest is (Letprod(il,c)::accum)
    | (An_letroll (i,c))::rest,_     -> merge rest is (Letroll (i,c)::accum)
(* end LX *)
(* CYCLONE *)
    | (An_cgstart(i,c))::rest,insts ->
	merge rest (Cyclone.match_cgstart insts) (CgStart (i,c)::accum)
    | (An_cgdump(i1,i2))::rest,insts ->
	let (cgReg,r,insts) = Cyclone.match_cgdump i2 insts in
	merge rest insts ((CgDump(cgReg,i1,r,i2))::accum)
    | (An_cghole(_,i2)) :: rest,[] -> 
	cyc_check_hole i2; raise (Found_Hole(annots,List.rev accum))
    | (An_cghole(i1,i2))::rest,insts ->
	if not (!found_hole) then fail "An_cghole 1" else
	found_hole := false;
	let (r,insts) = Cyclone.match_cghole insts in
	merge rest insts (CgHole (r,i1,i2)::accum)
    | (An_cgholejmp(i1,ic))::rest,[] ->
	cyc_check_hole (fst ic);
	raise (Found_Hole(annots,List.rev accum))
    | (An_cgholejmp(i1,ic))::rest,insts ->
	if not (!found_hole) then fail "An_cgholejmp 1" else
	found_hole := false;
	let insts = Cyclone.match_cgholejmp insts in
	merge rest insts (CgHoleJmp(i1,ic) :: accum)
    | (An_cgholejcc(i1,ic,iso))::rest,[] ->
	cyc_check_hole (fst ic);
	raise (Found_Hole(annots,List.rev accum))
    | (An_cgholejcc(i1,ic,iso))::rest,insts ->
	if not (!found_hole) then fail "An_cgholejcc 1" else
	found_hole := false;
	let (cc,insts) = Cyclone.match_cgholejcc insts in
	let annots = 
	  match merge iso [] [] with
	    [] -> None
	  | is2 -> Some is2 in
	merge rest insts (CgHoleJcc(cc,i1,ic,annots) :: accum)
    | (An_cgfill(i1,i2))::rest,insts ->
        let (sec1,off1) = label_offset i1 in
	let (sec2,off2) = label_offset i2 in
        let off1 = int_to_int32 off1      in
	let off2 = int_to_int32 off2      in
	if sec1 <> sec2 then fail "0" else 
	let (cgReg,r1,r2,insts) = 
	  Cyclone.match_cgfill off1 off2 insts in
	merge rest insts ((CgFill(cgReg,r1,i1,i2,r2))::accum)
    | (An_cgfilljmp(i1,i2,i3,i4))::rest,insts ->
	let (sec1,off1) = label_offset i1 in
	let (sec2,off2) = label_offset i2 in
	let (sec3,off3) = label_offset i3 in
	let (sec4,off4) = label_offset i4 in
	if sec1 <> sec3 || sec1 <> sec2 || sec1 <> sec4 
	then fail "merge: cgfilljmp -- bad section 1." else 
	let off1 = int_to_int32 off1 in
	let off2 = int_to_int32 off2 in
	let off3 = int_to_int32 off3 in
	let off4 = int_to_int32 off4 in
	let (cgReg,holeReg,tmplReg,insts) = 
	  Cyclone.match_cgfilljmp off1 off2 off3 off4 insts in
	merge rest insts (CgFillJmp (cgReg,holeReg,i1,i2,tmplReg,i3,i4)::accum)
    | (An_cgfilljcc(i1,i2,i3,i4))::rest,insts ->
	let (sec1,off1) = label_offset i1 in
	let (sec2,off2) = label_offset i2 in
	let (sec3,off3) = label_offset i3 in
	let (sec4,off4) = label_offset i4 in
	if sec1 <> sec2 || sec1 <> sec3 || sec1 <> sec4 
	then fail "merge: cgfilljcc -- bad section 1." else 
	let off1 = int_to_int32 off1 in
	let off2 = int_to_int32 off2 in
	let off3 = int_to_int32 off3 in
	let off4 = int_to_int32 off4 in
	let (cgReg,holeReg,tmplReg,insts) = 
	  Cyclone.match_cgfilljcc off1 off2 off3 off4 insts in
	merge rest insts (CgFillJcc (cgReg,holeReg,i1,i2,tmplReg,i3,i4)::accum)
    | (An_cgforget(i1,i2))::rest,_ ->
	merge rest is ((CgForget(i1,i2))::accum)
    | (An_cgend)::rest,insts ->
	let (r,insts) = Cyclone.match_cgend insts in
	merge rest insts (CgEnd r::accum)
(* end CYCLONE *)
    | [],_::_ -> fail "merge:too many instructions"
    | _::_,[] -> fail "merge:too many annotations"
    | a::_,i::_ -> 
	print_string "\nAnnotation: "; print_inst_annot a; print_string "\n";
	print_string "Instruction: ";
	Talpp.print_instruction Format.std_formatter Talpp.std_options i;
	Format.print_newline(); 
	fail "merge_inst_annots -- bad instruction" in 
(*
  List.iter (fun a -> (print_inst_annot a; print_string "\n")) annots;
  List.iter (fun i -> (Talpp.print_instruction Format.std_formatter
			 Talpp.std_options i; Format.print_newline ())) instrs;
*)
  merge annots instrs []
;;

let print_data_annot d = 
  match d with
    An_dlabel _ -> print_string "An_dlabel"
  | An_dbytes _ -> print_string "An_dbytes"
  | An_d2bytes  -> print_string "An_d2bytes"
  | An_d4bytes _ -> print_string "An_d4bytes"
  | An_dfloat32 -> print_string "An_dfloat32"
  | An_dfloat64 -> print_string "An_dfloat64"
  | An_djunk -> print_string "An_djunk"
  | An_drep _ -> print_string "An_drep"
  | An_dup -> print_string "An_dup"
  | An_ddown -> print_string "An_ddown"
;;

(* return an annotation for a hunk of data *)
let data_annots ds = 
  let dannot d = 
    match d with
      Dlabel (x,c) -> An_dlabel c
    | Dbytes (s) -> An_dbytes(String.length s)
    | D2bytes _ -> An_d2bytes
    | D4bytes (_,c) -> An_d4bytes c
    | Dfloat32 _ -> An_dfloat32
    | Dfloat64 _ -> An_dfloat64
    | Djunk -> An_djunk
    | Drep (ri,s) -> An_drep (match ri with 
	 RCon _ -> An_con
       | RKind _ -> An_kind
       | RLabel _ -> An_label)
    | Dup -> An_dup
    | Ddown -> An_ddown
  in List.map dannot ds
;;

(* find labels that have no real instructions since they will have the
 * same address as other labels.  All other such labels will need
 * a VirtLabel coercion on their uses

 * Take a list of code block vectors to accomodate templates.
 *)
let virtual_label_map cbvs = 
  let d = ref (Dict.empty id_compare) in
  let vlm cbv =
     let has_real_instruction i = 
       let (_,_,insts) = cbv.(i) in
       let len = Array.length insts in
       let rec loop i = 
	 if i < len then
	   (not (is_virtual_instruction (insts.(i)))) or (loop (i+1))
	 else false in 
       loop 0 
     in
     let has_real_v = Array.init (Array.length cbv) has_real_instruction in
     
     (* Labels are associated with holes so if a block has a bunch of 
	virtual instructions followed by a hole instruction then we 
	need to emit a VirtLabel annotation. *)

     let starts_with_hole insts = 
       let num_insts = Array.length insts in
       let rec aux i =
	 if i=num_insts then false else 
	 if is_virtual_instruction insts.(i) then aux (i+1) else 
	 (match insts.(i) with
	 | CgHole(_,_,_)      -> true
	 | CgHoleJmp(_,_)     -> true
	 | CgHoleJcc(_,_,_,_) -> true
	 | _ -> false) in
       aux 0
     in
     let prev_virtual = ref false in
     for i = 0 to Array.length has_real_v - 1 do
       let l,_,insts = cbv.(i) in
       let is_virtual = 
	 if has_real_v.(i) then
	   (let r = !prev_virtual in 
	   prev_virtual := false; 
	   (r or (starts_with_hole insts)))
	 else (prev_virtual := true; true) in
       if is_virtual then (d := Dict.insert !d l ());
       ()
     done in
  List.iter vlm cbvs;
  let d = !d in 
  let f x = Dict.member d x in
  f
;;
