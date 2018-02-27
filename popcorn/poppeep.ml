(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Frederick Smith,                    *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* poppeep.ml -- peephole optimizer for the stack based code generator. *)

open Numtypes;;
module T = Tal

(* peephole optimization *)
let optimize in_template insts =
  let some_optimization = ref false in
  let rec click(insts) = (some_optimization := true; peephole insts)
  and peephole insts =
    match insts with
      [] -> []

      (* MOV EAX, [ESP]    ======> POP EAX
         ADD ESP, 4
       *)
    | (T.Mov(T.Reg T.Eax,(T.Prjr((T.Esp,[]),i1,None),[])))::
      (T.ArithBin(T.Add,T.Reg T.Esp,T.Immed i2))::rest
      when i1=$i32_0 & i2=$i32_4 ->
	click((T.Pop(T.Reg T.Eax))::rest)

      (* PUSH gc  =========> MOV r,gc
         POP  r
       *)
    | (T.Push gc)::T.Pop(T.Reg r)::rest ->
	click((T.Mov(T.Reg r,gc))::rest)

      (* MOV  EAX, g1      ======> MOV EAX, g1
         PUSH EAX
         MOV  EAX, g2
         POP  EAX

         MOV  EAX, g1      ======> MOV r',  g1
         PUSH EAX                  MOV EAX, g2'
         MOV  EAX, g2     where r' != EAX and g2' is g2 with offset adjusted
         POP  r'          note: incorrect if g2 == r' DOES THIS HAPPEN?
       *)
    | (T.Mov(T.Reg T.Eax,cgop))::(T.Push(T.Reg T.Eax,[]))::
      (T.Mov(T.Reg T.Eax,(gop2,cs2)))::(T.Pop(T.Reg r'))::rest ->
      	(match r' with
	  T.Eax -> click((T.Mov(T.Reg T.Eax,cgop))::rest)
      	|	_ -> click((T.Mov(T.Reg r',cgop))::
			      (T.Mov(T.Reg T.Eax,((match gop2 with
		                T.Prjr((T.Esp,[]),i,None) ->
				  T.Prjr((T.Esp,[]),i-$i32_4,None)
	                      | _ -> gop2),cs2)))::rest))

(* Cyclone *)
(* Changes an absolute call to a relative call, which causes bugs for Cyclone
   so we disable it within templates *)
      (* MOV EAX, addr    ========> CALL addr
         CALL EAX
       *)
    | (T.Mov(T.Reg T.Eax,(T.Addr x,[])))::(T.Call(T.Reg T.Eax,c))::rest 
      when not(in_template) ->
      	click((T.Call(T.Addr x,c))::rest)
(* End Cyclone *)

      (* MOV  EAX, g ========> PUSH g
         PUSH EAX       note: incorrect if EAX is live but doesn't happen
       *)
    | (T.Mov(T.Reg T.Eax,cgop))::(T.Push(T.Reg T.Eax,[]))::rest ->
      	click((T.Push(cgop))::rest)

      (* MOV EAX, imm        ========> MOV [EBX+off], imm
         MOV [EBX+off], EAX   note: incorrect if EAX if live but doesn't happen
       *)
    | (T.Mov(T.Reg T.Eax,(((T.Immed _) as gop),cs)))::
      (T.Mov(T.Prjr ((T.Ebx,[]),i,None),(T.Reg T.Eax,[])))::rest ->
	click((T.Mov(T.Prjr((T.Ebx,[]),i,None),(gop,cs)))::rest)

      (* ADD ESP, imm1       ========> ADD ESP, imm1+imm2
         ADD ESP, imm2
       *)
    | (T.ArithBin(T.Add,T.Reg T.Esp,T.Immed i))::
      (T.ArithBin(T.Add,T.Reg T.Esp,T.Immed j))::rest ->
      	click((T.ArithBin(T.Add,T.Reg T.Esp,T.Immed(i+$j)))::rest)

      (* SUB ESP, imm1       ========> SUB ESP, imm1+imm2
         SUB ESP, imm2
       *)
    | (T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed i))::
      (T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed j))::rest ->
      	click((T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed(i+$j)))::rest)

      (* ADD ESP, imm1       ========> OP ESP, abs(imm1 - imm2)
         SUB ESP, imm2     where OP = ADD if imm1 > imm2, 
                                 OP = SUB if imm1 < imm2,
	                         instruction deleted otherwise
       *)
    | (T.ArithBin(T.Add,T.Reg T.Esp,T.Immed i))::
      (T.ArithBin(T.Sub,T.Reg T.Esp,T.Immed j))::rest ->
	if i = j then click rest
	else 
	  let op,arg = if i > j then T.Add,i-$j else T.Sub,j-$i in
      	  click((T.ArithBin(op,T.Reg T.Esp,T.Immed arg))::rest)
      
       (* ADD ESP, 0       =========> delete instruction
          
          ADD ESP, 4      =========> MOV [ESP], g
          PUSH g        where g is not ESP, or a projection

          ADD  ESP, i      =========> ADD ESP, i-4 
          PUSH g                      MOV [ESP], g
	                where g is not ESP, or a projection and i > 4
	*)
    | ((T.ArithBin(T.Add,T.Reg T.Esp,T.Immed i)) as i1)::
      (((T.Push(gop,cs))::rest) as tail) ->
	begin
	  let reduce = 
	    i >=$ i32_4 & 
	    (match gop with
	      (T.Immed _ | T.Addr _) -> true
	    | T.Reg T.Esp -> false
	    | T.Reg _ -> true
	    | _ -> false)
	  in
	  if reduce then
	    if i =$ i32_4 then
	      click((T.Mov(T.Prjr((T.Esp,[]),i32_0,None),(gop,cs)))::rest)
	    else 
	      click((T.ArithBin(T.Add,T.Reg T.Esp,T.Immed (i -$ i32_4)))::
		    (T.Mov(T.Prjr((T.Esp,[]),i32_0,None),(gop,cs)))::rest)
	  else if i =$ i32_0 then peephole tail
	  else i1::(peephole tail)
	end

      (* MOV    EAX, (g,c1) ========> MOV EAX, (g, c1@c2)
         COERCE EAX, c2
       *)
    | (T.Mov(T.Reg T.Eax,(gop,coercions)))::
      (T.Coerce(T.Reg T.Eax,coercions2))::rest ->
      	click((T.Mov(T.Reg T.Eax,(gop,coercions2 @ coercions)))::rest)

      (* MOV EBX, 1       ========> MOV EAX, g
         MOV EAX, g                 INC EAX
         ADD EAX, EBX     note: incorrect if EBX is live, but doesn't happen

         MOV EBX, 1       ========> MOV EAX, g
         MOV EAX, g                 DEC EAX
         SUB EAX, EBX     note: incorrect if EBX is live, but doesn't happen
         
         MOV EBX, imm     ========> MOV EAX, g
         MOV EAX, g                 op  EAX, imm
         op  EAX, EBX     note: incorrect if EBX is live, but doesn't happen
      *)
    | (T.Mov(T.Reg T.Ebx,(T.Immed i,[T.Subsume _])))::
      (T.Mov(T.Reg T.Eax,cgop))::
      (T.ArithBin(ab,T.Reg T.Eax,T.Reg T.Ebx))::rest ->
	(match ab with
	  T.Add when i=$i32_1 -> 
	    click((T.Mov(T.Reg T.Eax,cgop))::(T.ArithUn(T.Inc,T.Reg T.Eax))
		     ::rest)
	| T.Sub when i=$i32_1 ->
	    click((T.Mov(T.Reg T.Eax,cgop))::(T.ArithUn(T.Dec,T.Reg T.Eax))
		     ::rest)
	| _ -> click((T.Mov(T.Reg T.Eax,cgop))
			  ::(T.ArithBin(ab,T.Reg T.Eax,T.Immed i))::rest))

      (* MOV ECX, imm      ==========> MOV EAX, g
         MOV EAX, g                    CMP EAX, imm
         CMP EAX, ECX      note: incorrect if ECX is live, but doesn't happen
       *)
    | (T.Mov(T.Reg T.Ecx,(T.Immed i,cs)))::(T.Mov(T.Reg T.Eax,cgop))::
      (T.Cmp((T.Reg T.Eax,ccs),(T.Reg T.Ecx,[])))::rest ->
      	click((T.Mov(T.Reg T.Eax,cgop))::
	      (T.Cmp((T.Reg T.Eax,ccs),(T.Immed i,cs)))
	      ::rest)

       (* JMP g            =========> JMP g
          ...
        *)
    | (T.Jmp cgop)::_::_ -> click [(T.Jmp cgop)]

       (* MOV [ESP+off], EAX  ==========>     MOV [ESP+off], EAX
          MOV r, g                            MOV r,g
          MOV EAX, [ESP+off]   where r != EAX

          MOV [ESP+off1], EAX ==========>     MOV [ESP+off1], EAX
          MOV EAX, g                          MOV EAX, [ESP+off2]
          MOV EAX, [ESP+off2]
        *)
    | ((T.Mov(T.Prjr((T.Esp,[]),i,None),(T.Reg T.Eax,[]))) as i1)::
      ((T.Mov(T.Reg r,cgop)) as i2)::
      ((T.Mov(T.Reg T.Eax,(T.Prjr((T.Esp,[]),j,None),[]))) as i3)::rest ->
	(if i =$ j & r != T.Eax then click(i1::i2::rest)
	else if r = T.Eax then click(i1::i3::rest)
	else (i1::(peephole (List.tl insts))))

    | i::rest -> i::(peephole rest) in
  let rec optimize_loop insts = 
    (some_optimization := false;
     let insts = peephole insts in
     if !some_optimization then optimize_loop insts else insts)
  in optimize_loop insts
    
