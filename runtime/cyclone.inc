;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Use the _begin_CYCLONE macro just after using the	 ;
; _begin_TAL macro in any file that uses the Cyclone	 ;
; language extensions.					 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
_begin_CYCLONE MACRO
	EXTERN _CG_start :NEAR
	EXTERN _CG_dump :NEAR
	EXTERN _CG_abort :NEAR
	EXTERN _CG_end :NEAR
	EXTERN _CG_mark :NEAR
	EXTERN _external_addr :NEAR
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;				      ;
; CGSTART type			      ;
;				      ;
; Begin a new code generation region. ;
;				      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cgstart MACRO	type:REQ
	PUSH4REGS
	MOV	EAX,_CG_start
	CALL	EAX
	POP4REGS
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;						  ;
; CGDUMP tv, reg, lab				  ;
;						  ;
; Dump the template at label lab into the current ;
; code generation region.  A pointer to the first ;
; instruction of the copied template is put into  ;
; register reg.	 The type variable tv is used	  ;
; by the verifier; it is a BINDING OCCURRENCE.	  ;
;						  ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cgdump	MACRO	tv:REQ, reg:REQ, lab:REQ
	PUSH3REGS reg
	PUSH	lab
	MOV	EAX,_CG_dump
	CALL	EAX
	ADD	ESP, 4
IFDIFI <reg>, <EAX>
	MOV	reg, EAX
ENDIF
	POP3REGS reg
ENDM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								 ;
; CGFILL reg1, labeloftmpl, labelofhole, reg2			 ;
;								 ;
; reg1 is a register pointing into the code being generated,	 ;
; labeloftmpl is the label at the start of the template,	 ;
; labelofhole is the label just before the hole,		 ;
; reg2 is a register containing the value to stuff into the hole ;
;								 ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cgfill	MACRO	reg1:REQ, labeloftmpl:REQ, labelofhole:REQ, reg2:REQ
	MOV	[reg1+(labelofhole-labeloftmpl-3)], reg2
        PUSH3REGS reg2
        PUSH    reg2        
        MOV     EAX,_CG_mark
        CALL    EAX
        POP     reg2
        POP3REGS reg2
ENDM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								           ;
; CGFILLBTAG holereg, htmplab, holelab, targreg, ttmplab, targlab          ;
; (used for BTAGI and BTAGVAR)					           ;
;								           ;
; holereg is a register pointing to the generated hole code                ;
; htmplab is the label of the hole template                                ;
; holelab is the label of the hole                                         ;
; targreg is a register pointing to the generated target code              ;
; ttmplab is the label of the target template                              ;
; targlab is the label of the target                                       ;
;								           ;
;  | BTAGI ... target |                                                    ;
;  --------------------                                                    ;
;                                                                          ;
;  --------------------                                                    ;
;  | target ...       |                                                    ;
;                                                                          ;
; The location of the hole is calculated as follows:                       ;
;   holereg + (holelab + 3 + 2 - htmplab - 4)                              ;
;                                                                          ;
; The values 3 and 2 are added in due to the CMP and JMP instructions.     ;
; The 4 is subtracted because of the template length.  The final           ;
; expressions is:                                                          ;
;   holereg + (holelab - htmplab + 1)                                      ;
;                                                                          ;
; The target address is calculated as follows:                             ;
;  (targreg-holereg)+(targlab-targtmpllab-4)-(holelab-holetmpllab-4+3+2+4) ;
;                                                                          ;
; 4 is subtracted from each template due to template length.  Adding       ;
; 3, 2, and 4 is due to the CMP instruction, JMP opcode, and JMP           ;
; address.  The final expression is:                                       ;
;   (targreg-holereg)+(targlab-targtmpllab)-(holelab-holetmpllab)-9        ;
;                                                                          ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cgfillbtag	MACRO	holereg:REQ, htmplab:REQ, holelab:REQ,
                        targreg:REQ, ttmplab:REQ, targlab:REQ 
	PUSH	targreg
	SUB	targreg,holereg
	ADD	targreg,(targlab-ttmplab)-(holelab-htmplab)-9
	MOV	[holereg+(holelab-htmplab+1)],targreg
	POP	targreg
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								          ;
; CGFILLJMP holereg, htmplab, holelab, targreg, ttmplab, targlab          ;
;								          ;
; This is the same as CGITBTAGI, except a jmp instr filled in.            ;
;                                                                         ;
;                                                                         ;
; The location of the hole is calculated as follows:                      ;
;  holereg+(holelab+1-htmplab-4)=holereg+(holelab-htmplab-3)              ;
; 1 is added for the JMP opcode, 4 is subtracted due to template length:  ;
;                                                                         ;
; The target address is calculated as follows:                            ;
;  (targreg-holereg)+(targlab-targtmpllab-4)-(holelab-holetmpllab-4+1+4)= ;
;  (targreg-holereg)+(targlab-targtmpllab)-(holelab-holetmpllab)-5        ;
;                                                                         ;
; 4 is subtracted from each template due to template length.  Adding 1    ;
; and and 4 are due to the JMP opcode and address.                        ;
;                                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cgfilljmp	MACRO	holereg:REQ, htmplab:REQ, holelab:REQ,
                targreg:REQ, ttmplab:REQ, targlab:REQ 
	PUSH	targreg
	SUB	targreg,holereg
	ADD	targreg,(targlab-ttmplab)-(holelab-htmplab)-5
	MOV	[holereg+(holelab-htmplab-3)],targreg
	POP	targreg
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								          ;
; CGFILLJCC holereg, htmplab, holelab, targreg, ttmplab, targlab          ;
;								          ;
; This is the same as CGITBTAGI, except a jcc instr filled in.            ;
;                                                                         ;
; The location of the hole is calculated as follows:                      ;
;  holereg+(holelab+2-htmplab-4)=holereg+(holelab-htmplab-2)              ;
; 2 is added for the JCC opcode, 4 is subtracted due to template length:  ;
;                                                                         ;
; The target address is calculated as follows:                            ;
;  (targreg-holereg)+(targlab-targtmpllab-4)-(holelab-holetmpllab-4+2+4)= ;
;  (targreg-holereg)+(targlab-targtmpllab)-(holelab-holetmpllab)-6        ;
;                                                                         ;
; 4 is subtracted from each template due to template length.  Adding 2    ;
; and 4 are due to the JMP opcode and address.                            ;
;                                                                         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cgfilljcc	MACRO	holereg:REQ, htmplab:REQ, holelab:REQ,
                targreg:REQ, ttmplab:REQ, targlab:REQ 
	PUSH	targreg
	SUB	targreg,holereg
	ADD	targreg,(targlab-ttmplab)-(holelab-htmplab)-6
	MOV	[holereg+(holelab-htmplab-2)],targreg
	POP	targreg
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;			   ;
; Trevor's original guess. ;
;			   ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cgfillrel	MACRO	reg1:REQ, labeloftmpl:REQ, labelofhole:REQ, reg2:REQ, labeloftmpl2, labeloftarget:REQ
	PUSH	reg2
	LEA	reg2,	 [reg2+(labeloftarget-labeloftmpl2-4)]
	MOV	[reg1+(labelofhole-labeloftmpl+1)], reg2
	POP	reg2
ENDM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;				;
; CGFORGET tv			;
;				;
; Needed for the verifier only. ;
;				;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cgforget	MACRO	tv:REQ
ENDM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;					      ;
; CGABORT val				      ;
;					      ;
; Abort the most recent val code generations; ;
; val should be a register or immediate.      ;
;					      ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cgabort MACRO	val:REQ
	PUSH4REGS
	PUSH	val
        MOV     EAX,_CG_abort	; Force MASM to assemble the call as an
	CALL	EAX		; absolute, in case it is in a template
	ADD	ESP, 4
	POP4REGS
ENDM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;						     ;
; CGEND reg					     ;
;						     ;
; Complete the most recent code generation and leave ;
; a pointer to the new function in register reg.     ;
;						     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cgend MACRO   reg:REQ
	PUSH3REGS reg
	MOV	EAX,_CG_end
	CALL	EAX
IFDIFI <reg>, <EAX>
	MOV	reg, EAX
ENDIF
	POP3REGS reg
ENDM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;							 ;
; TEMPLATE_START lengthlabel,afterlast,type		 ;
; TEMPLATE_END afterlast                                 ;
;							 ;
; These macros delimit templates.  TEMPLATE_START        ;
; generates a 32-bit length field for templates.	 ;
; This length field appears just before the first	 ;
; instruction of the template, and the template itself	 ;
; is a pointer to the length field.			 ;
;							 ;
; lengthlabel is the label to give to the template       ;
;  (this is a binding occurrence)                        ;
; afterlast is the label just after the last instruction ;
;  of the template.					 ;
; type is the type of the template and MASM ignores it.  ;
;							 ;
; TEMPLATE_END simply declares the label afterlast       ;
;  (this is a binding occurrence)                        ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
template_start		MACRO	lengthlabel:REQ, afterlast:REQ, type:REQ
	lengthlabel:
	DD (afterlast - lengthlabel - 4)
ENDM

template_end	MACRO afterlast:REQ
	afterlast:
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; CGHOLE reg, labeloftmpl, labelofhole				;
;								;
; Declares a new hole labelofhole in the template whose first	;
; instruction has label labeloftmpl.  The instruction will move	;
; the contents of the hole into register reg (when the hole has ;
; been filled.							;
;								;
; labelhole is a BINDING OCCURRENCE.				;
;								;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cghole	MACRO	reg:REQ, labeloftmpl:REQ, labelofhole:REQ
labelofhole:
	MOV	reg,0
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; CGHOLEBTAGI                                                   ;
;								;
; Declares a new hole holelab in the template, which consists   ;
; of a BTAGI instruction with the label holelab.  An external   ;
; address is used to create a four byte hole.  The label        ;
; tmpllab is used by the verifier, but not used in the macro	;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cgholebtagi	MACRO	cc:REQ, reg:REQ, tag:REQ, tmpllab:REQ, holelab:REQ
holelab:
	BTAGI	cc,reg,tag,_external_addr
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; CGHOLEBTAGVAR                                                 ;
; Same as CGHOLEBTAGI, except for a BTAGVAR instruction         ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cgholebtagvar	MACRO	cc:REQ, sum:REQ, tag:REQ, tmpllab:REQ, holelab:REQ
holelab:
	BTAGVAR	cc,sum,tag,_external_addr
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; CGHOLEJMP                                                     ;
; Same as CGHOLEBTAGI, except for a JMP instruction             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cgholejmp	MACRO	tmpllab:REQ, holelab:REQ
holelab:
	JMP	_external_addr
ENDM

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;								;
; CGHOLEJCC                                                     ;
; Same as CGHOLEBTAGI, except for a JCC instruction             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

cgholejcc	MACRO	cc:REQ, tmpllab:REQ, holelab:REQ
holelab:
	make_jmp(cc)	_external_addr
ENDM


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                             ;
; These macros save and restore the registers ;
;                                             ;
; x4REGS treats regs EAX, EBX, ECX, and EDX   ;
;                                             ;
; x3REGS only treats regs EBX, ECX, and EDX   ;
; (for when return value is stored in EAX)    ;
;                                             ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

push4regs MACRO
	PUSH	EAX
	PUSH	EBX
	PUSH	ECX
	PUSH	EDX
ENDM

pop4regs MACRO
	POP	EDX
	POP	ECX
	POP	EBX
	POP	EAX
ENDM

push3regs MACRO reg:REQ
IFDIFI <reg>, <EAX>
	PUSH	EAX
ENDIF
IFDIFI <reg>, <EBX>
	PUSH	EBX
ENDIF
IFDIFI <reg>, <ECX>
	PUSH	ECX
ENDIF
IFDIFI <reg>, <EDX>
	PUSH	EDX
ENDIF
ENDM

pop3regs MACRO reg:REQ
IFDIFI <reg>, <EDX>
	POP	EDX
ENDIF
IFDIFI <reg>, <ECX>
	POP	ECX
ENDIF
IFDIFI <reg>, <EBX>
	POP	EBX
ENDIF
IFDIFI <reg>, <EAX>
	POP	EAX
ENDIF
ENDM

;==================================================
; EOF: cyclone.inc
