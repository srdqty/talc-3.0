; TAL macros
;
; Copyright Greg Morrisett, Neal Glew,
;   January 1999, all rights reserved.
;==================================================
; To bracket TAL code with appropriate directives
_begin_TAL MACRO
	.586P
	.MODEL	FLAT, SYSCALL
	EXTERN	C GC_malloc :NEAR, _array_bounds_error :NEAR, \
                _division_by_zero_error :NEAR, _out_of_memory :NEAR, \
                _taltla :NEAR
ENDM
_end_TAL MACRO
ENDM
;==================================================
; To get top level typing information in
LABELTYPE MACRO	a1
ENDM
TAL_IMPORT MACRO intref:REQ
	@@@xport TEXTEQU <>
	INCLUDE	intref
ENDM
TAL_EXPORT MACRO intref:REQ
	@@@xport TEXTEQU <X>
	INCLUDE	intref
ENDM
	OPTION	NOKEYWORD:<TYPE>
TYPE	MACRO	lcd:REQ
ENDM
VAL	MACRO	l:REQ,c:REQ
	IFNB @@@xport
	PUBLIC	l
	ELSE
	EXTERN	l :NEAR
	ENDIF
ENDM
;==================================================
; Code/Data switching
DATA	MACRO
	.DATA
	ASSUME CS:_DATA
ENDM
CODE	MACRO
	.CODE
ENDM
;==================================================
; Instructions
ASUB	MACRO	dest:REQ, array:REQ, scale:REQ, index:REQ, size:REQ
	CMP	index,size
	JAE	_array_bounds_error
	MOV	dest,[array+scale*index]
ENDM
AUPD	MACRO	array:REQ, scale:REQ, index:REQ, src:REQ, size:REQ
	CMP	index,size
	JAE	_array_bounds_error
	MOV	[array+scale*index],src
ENDM
; *** ASSUMPTION: exntag of an exn is in first word of record
BEXN	MACRO	reg:REQ, exntag:REQ, label:REQ
	CMP	DWORD PTR [reg+0],exntag
	JNE	label
ENDM
make_jmp MACRO	cc:REQ
	EXITM	@CATSTR(<J>,<cc>)
ENDM
BTAGI	MACRO	cc:REQ, reg:REQ, tag:REQ, label:REQ
	CMP	reg,tag
	make_jmp(cc)	label
ENDM
BTAGVAR	MACRO	cc:REQ, sum:REQ, tag:REQ, label:REQ
	CMP	DWORD PTR sum,tag
	make_jmp(cc)	label
ENDM
COERCE	MACRO	arg:REQ
ENDM
FALLTHRU MACRO	gamma:REQ
ENDM
GETTLA  MACRO	reg:REQ, x:REQ
	MOV	reg,[_taltla+x]
ENDM
SETTLA	MACRO	x:REQ, arg:REQ
	MOV	DWORD PTR [_taltla+x],arg
ENDM
MALLOC	MACRO	size:REQ, types:REQ, tagging
	PUSH	size
;
; Cyclone
; Fool MASM into emiting an absolute jump
; (CALL GC_malloc would result in a relative jump).
; We need this in Cyclone because jumps from within templates
; must be absolute jumps: templates are copied, and this
; changes offsets.
;
	MOV     EAX,GC_malloc
	CALL	EAX
	ADD	ESP,4
	TEST	EAX,EAX
	JZ	_out_of_memory
ENDM
PROOF	MACRO	pf:REQ
ENDM
UNPACK	MACRO	tvar:REQ, reg:REQ, value:REQ
IFDIFI <reg>, <value>
	MOV	reg, value
ENDIF
ENDM
SUNPACK MACRO   tvar:REQ, genop:REQ
ENDM
;==================================================
; Operands
; Virtual registers - MASM shouldn't see these, generate an error
R	MACRO	num:REQ
	.ERR	"Use of virtual register"
ENDM
R16	MACRO	num:REQ
	.ERR	"Use of virtual register"
ENDM
RL	MACRO	num:REQ
	.ERR	"Use of virtual register"
ENDM
RH	MACRO	num:REQ
	.ERR	"Use of virtual register"
ENDM
S	MACRO	tag:REQ
	EXITM	<tag>
ENDM
;==================================================
; Data Items
EXNNAME	MACRO	type:REQ
	DD	?
ENDM
TAL_STRUCT MACRO
ENDM
TAL_ENDS MACRO
ENDM
;==================================================
; Coercions
pack	MACRO	t1:REQ, arg:REQ, t2:REQ
	EXITM	<arg>
ENDM
tapp	MACRO	arg:REQ, type:REQ
	EXITM	<arg>
ENDM
roll	MACRO	type:REQ, arg:REQ
	EXITM	<arg>
ENDM
unroll	MACRO	arg:REQ
	EXITM	<arg>
ENDM
sum	MACRO	type:REQ, arg:REQ
	EXITM	<arg>
ENDM
rollsum	MACRO	type:REQ, arg:REQ
	EXITM	<arg>
ENDM
rec	MACRO	arg:REQ
	EXITM	<arg>
ENDM
exn	MACRO	arg:REQ
	EXITM	<arg>
ENDM
array	MACRO	off:REQ, depth:REQ, con:REQ, arg:REQ
	EXITM	<arg>
ENDM
slot	MACRO	off:REQ, sz:REQ, arg:REQ
	EXITM	<arg>
ENDM
subsume	MACRO	type:REQ, arg:REQ
	EXITM	<arg>
ENDM
prove   MACRO   arg:REQ
        EXITM   <arg>
ENDM
;==================================================
; EOF: tal.inc
