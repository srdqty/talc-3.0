(**********************************************************************)
(* (c) Greg Morrisett, Neal Glew, Dan Grossman                        *)
(*     June 1998, all rights reserved.                                *)
(**********************************************************************)

(* tallex.mll
 *
 * A basic lexer for tal format MASM files.
 *
 * TODO: really should make a more efficient symbol table as the three hash
 *       table lookups with exception handlers ain't great!
 *)

{
open Numtypes;;
open Tal;;
open Talparser;;

let err s lexbuf =
  let seg =
    Gcdfec.seg_of_abs
      (Lexing.lexeme_start lexbuf) (Lexing.lexeme_end lexbuf) in
  Gcdfec.post_error (Gcdfec.mk_err_lex seg s)
;;

(* Unfortunately we need to scan some filenames and they look too much like
 * identifiers.  The only way around this is to use context information to
 * know when to look for them.  The rw_filename table contains keywords that
 * require a filename on the same line.
 *)

let rw_filename = Hashtbl.create 13;;

let _ =
  List.iter 
    (fun (s,f) -> 
      Hashtbl.add rw_filename s f;
      Hashtbl.add rw_filename (String.uppercase s) f)
    ["include",(fun s->Tinclude s); "tal_export",(fun s->Ttal_export s);
      "tal_import",(fun s->Ttal_import s)]
;;

(* MASM reserved words are not case sensitive, we use two tables for reserved
 * words: those that are case sensitive (some of the TAL keywords), and those
 * that aren't (some TAL keywords, all MASM reserved words)
 *)

let rw_cis = Hashtbl.create 357;; (* Replace size with something better *)
let rw_cs = rw_cis (* Hashtbl.create 157;;*)

let _ =
  List.iter
    (fun (s,t) -> 
      Hashtbl.add rw_cis s t; 
      Hashtbl.add rw_cis (String.uppercase s) t)
    [ "adc",Tadc; "add",Tadd; "ah",Tah; "al",Tal;"align",Talign;
      "and",Tand; 
      "ax",Tax; "bh",Tbh; "bl",Tbl; "bp",Tbp;
      "bswap",Tbswap; "btagi",Tbtagi; "btagvar",Tbtagvar; "bx",Tbx;
      "cap",Tcap; "byte",Tbyte;
      "call",Tcall; "cbw",Tcbw; "cdq",Tcdq; "ch",Tch; "cl",Tcl;
      "clc",Tclc; "cmc",Tcmc;
      "cmova",Tcmov Above; "cmovae",Tcmov AboveEq; "cmovb",Tcmov Below;
      "cmovbe",Tcmov BelowEq; "cmovc",Tcmov Below; "cmove",Tcmov Eq;
      "cmovg",Tcmov Greater; "cmovge",Tcmov GreaterEq; "cmovl",Tcmov Less;
      "cmovle",Tcmov LessEq; "cmovna",Tcmov BelowEq; "cmovnae",Tcmov Below;
      "cmovnb",Tcmov AboveEq; "cmovnbe",Tcmov Above; "cmovnc",Tcmov AboveEq;
      "cmovne",Tcmov NotEq; "cmovng",Tcmov LessEq; "cmovnge",Tcmov Less;
      "cmovnl",Tcmov GreaterEq; "cmovnle",Tcmov Greater;
      "cmovno",Tcmov NotOverflow; "cmovnp",Tcmov ParityOdd;
      "cmovns",Tcmov NotSign; "cmovnz",Tcmov NotEq; "cmovo",Tcmov Overflow;
      "cmovp",Tcmov ParityEven; "cmovpe",Tcmov ParityEven;
      "cmovpo",Tcmov ParityOdd; "cmovs",Tcmov Sign; "cmovz",Tcmov Eq;
      "cmp",Tcmp; "code",Tcode; "coerce",Tcoerce;
      "cwd",Tcwd; "cwde",Tcwde; "cx",Tcx; 
      "data",Tdata; "db",Tdb; "dd",Tdd;
      "dec",Tdec; "dh",Tdh; "di",Tdi;"div",Tdiv;
      "dl",Tdl; "dw",Tdw; "dword",Tdword; "dx",Tdx; 
      "eax",Treg Eax; "ebp",Treg Ebp; "ebx",Treg Ebx; "ecx",Treg Ecx; 
      "edi",Treg Edi; "edx",Treg Edx;
      "end",Tend; "esi",Treg Esi; "esp",Treg Esp; "fallthru",Tfallthru;
      "ST0",Tfpreg 0; "ST1",Tfpreg 1; "ST2",Tfpreg 2; "ST3",Tfpreg 3;
      "ST4",Tfpreg 4; "ST5",Tfpreg 5; "ST6",Tfpreg 6; "ST7",Tfpreg 7;
      "ST0?",Tfpregq 0; "ST1?",Tfpregq 1; "ST2?",Tfpregq 2; "ST3?",Tfpregq 3;
      "ST4?",Tfpregq 4; "ST5?",Tfpregq 5; "ST6?",Tfpregq 6; "ST7?",Tfpregq 7;
      "forgetname",Tforgetname; "forgetunique",Tforgetunique; 
      "idiv",Tidiv; "imul",Timul; "inc",Tinc; "int",Tint;
      "into",Tinto; 
      "ja",Tj Above; "jae",Tj AboveEq; "jb",Tj Below; "jbe",Tj BelowEq;
      "jc",Tj Below; "je",Tj Eq; "jecxz",Tjecxz; "jg",Tj Greater;
      "jge",Tj GreaterEq; "jl",Tj Less; "jle",Tj LessEq; "jmp",Tjmp;
      "jna",Tj BelowEq; "jnae",Tj Below; "jnb",Tj AboveEq; "jnbe",Tj Above;
      "jnc",Tj AboveEq; "jne",Tj NotEq; "jng",Tj LessEq; "jnge",Tj Less;
      "jnl",Tj GreaterEq; "jnle",Tj Greater; "jno",Tj NotOverflow;
      "jnp",Tj ParityOdd; "jns",Tj NotSign; "jnz",Tj NotEq; "jo",Tj Overflow;
      "jp",Tj ParityEven; "jpe",Tj ParityEven; "jpo",Tj ParityOdd;
      "js",Tj Sign; "jz",Tj Eq;
      "labeltype",Tlabeltype; "lahf",Tlahf; "lea",Tlea;
      "loopd",Tloopd; "looped",Tlooped; "loopned",Tloopned;
      "malloc",Tmalloc; "mov",Tmov; "movsx",Tmovsx; "movzx",Tmovzx;
      "mul",Tmul; "name", Tname; "nameobj",Tnameobj; 
      "neg",Tneg; "nop",Tnop; "not",Tnot; "or",Tor; "pop",Tpop;
      "popad",Tpopad; "popfd",Tpopfd;
      "proof", Tproof; "ptr",Tptr;
      "push",Tpush; "pushad",Tpushad;
      "pushfd",Tpushfd; 
      "qword",Tqword;
      "rcl",Trcl; "rcr",Trcr; 
      "rdtsc",Trdtsc;
      "removename",Tremovename;
      "retn",Tretn; "rol",Trol;
      "ror",Tror; "sahf",Tsahf; "sal",Tsal; 
      "sar",Tsar; "sbb",Tsbb;
      "seta",Tset Above; "setae",Tset AboveEq; "setb",Tset Below;
      "setbe",Tset BelowEq; "setc",Tset Below;
      "sete",Tset Eq; "setg",Tset Greater; "setge",Tset GreaterEq;
      "setl",Tset Less; "setle",Tset LessEq; "setna",Tset BelowEq;
      "setnae",Tset Below; "setnb",Tset AboveEq; "setnbe",Tset Above;
      "setnc",Tset AboveEq; "setne",Tset NotEq; "setng",Tset LessEq;
      "setnge",Tset Less; "setnl",Tset GreaterEq; "setnle",Tset Greater;
      "setno",Tset NotOverflow; "setnp",Tset ParityOdd; "setns",Tset NotSign;
      "setnz",Tset NotEq; "seto",Tset Overflow; "setp",Tset ParityEven;
      "setpe",Tset ParityEven; "setpo",Tset ParityOdd; 
      "sets",Tset Sign; "setz",Tset Eq;
      "shl",Tshl; "shld",Tshld; "shr",Tshr; "shrd",Tshrd; "si",Tsi; "sp",Tsp;
      "stc",Tstc; "sub",Tsub; "sunpack",Tsunpack; "tagof",Ttagof;
      "tal_struct",Ttal_struct; "tal_ends",Ttal_ends; "test",Ttest;
      "type",Ttype; "typeof",Ttypeof; 
      "unpack",Tunpack; "val",Tval; "xchg",Txchg; "xor",Txor;"word",Tword;
       (* LX *)
       "letprod", Tletprod; "letroll", Tletroll;
       "vcase", Tvcase; "kind", Tkind; "reckind", Tkindrec; "primrec", Tconrec;
       "andkind", Tandkind; "andprim", Tandcon ;
       (* end LX *)
       (* LR *)
       "drep", Tdr; "label", Tlabel
       (* end LR *)
    ]
;;

(* No argument floating point instructions *)
let _ =
  List.iter
    (fun (s,t) -> 
      Hashtbl.add rw_cis s t; 
      Hashtbl.add rw_cis (String.uppercase s) t)
    ["f2xm1", Tfp F2xm1; "fabs", Tfp Fabs; "fchs", Tfp Fchs;
      "fclex", Tfp Fclex; "fnclex", Tfp Fnclex; "fcompp", Tfp Fcompp;
      "fucompp", Tfp Fucompp; "fcos", Tfp Fcos; "fdecstp", Tfp Fdecstp;
      "fincstp", Tfp Fincstp; "finit", Tfp Finit; "fninit", Tfp Fninit;
      "fld1", Tfp Fld1; "fldz", Tfp Fldz; "fldpi", Tfp Fldpi; 
      "fldl2e", Tfp Fldl2e; "fldl2t", Tfp Fldl2t; "fldlg2", Tfp Fldlg2; 
      "fldln2", Tfp Fldln2; "fnop", Tfp Fnop; "fptan", Tfp Fptan;
      "fpatan", Tfp Fpatan; "fprem",Tfp Fprem; "fprem1",Tfp Fprem1;
      "frndint", Tfp Frndint; "fscale", Tfp Fscale; "fsin", Tfp Fsin;
      "fsincos", Tfp Fsincos; "fsqrt", Tfp Fsqrt; "ftst", Tfp Ftst;
      "fwait", Tfp Fwait; "fxam", Tfp Fxam; "fxtract", Tfp Fxtract;
      "fyl2x", Tfp Fyl2x; "fyl2xp1", Tfp Fyl2xp1
    ] 
;;

(* Some argument floating point instructions *)
let _ =
  List.iter
    (fun (s,t) -> 
      Hashtbl.add rw_cis s t; 
      Hashtbl.add rw_cis (String.uppercase s) t)
    [
      (* Tfpbin: binary operator, no args, regs, or memory operand *)
      "fadd", Tfpbin Fadd; "fdiv", Tfpbin Fdiv; "fdivr", Tfpbin Fdivr; 
      "fmul", Tfpbin Fmul; "fsub", Tfpbin Fsub; "fsubr", Tfpbin Fsubr;
      (* Tfpmem: memory operand only *)
      "fiadd", Tfpmem Fiadd; 
      "ficom", Tfpmem Ficom; "ficomp", Tfpmem Ficomp; "fidiv", Tfpmem Fidiv;
      "fidivr", Tfpmem Fidivr; "fimul", Tfpmem Fimul; 
      "fisub", Tfpmem Fisub; "fisubr", Tfpmem Fisubr;
      "fild", Tfpunary Fild; "fist", Tfpmem Fist; "fistp", Tfpmem Fistp;
      (* Tfpsst: binary operator, reg operands, 2nd is ST  *)
      "faddp", Tfpsst Faddp; "fdivp", Tfpsst Fdivp; "fdivrp", Tfpsst Fdivrp; 
      "fmulp", Tfpsst Fmulp; "fsubp", Tfpsst Fsubp; "fsubrp", Tfpsst Fsubrp;
      (* Tfcom: comparison operators: no args, reg, or mem *)
      "fcom", Tfcom Fcom; "fcomp", Tfcom Fcomp; 
      (* Tfpnone_or_reg: no args or 1 register arg (ST is implicit arg) *)
      "fucom", Tfpnone_or_reg Fucom; "fxch", Tfpnone_or_reg Fxch;
      "fucomp", Tfpnone_or_reg Fucomp;
      (* Tfpunary: unary operator, reg or mem *)
      "fld", Tfpunary Fld; "fst", Tfpunary Fst; "fstp", Tfpunary Fstp;
      (* Tffree: takes a single stack argument *)
      "ffree", Tffree; 
      (* Tfpregs: take 2 register operands *)
      "fcomi", Tfpregs Fcomi; "fcomip", Tfpregs Fcomip;
      "fucomi", Tfpregs Fucomi; "fucomip", Tfpregs Fucomip;
      (* Tfstsw: take memory operand or AX as argument *)
      "fstsw", Tfstsw Fstsw; "fnstsw", Tfstsw Fnstsw; 
    ] 
;;

let _ =
  List.iter
    (fun (s,t) -> Hashtbl.add rw_cs s t) 
    ["?", Tquestion; "_begin_TAL",T_begin_TAL; "_end_TAL",T_end_TAL;
      "All",TAll;
      "array",Tarray; "B1",TB Byte1; "B2",TB Byte2; "B4",TB Byte4;
      "B8",TB Byte8; "Tbool", Tbool; "Exist",TExist; "false",Tfalse;
      "iff", Tciff;
      "fn",Tfn; "junk",Tjunk; "junk1",TJB Byte1; "junk2",TJB Byte2; 
      "junk4",TJB Byte4; "junk8",TJB Byte8; 
      "pack",Tpack; "prove",Tprove;
      "R",TR; "R16",TR16; "rec",Trec; "RH",TRH; "RL",TRL; "roll",Troll;
      "rollsum",Trollsum;
      "S",TS; "se",Tse; "slot",Tslot; "sptr",Tsptr;
      "ST",Tst; "subsume",Tsubsume; "sum",Tsum; "true", Ttrue;
      "tapp",Ttapp;
      "Nm",TNm;"unroll",Tunroll;
      "virtual",Tvirtual;
      (* Base Kinds *)
      "T",TT; 
      "T1",Tbk (kbyte Byte1); "T2", Tbk (kbyte Byte2); 
      "T4",Tbk (kbyte Byte4); "T8",Tbk (kbyte Byte8);
      "Tm",TTm;
      "Tms",Tbk kms; "Ts",Tbk kstack; "Sint",Tbk kint; 
      "Tn",Tbk kname; "Tcap",Tbk kcap; 
      (* Float *)
      "F4",Tfloat32; "F8",Tfloat64;
      "REAL4",Tdfloat32; "REAL8",Tdfloat64;
      (* LX *)
       "inj", Tinj; "case",  Tcase;  "void", Tvoid ;
       (* end LX *)
       "Rep", Trep
    ] 
;;

(* Cyclone *)
let _ =
  List.iter
    (fun (s,t) -> 
      Hashtbl.add rw_cis s t;
      Hashtbl.add rw_cis (String.uppercase s) t)
    [ "cgstart",Tcgstart; "cgdump",Tcgdump;
      "cgend",Tcgend; "cgforget",Tcgforget;
      "cgfill",Tcgfill; "cgfilljmp",Tcgfilljmp; "cgfilljcc",Tcgfilljcc;
      "cghole",Tcghole; "cgholejmp",Tcgholejmp; "cgholejcc",Tcgholejcc;
      "template_start",Ttemplate_start; "template_end",Ttemplate_end;
      "tmpl",Ttmpl; "cgregion",Tcgregion; "ecg",Tecg
    ]
;;
let _ =
  List.iter
    (fun (s,t) -> Hashtbl.add rw_cs s t) 
    [ "_begin_CYCLONE",T_begin_CYCLONE;
      "tptr",Ttptr; ]
;;
(* End Cyclone *)

let process_identifier s filename lexbuf =
(*   let lower = String.lowercase s in *)
  try Hashtbl.find rw_cis  s (* lower *)
  with Not_found ->
(*    try Hashtbl.find rw_cs s
    with Not_found -> *)
      try
	let t = (Hashtbl.find rw_filename s (* lower*) ) in
	t (filename lexbuf)
      with Not_found ->
      	Tident s
;;

let process_number s =
  let l = String.length s in
  if l=0 then invalid_arg "Tallex.process_number";
  match s.[l-1] with
    'b' | 'y' | 'o' | 'q' | 'h' ->
      failwith "Tallex.process_number - nondecimal unimplemented"
  | 'd' | 't' -> Tnumber (int32_of_string (String.sub s 0 (l-1)))
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' ->
      Tnumber (int32_of_string s)
  | 'r' -> invalid_arg "Tallex.process_number: incorrect hexformat real designation"
  | _ -> invalid_arg "Tallex.process_number"
;;

let process_string s =
  Tstring (String.sub s 1 (String.length s - 2))
;;

let process_literal s =
  let l = String.length s in
  if l >= 3 then
    Tliteral (String.sub s 2 (l - 2))
  else
    invalid_arg "Tallex.process_literal: incorrect literal format"
;;
} 

rule main = parse
(* Whitespace, Comments *)
  [' ' '\009' '\011' '\012']+ {main lexbuf}
| '\010' | '\013' | "\010\013" | "\013\010"
    {Gcdfec.new_line lexbuf; Teol}
| ';' {comment lexbuf; Teol}
(* line continuation -- ignore the rest, but do not return Teol *)
| '\\' { comment lexbuf; main lexbuf}
(* Identifiers *)
| ['a'-'z' 'A'-'Z' '_' '$' '?']['a'-'z' 'A'-'Z' '_' '@' '$' '?' '0'-'9']*
    {process_identifier (Lexing.lexeme lexbuf) filename lexbuf}
(* Literals *)
| '-'? ['0'-'9']+['d' 't']?
| ['0'-'9']+['b' 'y' 'o' 'q']
| ['0'-'9']['0'-'9' 'a'-'f' 'A'-'F']*['h' 'r']
    {process_number (Lexing.lexeme lexbuf)}
(* Floating point values: designated using 0x<hexdigits> *)
| '0''x'['0'-'9' 'a'-'f' 'A'-'F']+
    {process_literal (Lexing.lexeme lexbuf)}
| '-'?['0'-'9']+"."['0'-'9']+(['E''e']['+''-']?['0'-'9']+)?
    { Tfloat (Lexing.lexeme lexbuf) }
| '-'?['0'-'9']+("."['0'-'9']*)?(['E''e']['+''-']?['0'-'9']+)
    { Tfloat (Lexing.lexeme lexbuf) }

| '\'' [' '-'&' '\040'-'~']* '\''
| '\034' ['\032' '\033' '\035'-'~']* '\034'
    {process_string (Lexing.lexeme lexbuf)}
(* Punctuation *)
| '<' {Tlab}            | '^' {Tcaret}
| '>' {Trab}            | '#' {Thash} (* @ used in identifiers. *)
| '(' {Tlb}             | ',' {Tcomma}
| ')' {Trb}		| '.' {Tdot}  
| '[' {Tlsb}		| '*' {Tstar} 
| ']' {Trsb}		| '+' {Tplus} 
| '{' {Tlcb}		| ':' {Tcolon}
| '}' {Trcb}		| '=' {Tequal}
| '|' {Tbar}            | '`' {Tbackquote}
| '!' {Tbang}           | '&' {Tamper}
(* Special *)
| "-!>" {Tarrow}
| "::" {Tcons}
| "<=" {Tleq}
| "^r" {Tvar Read}
| "^w" {Tvar Write}
| "^rw" {Tvar ReadWrite}
| "&[" {Tamperlsb}
| eof {Teof}
| "++" {Tplusplus}
| "-" {Tminus}
| "*u" {Tmulu}
| "*#" {Tmuls}
| "=!>" {Tcimplies}
| "&&" {Tcand}
| "~" {Tnot}
| "<#" {Tlts}
| "<=#" {Tltes}
(* Unused *)
| ['\\' '/' '%' '\'']
    {err "Currently unused character" lexbuf; main lexbuf}
| ['\000'-'\008' '\014'-'\031' '\127'-'\255']
    {err "Illegal Character" lexbuf; raise Gcdfec.Exit}
| _ {failwith ("should not be here! "^
	       (Char.escaped (Lexing.lexeme_char lexbuf 0)))}
(* Comments *)
and comment = parse
  '\010' | '\013' | "\010\013" | "\013\010" {Gcdfec.new_line lexbuf}
| [^ '\010' '\013' ]+ {comment lexbuf}

(* Filenames *)
and filename = parse
  [' ' '\009' '\011' '\012']+ {filename lexbuf}
| ['\033'-'\126']+ {Lexing.lexeme lexbuf}
| _ {err "Illegal Character in filename" lexbuf; raise Gcdfec.Exit}

(* EOF: tallex.mll *)
