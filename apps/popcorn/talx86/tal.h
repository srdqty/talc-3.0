#ifndef TAL_H
#define TAL_H

#include <core.h>
#include <list.h>
#include <id.h>
#include <set.h>
#include <dict.h>

prefix Tal  {
open Tal    {
open Core   {
open List   {
open Id     {

extern union scale{ void Byte1, Byte2, Byte4, Byte8;}
extern union reg { 
  void Eax, Ebx, Ecx, Edx, Esi, Edi, Ebp, Esp;
  id Virt;
}
extern union kind { 
  scale Kbyte; 
  void  Ktype; 
  int   Kmemi; 
  void  Kmem; 
  void  Kstack; 
  void  Kint;
  void  Kbool;
  *(kind,kind) Karrow; 
  <kind>list   Kprod;
  void  Kname; 
  void  Kcap; 
  void  Kms;
}
extern union reg_part { void RPe, RPx, RPh, RPl;}
extern union primcon { 
  scale PCbytes; 
  void  PCfloat32;
  void  PCfloat64;
  int   PCjunk; 
  scale PCjunkbytes ;
  int   PCint; 
  void  PCtrue; 
  void  PCfalse;
}
extern union variance {  void Read, Write, ReadWrite; }
extern union log { void Cadd, Csub, Cmuls, Cmulu, Cand, Cor, Cimp, Ciff, Cnot;
                   void Clts, Cltu, Cltes, Clteu; }

extern union alias_info { void Unique, MayAlias; }

 extern union fpreg { void FPfull, FPempty, FPany; }

extern fpstack;

extern struct con {rcon rcon; con_state con_state; < <id>Set::set>Opt freevars;}
extern union con_state { void NotNorm, Normalized, WeakHead; }
extern union rcon {
  // language portion
  id                    Cvar;
  *(id,kind,con)        Clam;
  *(con,con)            Capp;
  <con>list             Ctuple;
  *(int,con)            Cproj;
  // type portion
  id                    Clab;
  primcon               Cprim;
  <*(id,kind,con)>list  Crec;
  *(id,kind,con)        Cforall;
  *(id,kind,con,con)    Cexist;
  con                   Ccode;
  machine_state         Cms;
  *(con,con)            Cmsjoin;
  *(<int>list,<con>Opt,<*(con,variance)>Opt) Chptr;
  *(con,variance)       Cfield;
  <con>list             Cprod;
  <con>list             Csum;
  *(con,con)            Carray;
  con                   Csing;
  // stack portion
  con                   Csptr;
  void                  Cempty;
  *(con,con)            Ccons;
  *(con,con)            Cappend;
  // arithmetic and logical portion
  *(log,<con>list)      Clog;
  *(con,con)            Cif;
  // alias portion
  con                   Cname;
  <id,*(alias_info,con)>Dict::dict Ccap;
  <con>list             Cjoin;
  con                   Ctagof;
  // Cyclone +
  *(con,<con>Opt,
    <*(id,con)>list,
    <*(id,con)>list)    Ctmpl;
  id                    Ctptr;
  *(con,<con>Opt,
    <*(id,
       <*(id,con)>list,
       <*(id,con)>list)
    >list)              Ctrgn;
  // Cyclone -
}
extern struct machine_state {
  <reg,con>Dict::dict regs;
  ccinfo              cc;
  ccinfo              save_cc;
  con                 cap;
  fpstack             fps;
}
extern union ccinfo { 
  void       noinfo;
  *(con,con) cmp;
  *(con,con) test;
}


extern union annotate {
  con                Con;
  reg                Reg;
  *(reg,int)         StackTail;
  *(reg,int,int,con) StackSlice;
}
extern union coercion {
  *(con,con)     Pack;
  annotate       Tapp;
  con            Roll;
  void           Unroll;
  con            Tosum;
  void           Fromsum;
  con            RollTosum;
  *(int,int,con) Toarray; 
  *(int,int)     Slot;
  con            Subsume;
  void           Forgetname;
  void           Prove;
}
extern struct <a>coerce { a op; <coercion>list coercions;} 

extern union genop {
  int Immed;
  reg Reg;
  id Addr;
  *(<reg>coerce, int, <*(scale,reg)>Opt) Prjr;
  *(<id>coerce,  int, <*(scale,reg)>Opt) Prjl;
}
// warning: two prjr or prjl with different coercions may be deemed equal!
extern int genop_compare (genop g1, genop g2);
extern union condition {
  void Above,  AboveEq,    Below,     BelowEq, Eq,          Greater,
    GreaterEq, Less,       LessEq,    NotEq,   NotOverflow, NotSign, 
    Overflow,  ParityEven, ParityOdd, Sign;
}

extern union arithbin { void Adc, Add,  And,   Imul2, Or,  Sbb, Sub, Xor;}
extern union arithun  { void Dec, Inc,  Neg,   Not; }
extern union arithmd  { void Div, Idiv, Imul1, Mul; }
extern union arithsr  { void Rcl, Rcr,  Rol,   Ror,   Sal, Sar, Shl, Shr;}
extern union conv     { void Cbw, Cdq,  Cwd,   Cwde;}

extern union mallocarg { 
  scale           Mbytes;
  <mallocarg>list Mprod;
  *(scale,int)    Mbytearray;
}

 extern union fpnoargs {
   void F2xm1, Fabs, Fchs, Fclex, Fnclex, Fcompp, Fucompp, Fcos, Fdecstp;
   void Fincstp, Finit, Fninit, Fld1, Fldz, Fldpi, Fldl2e, Fldl2t;
   void Fldlg2, Fldln2, Fnop,Fpatan, Fprem, Fprem1, Fptan, Frndint;
   void Fscale, Fsin, Fsincos, Fsqrt, Ftst, Fwait, Fxam, Fxtract;
   void Fyl2x, Fyl2xp1;
 } 

 extern union fpargs {
   int            FPstack;
   *(bool,int)    FPstack2;
   *(scale,genop) FPgenop;
 }

 extern union fpsomeargs {
// generic binary instructions 
   void Fadd, Fcom, Fdiv, Fdivr, Fmul, Fsub, Fsubr, Fucom, Fxch;
// integer instructions 
   void Fiadd, Ficom, Ficomp, Fidiv, Fidivr, Fimul, Fisub, Fisubr;
// instructions that pop an argument 
   void Faddp, Fcomp, Fdivp, Fdivrp, Fmulp, Fsubp, Fsubrp, Fucomp;
// unary load and store instructions 
   void Fst, Fstp, Fist, Fistp, Fld, Fild;
// change fp register tag to empty 
   void Ffree;
// comparison operations that write condition codes to main unit 
// implemented only on the pentium pro and better processors 
   void Fcomi, Fcomip, Fucomi, Fucomip;
// Store Status Word 
  void Fstsw, Fnstsw;
 }

extern union instruction {
  *(arithbin,genop,genop)             ArithBin;
  *(arithun,genop)                    ArithUn;
  *(arithmd,genop)                    ArithMD;
  *(arithsr,genop,<int>Opt)           ArithSR;
  reg                                 Bswap;
  <genop>coerce                       Call;
  void                                Clc;
  void                                Cmc;
  *(condition,reg,<genop>coerce)      Cmovcc;
  *(<genop>coerce,<genop>coerce)      Cmp; 
  conv                                Conv;
  *(reg,genop,int)                    Imul3;
  int                                 Int;
  void                                Into;
  *(condition,<id>coerce,<instruction>list) Jcc;
  *(<id>coerce,<instruction>list)           Jecxz;
  <genop>coerce                       Jmp;
  id                                  Label; // For creating small blocks!
  void                                Lahf;
  *(reg,genop)                        Lea;
  *(<id>coerce,<bool>Opt)             Loopd;
  *(genop,<genop>coerce)              Mov;
  *(bool, genop, reg_part, genop, reg_part) Movpart;
  void                                Nop;
  genop                               Pop;
  void                                Popad;
  void                                Popfd;
  <genop>coerce                       Push;
  void                                Pushad;
  void                                Pushfd;
  <int>Opt                            Retn;
  void                                Sahf;
  *(condition,genop)                  Setcc;
  *(genop,reg,<int>Opt)               Shld;
  *(genop,reg,<int>Opt)               Shrd;
  void                                Stc;
  *(genop,genop)                      Test;
  *(genop,reg)                        Xchg;
  *(reg,genop,int,reg,genop)          Asub;
  *(genop,int,reg,reg,genop)          Aupd;
  <genop>coerce                       Coerce;
  <id>coerce                          CoerceName;
  string                              Comment;
  <con>list                           Fallthru;
  *(id,int,<mallocarg>Opt)            Malloc;
  <*(id,<con>list)>list               Proof;
  *(id,reg,<genop>coerce)             Unpack;
  *(id,genop)                         Sunpack;
  *(id, genop)                        Nameobj;
  id                                  ForgetUnique;
  id                                  RemoveName;
  void                                Rdtsc;
  // Floating point instructions.
  fpnoargs                            FPnoargs;
  *(fpsomeargs,fpargs)                FPsomeargs;
  // Cyclone +
  *(id,con)                           CgStart;
  *(reg,id,reg,id)                    CgDump;
  *(reg,id,id)                        CgHole;
  *(id,<id>coerce)                    CgHoleJmp;
  *(condition, id, <id>coerce,<instruction>list)        
                                      CgHoleJcc;
  *(reg,reg,id,id,reg)                CgFill;
  *(reg,reg,id,id,reg,id,id)          CgFillJmp;
  *(reg,reg,id,id,reg,id,id)          CgFillJcc;
  *(id,id)                            CgForget;
  reg                                 CgEnd;
  // Cyclone -
}
 
extern struct code_block { 
  id id; 
  <con>Opt tipe; 
  instruction insts[]; 
}
//Cyclone +
extern struct template {
  id start_id;
  con tipe;
  code_block blocks[];
}
// Cyclone -
extern union data_item { 
  <id>coerce  Dlabel;
  string      Dbytes; 
  int         D2bytes;
  <int>coerce D4bytes;
  float      Dfloat32;
  double      Dfloat64;
  void        Djunk;
  void        Dup;
  void        Ddown;
}
extern struct data_block 
{ id id;
  int align;
  <con>Opt tipe;
  < <data_item>list>coerce data; 
}
extern union int_con_def {
  void AbsCon;
  con  BoundCon;
  con  ConcCon;
}
extern struct tal_int {
  *(id,con)              int_abbrevs[];
  *(id,kind,int_con_def) int_cons   [];
  *(id,con)              int_vals   [];
}
extern struct tal_int_type {
  <*(id,kind,con)>list it_cons;
  <*(id,con)>     list it_vals;
}
extern struct tal_imp { 
  string         imports    [];
  string         exports    [];
  *(id,con)      imp_abbrevs[];
  *(id,kind,con) con_blocks [];
  code_block     code_blocks[];
  data_block     data_blocks[];
  // Cyclone +
  template       templates  [];
  // Cyclone -
}

extern int compare_regs    (reg,reg);
extern int compare_regparts(reg_part,reg_part);

extern kind k4byte;

extern fpstack       fpstack_st0; // Stack with only st0 occupied.
extern fpstack       fpstack_empty();
extern bool         fpstack_is_empty(fpstack);
extern void          fpstack_set(fpstack,int,fpreg);
extern fpreg         fpstack_get(fpstack,int);
extern machine_state ms_empty     ();
extern con           cempty_cap   ();
extern machine_state ms_map       (con  f(con), machine_state);
extern void          ms_app<a>    (a f(con), machine_state);
extern con           ms_get_reg   (machine_state,reg);
extern machine_state ms_set_reg   (machine_state,reg,con);
extern machine_state ms_set_regs  (machine_state, <*(reg,con)>list);
extern machine_state ms_del_reg   (machine_state,reg);
extern machine_state ms_del_regs  (machine_state,<reg>list);
extern machine_state ms_map_reg   (con f(con), machine_state);
extern void          ms_iter_reg<a>(a f(reg,con), machine_state);
extern a             ms_fold_reg<a>(a f(reg,con,a), machine_state, a);
extern ccinfo        ms_get_cc    (machine_state);
extern machine_state ms_restore_cc(machine_state);
extern con           ms_get_cap   (machine_state);
extern machine_state ms_set_cap   (machine_state, con);
extern machine_state ms_join      (machine_state, machine_state);
extern fpstack       ms_get_fpstack(machine_state);
extern machine_state ms_set_fpstack(machine_state, fpstack);

extern rcon Cprim (primcon);
extern con  defcon(rcon);
extern con  wcon  (rcon);
extern con  prcon (rcon);
extern con  pcbytes(scale);
extern con  cbyte8();
extern con  cbyte4();
extern con  cbyte2();
extern con  cbyte1();
extern con  pcjunk4();
extern con  pcjunk8();
extern con  pcjunk(int);
extern con  pcjunkbytes(scale);
extern con  pcint(int);
extern con  pcfloat32();
extern con  pcfloat64();
extern con  pctrue();
extern con  pcfalse();
extern con  cvar(id);
extern con  clam(id,kind,con);
extern con  capp(con,con);
extern con  ctuple(<con>list);
extern con  cproj(con,int);
extern con  clab(id);
extern con  crec(<*(id,kind,con)>list);
extern con  cforall(id, kind, con);
extern con  cexist(id, kind, con);
extern con  exist_p(id, kind, con, con);
extern con  cms(machine_state);
extern con  cmsjoin(con,con);
extern con  ccode(con);
extern con  ccode_ms(machine_state);
extern con  ccode_l(<*(reg,con)>list);
extern con  cfield(con,variance);
extern con  cprod(<con>list);
extern con  cprod_b(<con>list);
extern con  csum(<con>list);
extern con  carray(con,con);
extern con  carray_s(id, con);
extern con  csing(con);
extern con  csptr(con);
extern con  cempty();
extern con  ccons(con,con);
extern con  cappend(con,con);
// Cyclone +
extern con  ctmpl(con,<con>Opt,<*(id,con)>list,<*(id,con)>list);
extern con  ctptr(id);
extern con  ctrgn(con,<con>Opt,<*(id,<*(id,con)>list,<*(id,con)>list)>list);
// Cyclone -
extern con  chptr(<int>list, <con>Opt, <*(con,variance)>Opt);
extern con  cptr(con);
extern con  ccap(<id,*(alias_info,con)>Dict::dict);
extern con  cjoin(<con>list);
extern con  cname(con);
extern con  ctagof(con);
extern con  cif(con,con);
extern con  clog(log,<con>list);
extern con  cadd(<con>list);
extern con  csub(con,con);
extern con  cmuls(int,con);
extern con  cmulu(int,con);
extern con  cand(<con>list);
extern con  cor(<con>list);
extern con  cnot(con);
extern con  cimplies(con,con);
extern con  ciff(con,con);
extern con  clts(con,con);
extern con  cltu(con,con);
extern con  cltes(con,con);
extern con  clteu(con,con);
extern con  cgts(con,con);
extern con  cgtu(con,con);
extern con  cgtes(con,con);
extern con  cgteu(con,con);
extern con  ceq(con,con);
extern con  cne(con,con);

extern int  min_pointer_integer;
extern bool is_non_pointer_integer(int);

extern condition negate_condition(condition);

extern condition flip_condition(condition);

}}}}}

#endif
