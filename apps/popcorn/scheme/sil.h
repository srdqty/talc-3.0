#ifndef SIL_H
#define SIL_H

#include "list.h"
#include "sast.h"

prefix Sil {
open   Sil {
open  List {
open  Sast {

extern <*(string,ilexp)>list xprog(<*(string,exp)>list);

extern exception compilerBug(string);
extern int max_args;
extern union tipe {void D, Int, String, Char, Indesc, Outdesc, Pair; int  Fn;}

extern union s_coercion {
  void Int2D, String2D, Char2D, Indesc2D, Outdesc2D, Pair2D;
  void D2Int, D2String, D2Char, D2Indesc, D2Outdesc, D2Pair;
  int  Fn2D,  D2Fn;
}

extern union uexp {
  int      Int;
  string   String;
  char     Char;
  void     Nil;
  void     False;
  void     True;
  var      Var;
  illambda Lambda;

  *(var,    ilexp)                  Set;
  *(ilexp,  <ilexp>list)            App;
  *(Sast::primop, <ilexp>list)      Op;
  *(<*(string, ilexp) >list, ilexp) Let;
  *(ilexp, ilexp, ilexp)            If;
  <ilexp>list                       Seq;
  *(s_coercion,ilexp)               Coerce;
}
extern struct ilexp { uexp e; tipe t; }
extern struct illambda { <string>list args; ilexp body;}

}}}}

#endif
