#ifndef SAST_H
#define SAST_H

#include "list.h"

prefix Sast {
  open Sast {
  open List {

extern union primop { 
  void 
  Plus, Minus, Times, Div,
    Inteq, Ptreq, Not, Less, Greater, Lesseq, Greatereq,
    Isint, Isbool, Isnil, Ispair, Isfn, Ischar, Isstring, Isindesc, Isoutdesc,
    Cons, Car, Cdr, Setcar, Setcdr,
    Openin, Openout, Closein, Closeout, Flushout, 
    Getchar, Peekchar, Putchar, Write,
    Currentin, Currentout,  Winfile, Woutfile, Iseof, 
    Newstring, Sizes, Subs, Sets,
    Chr, Ord;
}
extern struct var {
  string v;
  int    l;
}
extern struct lambda {
  <string>list args;
  exp          body;
}
extern union exp {
  int    Int;
  string String;
  char   Char;
  void   Nil; 
  void   False;
  void   True;
  var    Var; // name, location (for errors)
  lambda Lambda;

  *(var, exp)                     Set;
  *(exp,    <exp>list)            App;
  *(primop, <exp>list)            Op;
  *(<*(string, exp)   >list, exp) Let;
  *(<*(string, lambda)>list, exp) Letrec;
  *(exp, exp, exp)                If;
  <exp>list                       Seq;
}
extern int primop_arity(primop);
extern exception num_args(*(primop,int));
extern void print_arg_msg(*(primop,int));
extern string string_of_op(primop);

}}}
#endif
