
%{
  // Grossman et al, March 1999
  
// TO DO:
  // sharing module for identifiers
  // 1. Add void
  // 2. Add more niceties like one-armed if, when, unless, etc.
  // 3. Like old in-popcorn parser, allow and, or to take arbitrary args.


#include "core.h"
#include "lexing.h"
#include "list.h"

#include "sast.h"
#include "slex.h"
open Core;
open Lexing;
open List;
open Sast;
open Slex;

// don't prefix until trailer to avoid prefixing externs in pop_bison.simple
open   Sparse;

%}

%token NUM ID STRING TRUE FALSE 
%token PRIMOP
%token LPAREN RPAREN QUOTE 
%token NIL LIST BEGIN LAMBDA
%token SET LET LETREC IF COND ELSE AND OR DEFINE

%union {
  int                 Int;
  string              Str;
  exp                 Exp;
  primop              Primop;
  <*(string,exp)>list DefnList;  
  *(string,exp)       Defn;
  <exp>list           ExpList;
  <string>list        IdList;
  lambda              Lambda;
  <*(string,lambda)>list LambdaBinds;
}
%type <Int>       NUM
%type <Str>       ID STRING
%type <Exp>       TRUE FALSE implied_begin exp condpairs
%type <Primop>    PRIMOP
%type <DefnList>  prog deflist binds
%type <Defn>      def
%type <ExpList>   exps ne_exps
%type <IdList>    vars
%type <Lambda>    lambda
%type <LambdaBinds> lambdabinds

%% 

prog:
deflist { success = true; $$ = ^$($1); parse_result = $1; }
;
deflist:
   def         { $$ = ^$(^list($1,null)); }
|  def deflist { $$ = ^$(^list($1,$2));  }
;
def:
  LPAREN DEFINE ID exp RPAREN { $$ = ^$(^($3,$4)); }
| LPAREN DEFINE LPAREN ID vars RPAREN implied_begin RPAREN
   { $$ = ^$(^($4, ^exp.Lambda(^lambda($5, $7)))); }
;
exps:
     /* empty */ { $$ = ^$(null); }
|    ne_exps     { $$ = ^$($1);   }
;
ne_exps:
  exp         { $$ = ^$(^list($1,null)); }
| exp ne_exps { $$ = ^$(^list($1,$2));   }
;
implied_begin:
  ne_exps { $$ = ^$($1.tl == null ? $1.hd : ^exp.Seq($1)); }
;
exp:
  TRUE          { $$ = ^$(^exp.True);                 }
| FALSE         { $$ = ^$(^exp.False);                }
| NIL           { $$ = ^$(^exp.Nil);                  }
| LPAREN RPAREN { $$ = ^$(^exp.Nil);                  }
| NUM           { $$ = ^$(^exp.Int($1));              }
| STRING        { $$ = ^$(^exp.String($1));           }
| ID            { $$ = ^$(^exp.Var(^var($1,yyline))); }
| lambda        { $$ = ^$(^exp.Lambda($1));           }
| LPAREN LIST   exps RPAREN { $$ = ^$(make_list($3)); }
| QUOTE  LPAREN exps RPAREN { $$ = ^$(make_list($3)); }
| LPAREN PRIMOP exps RPAREN 
    { $$ = ^$(desugar_primop($2, $3, yyline));}
| LPAREN LET LPAREN binds RPAREN implied_begin RPAREN 
    { $$ = ^$(^exp.Let(^($4,$6))); }
| LPAREN LETREC LPAREN lambdabinds RPAREN implied_begin RPAREN
    { $$ = ^$(^exp.Letrec(^($4,$6))); }
| LPAREN IF exp exp exp RPAREN { $$ = ^$(^exp.If(^($3,$4,$5))); }
| LPAREN COND condpairs RPAREN  { $$ = ^$($3); }
| LPAREN AND exps RPAREN 
    { $$ = ^$(List::fold_right(desugar_and_f,$3,^exp.True)); }
| LPAREN OR exps RPAREN 
    { $$ = ^$(List::fold_right(desugar_or_f,$3,^exp.False)); }
| LPAREN exp exps RPAREN    { $$ = ^$(^exp.App(^($2,$3))); }
| LPAREN BEGIN exps RPAREN  { $$ = ^$(^exp.Seq($3)); }
| LPAREN SET ID exp RPAREN
    { $$ = ^$(^exp.Set(^(^var($3,yyline),$4))); } 
;
vars:
/* empty */ { $$ = ^$(null); }
| ID vars   { $$ = ^$(^list($1, $2)); }
;
binds:
/* empty */                   { $$ = ^$(null); }
| LPAREN ID exp RPAREN binds  { $$ = ^$(^list(^($2,$3),$5)); }
;
condpairs:
  LPAREN ELSE exp RPAREN            { $$ = ^$($3); }
| LPAREN exp exp RPAREN condpairs { $$ = ^$(^exp.If(^($2,$3,$5))); }
;
lambdabinds:
/* empty */                              { $$ = ^$(null); }
| LPAREN ID lambda RPAREN lambdabinds { $$ = ^$(^list(^($2,$3),$5));}
;
lambda:
  LPAREN LAMBDA LPAREN vars RPAREN implied_begin RPAREN 
     { $$ = ^$(^lambda($4, $6)); }
;

%%

prefix Sparse;

static <*(string,exp)>list parse_result = null;
static bool success = false;

< < <FILE>function_lexbuf_state>lexbuf>Opt lbuf = null;
// in slex.popl: int yylex() { return token(lbuf.v); }

<*(string,exp)>list parse_program(FILE f) { 
  lbuf = ^Opt(from_file(f));
  parse_result = null;
  success      = false;
  yyparse();
  if (!success) raise ParseError();
  return parse_result;
}

exception ParseError;

//////////////////////////////SYNTACTIC NICETIES////////////////////////
static exp Op0 (primop p) {
  return ^exp.Op(^(p, null));
}
static exp Op1 (primop p, exp e) {
  return ^exp.Op(^(p, ^list(e,null)));
}
static exp Op2 (primop p, exp e1, exp e2) {
  return ^exp.Op(^(p, ^list(e1, ^list(e2, null))));
}
static exp make_cons(exp e1, exp e2) {
  return Op2(^primop.Cons, e1, e2);
}
static exp make_list(<exp>list elts) {
  return List::fold_right(make_cons ,elts, ^exp.Nil);
}
static exp make_if (exp iffalse, *(exp,exp) test_and_true) {
  return ^exp.If(^(test_and_true.1, test_and_true.2, iffalse));
}

//////////////////////////DE-SUGARING/////////////////////////////////
static exp desugar_and_f(exp next, exp accum) {
  return ^exp.If(^(next, accum, ^exp.False));
}
static exp desugar_or_f(exp next, exp accum) {
  return ^exp.If(^(next, ^exp.True, accum));
}

// of course we are assuming primops can't be redefined or this is wrong.
// in that case, there's no such thing as a parse-time primop
static exp desugar_primop(primop p, <exp>list args, int line) {
  switch p {
  case Plus:      return exp_fold_right(p, args, ^exp.Int(0));
  case Minus:     return exp_fold_right(p, args, ^exp.Int(0));
  case Times:     return exp_fold_right(p, args, ^exp.Int(1));
  case Div:       return exp_fold_right(p, args, ^exp.Int(1));
  case Inteq:     return exp_and_left(p, args);
  case Less:      return exp_and_left(p, args);
  case Greater:   return exp_and_left(p, args);
  case Lesseq:    return exp_and_left(p, args);
  case Greatereq: return exp_and_left(p, args);
  case Getchar:   return default_input_port(p,  args, line);
  case Peekchar:  return default_input_port(p,  args, line);
  case Putchar:   return default_output_port(p, args, line);
  case Newstring:
    if (args       == null) raise num_args(^(p,line));
    if (args.tl    == null) return Op2(p,args.hd,^exp.Char(chr(0)));
    if (args.tl.tl == null) return Op2(p,args.hd,args.tl.hd);
    else                    raise num_args(^(p,line));
    
  default:
    if (primop_arity(p) != List::length(args))
      raise num_args(^(p,line));
    else
      return ^exp.Op(^(p,args));
  }
}
static exp exp_fold_right(primop p, <exp>list args, exp identity) {
  if (args    == null)  return identity;
  if (args.tl == null)  return Op2(p, identity, args.hd);
  
  exp result = Op2(p, args.hd, args.tl.hd);
  args       = args.tl.tl;
  while (args != null) {
    result = Op2(p, result, args.hd);
    args   = args.tl;
  }
  return result;
}
static exp exp_and_left(primop p, <exp>list args) {
  if (args == null || args.tl == null) return ^exp.True;
  
  exp result = Op2(p, args.hd, args.tl.hd);
  args       = args.tl;
  while (args.tl != null) {
    result =  make_if(^exp.False, ^(result, Op2(p, args.hd, args.tl.hd)));
    args   = args.tl;
  }
  return result;
}
static exp default_output_port(primop p, <exp>list args, int line) {
  if (args       == null)  raise num_args(^(p,line));
  if (args.tl    == null)  return Op2(p, args.hd, Op0(^primop.Currentout));
  if (args.tl.tl == null)  return Op2(p, args.hd, args.tl.hd);
  else                     raise num_args(^(p,line));
}
static exp default_input_port(primop p, <exp>list args, int line) {
  if (args    == null)  return Op1(p, Op0(^primop.Currentin));
  if (args.tl == null)  return Op1(p, args.hd);
  else                  raise num_args(^(p,line));
}
