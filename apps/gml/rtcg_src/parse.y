
%{
#include "core.h"
#include "lexing.h"
#include "list.h"
#include "string.h"
#include "set.h"

#include "gmlsyntax.h"

open Core;
open Lexing;
open List;
open Gmlsyntax;

open Parse;

%}

%token ID CONSTINT CONSTSTRING CONSTFP 
%token LBRACE RBRACE LBRACKET RBRACKET
%token BIND FAIL INCLUDE
%token TRUE FALSE
%token NEWLINE

%token APPLY GET IF LIGHT NEGI NEGF POINT POINTLIGHT PRINT REAL RENDER SCALE
%token SPOTLIGHT TRANSLATE USCALE ACOS ASIN CLAMPF COS FLOOR FRAC SIN SQRT
%token ADDF DIVF EQF LESSF MULF SUBF ADDI DIVI EQI LESSI MODI MULI SUBI 
%token CONE CUBE CYLINDER PLANE SPHERE 
%token ROTATEX ROTATEY ROTATEZ 
%token DIFFERENCE INTERSECT UNION 
%token GETX GETY GETZ LENGTH
%token TRUE FALSE

%union {
  int    Int;
  FP     Fp;
  string String;
  string Id;
  bool   Bool;
  syn  Syn;
  <syn>list SynList;
  <string>list StringList;
  *(<string>list,<syn>list) Prog;
  prim   Prim;
  prim_fp1 Prim_fp1;
  prim_fp2 Prim_fp2;
  prim_i2  Prim_i2;
  prim_obj Prim_obj;
  prim_obj1 Prim_obj1;
  prim_obj2 Prim_obj2;
  prim_point Prim_point;
}

// Top level Declarations
%type <Prog> prog
%type <StringList> includes
%type <Syn> syn
%type <SynList> syns
%type <Prim> prim
%type <Prim_fp1> prim_fp1
%type <Prim_fp2> prim_fp2
%type <Prim_i2> prim_i2
%type <Prim_obj> prim_obj
%type <Prim_obj1> prim_obj1
%type <Prim_obj2> prim_obj2
%type <Prim_point> prim_point
%type <String> CONSTSTRING
%type <Id>     ID
%type <Int>    CONSTINT
%type <Fp>     CONSTFP

%%

prog: includes syns { _ r = ^(List::rev($1),List::rev($2)); 
                      $$ = ^$(r); @1; success = true; parse_result = r; }
;

includes:
                               { $$ = ^$( null ); }   
| includes INCLUDE CONSTSTRING { $$ = ^$( ^list($3,$1) ); }
;

syns: 
                { $$ = ^$(null); }
|  syns syn { $$ = ^$( ^list($2,$1) ); }
;
 
syn:
  ID                       { $$ = ^$(^syn.Sid($1));      }
| BIND ID                  { $$ = ^$(^syn.Sbind($2));    }
| CONSTINT                 { $$ = ^$(^syn.Spush(^value.Vint($1)));     }
| CONSTSTRING              { $$ = ^$(^syn.Spush(^value.Vstring($1)));  }
| CONSTFP                  { $$ = ^$(^syn.Spush(^value.Vfp($1)));      }
| prim                     { $$ = ^$(^syn.Sprim($1));    }
| LBRACE syns RBRACE       { $$ = ^$(^syn.Sfun(List::rev($2)));     }
| LBRACKET syns RBRACKET   { $$ = ^$(^syn.Sarray(List::rev($2)));   }
| TRUE                     { $$ = ^$(^syn.Spush(^value.Vbool(true)));  }
| FALSE                    { $$ = ^$(^syn.Spush(^value.Vbool(false))); }
;
  
prim:
  APPLY      { $$ = ^$(^prim.Apply     ); }
| GET        { $$ = ^$(^prim.Get       ); }
| IF         { $$ = ^$(^prim.If        ); }
| LENGTH     { $$ = ^$(^prim.Length    ); }
| LIGHT      { $$ = ^$(^prim.Light     ); }
| NEGI       { $$ = ^$(^prim.Negi      ); }
| NEGF       { $$ = ^$(^prim.Negf      ); }
| POINT      { $$ = ^$(^prim.Point     ); }
| POINTLIGHT { $$ = ^$(^prim.Pointlight); }
| PRINT      { $$ = ^$(^prim.Print     ); }
| REAL       { $$ = ^$(^prim.Real      ); }
| RENDER     { $$ = ^$(^prim.Render    ); }
| SCALE      { $$ = ^$(^prim.Scale     ); }
| SPOTLIGHT  { $$ = ^$(^prim.Spotlight ); }
| TRANSLATE  { $$ = ^$(^prim.Translate ); }
| prim_fp1   { $$ = ^$(^prim.Fp1($1)   ); }
| prim_fp2   { $$ = ^$(^prim.Fp2($1)   ); }
| prim_i2    { $$ = ^$(^prim.I2($1)    ); }
| prim_obj   { $$ = ^$(^prim.Obj($1)   ); }
| prim_obj1  { $$ = ^$(^prim.Obj1($1)  ); }
| prim_obj2  { $$ = ^$(^prim.Obj2($1)  ); }
| prim_point { $$ = ^$(^prim.Prim_Point($1)); }
;

prim_fp1:
  ACOS   { $$ = ^$(^prim_fp1.Acos  ); }
| ASIN   { $$ = ^$(^prim_fp1.Asin  ); }
| CLAMPF { $$ = ^$(^prim_fp1.Clampf); }
| COS    { $$ = ^$(^prim_fp1.Cos   ); }
| FLOOR  { $$ = ^$(^prim_fp1.Floor ); }
| FRAC   { $$ = ^$(^prim_fp1.Frac  ); }
| SIN    { $$ = ^$(^prim_fp1.Sin   ); }
| SQRT   { $$ = ^$(^prim_fp1.Sqrt  ); }
;

prim_fp2:
  ADDF  { $$ = ^$(^prim_fp2.Addf  ); }
| DIVF  { $$ = ^$(^prim_fp2.Divf  ); }
| EQF   { $$ = ^$(^prim_fp2.Eqf   ); }
| LESSF { $$ = ^$(^prim_fp2.Lessf ); }
| MULF  { $$ = ^$(^prim_fp2.Mulf  ); }
| SUBF  { $$ = ^$(^prim_fp2.Subf  ); }
;

prim_i2:
  ADDI  { $$ = ^$(^prim_i2.Addi  ); }
| DIVI  { $$ = ^$(^prim_i2.Divi  ); }
| EQI   { $$ = ^$(^prim_i2.Eqi   ); }
| LESSI { $$ = ^$(^prim_i2.Lessi ); }
| MODI  { $$ = ^$(^prim_i2.Modi  ); }
| MULI  { $$ = ^$(^prim_i2.Muli  ); }
| SUBI  { $$ = ^$(^prim_i2.Subi  ); }

prim_obj:
  CONE     { $$ = ^$(^prim_obj.Cone     ); }
| CUBE     { $$ = ^$(^prim_obj.Cube     ); }
| CYLINDER { $$ = ^$(^prim_obj.Cylinder ); }
| PLANE    { $$ = ^$(^prim_obj.Plane    ); }
| SPHERE   { $$ = ^$(^prim_obj.Sphere   ); }

prim_obj1:
  ROTATEX  { $$ = ^$(^prim_obj1.Rotatex ); }
| ROTATEY  { $$ = ^$(^prim_obj1.Rotatey ); }
| ROTATEZ  { $$ = ^$(^prim_obj1.Rotatez ); }
| USCALE   { $$ = ^$(^prim_obj1.Uscale  ); }

prim_obj2: 
  DIFFERENCE { $$ = ^$(^prim_obj2.Difference ); }
| INTERSECT  { $$ = ^$(^prim_obj2.Intersect  ); }
| UNION      { $$ = ^$(^prim_obj2.Union      ); }
;

prim_point:
  GETX   { $$ = ^$(^prim_point.Getx   ); }
| GETY   { $$ = ^$(^prim_point.Gety   ); }
| GETZ   { $$ = ^$(^prim_point.Getz   ); }
;

%%

prefix Parse;

static bool success = false;
static *(<string>list,<syn>list) parse_result = ^(null,null);

< < <FILE>function_lexbuf_state>lexbuf>Opt lbuf = null;
exception ParseError;

*(<string>list,<syn>list) parse_program(FILE f) { 
  yylloc = ^yyltype(0,0,0,0,0,"");
  lbuf = ^Opt(from_file(f));  
  parse_result = ^(null,null);
  success      = false;
  yyparse();
  if (!success) raise ParseError();
  return parse_result;
}
