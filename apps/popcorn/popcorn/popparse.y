
%{

  // DUMMYLOC is probably a bad idea, but I can't think of how else
  // to hack it without pulling hacks with Bison

#include "core.h"
#include "lexing.h"
#include "list.h"
#include "string.h"
#include "set.h"
#include "gcdfec.h"
#include "poperr.h"
#include "popsyntax.h"
open Core;
open Lexing;
open List;
open Popsyntax;
open Gcdfec;

// defer prefix until trailer to avoid prefixing externs in pop_bison.simple
open Popparse;

#define SURROUND(s,e) Gcdfec::seg_of_abs(s.first_line,e.last_line)
#define DUMMYLOC      Gcdfec::seg_of_abs(0,0)

YYSTYPE nullStringList  = ^YYSTYPE.StringList(null);
YYSTYPE nullTypeModList = ^YYSTYPE.TypeModifierList(null);

%}

%token LPAREN RPAREN LBRACE RBRACE LCBRACE RCBRACE LBRACKET RBRACKET 
%token LCBRACKET RCBRACKET
%token EQUALS SEMICOLON QUESTION COLON COLONCOLON DOT COMMA AT 

%token ID
%token CONSTINT CONSTTRUE CONSTFALSE CONSTSTRING CONSTCHAR CONSTFLOAT CONSTDBL
%token TUPLEOFFSET

%token LESSTHAN GREATERTHAN LESSTHANEQ GREATERTHANEQ EE NE 
%token AMPERAMPER PIPEPIPE BANG 

%token PLUS MINUS TIMES DIV PERCENT 
%token AMPER PIPE CARET TILDE LESSLESS GREATERGREATER 
%token GREATERGREATERGREATER

%token VOID BOOL CHAR BYTE SHORT INT STRING EXN SIGNED UNSIGNED CONST 
%token FLOAT DOUBLE
%token EXTERN STATIC PUBLIC ABSTRACT STRUCT UNION EXCEPTION

%token RETURN IF ELSE FOR WHILE DO BREAK CONTINUE SWITCH CASE DEFAULT
%token HANDLE RAISE TRY
%token PREFIX OPEN
%token ABSTYPE WITH
%token ARRAY
%token CODEGEN CUT SPLICE FILL
%token UNDERSCORE FUN CATCH FINALLY SPRINTF
%token RDTSC
%token CDECL STDCALL

%token NEW NULL SIZE PRINTF FPRINTF CHR ORD

%right PLUSPLUS MINUSMINUS TILDE BANG
%left  TIMES DIV PERCENT
%left  PLUS MINUS
%left  LESSLESS GREATERGREATER GREATERGREATERGREATER
%left  LESSTHAN LESSTHANEQ GREATERTHAN GREATERTHANEQ
%left  EE NE
%left  AMPER
%left  CARET
%left  PIPE
%left  AMPERAMPER
%left  PIPEPIPE
%right EQUALS PLUSEQUAL MINUSEQUAL TIMESEQUAL DIVEQUAL MODEQUAL
%right AMPEREQUAL PIPEEQUAL CARETEQUAL
%right LESSLESSEQ GREATERGREATEREQ GREATERGREATERGREATEREQ

%union {
  char   Char;
  int    Int;
  bool   Bool;
  string String;
  float Float;
  double Double;
  convention Convention;
  <top_decl>list            TopDeclList;
  top_decl                  TopDecl;
  <string>list              StringList;
  <type_modifier>list       TypeModifierList;
  <*(<string>Opt, typ)>list ParamList;
  *(<string>Opt,typ)        ParamDecl;
  <*(string, typ)>list      StringTypList;
  *(string,typ)             StringTyp;
  size_of                   SizeOf;
  typ                       Typ;
  scope                     Scope;
  capability                Capability;
  <*(string,<type_modifier>list)>list StringTypeModifierList;
  <*(string,capability,typ)>list StructFields;

  stmt                                        Stmt;
  <*(var,typ,<exp>Opt)>list                   VarDecls;
  <*(var,<type_modifier>list,<exp>Opt)>list   InitList;
  *(var,<type_modifier>list,<exp>Opt)         InitVar;
  <*(var,<type_modifier>list,exp_or_fun)>list LocalInitList;  
  *(var,<type_modifier>list,exp_or_fun)       LocalInitVar;  
  *(switch_clause,<stmt>Opt)                  SwitchClauses;
  prim_pattern                                PrimPat;
  <prim_pattern>list                          PrimPatList;

  exp                                         Exp;
  <exp>list                                   ExpList;
  <*(<field_name>Opt,exp)>list                LabelledExpList;
  primop                                      Primop;
  <string>Opt                                 StringOpt;
}

// Top level Declarations
%type <TopDeclList> prog top_decls top_decl extern_decl global_var_decl
%type <TopDecl> prefix_decl open_decl func_decl struct_decl union_decl exn_decl
%type <TopDecl> abs_decl
// Types
%type <StringList> type_params tyvar_list abstype_params abstyvar_list
%type <TypeModifierList> type_mods
%type <ParamList> param_list
%type <ParamDecl> param_decl
%type <StringTypList> named_param_list
%type <StringTyp> named_param_decl
%type <SizeOf> num_type
%type <Typ> signed_type prim_type prim_type_or_wild
%type <Bool> static_opt question_opt
%type <Scope> scope_opt
%type <Capability> const_opt
%type <StringTypList> iddecl union_field_decls union_field_decl
%type <StringTypeModifierList> ibl
%type <StructFields> struct_field_decls struct_field_decl
%type <StringOpt> pid_or_array
%type <Convention> convention

// Statements
%type <Stmt> block stmts stmt stmt_no_short_if for_stmt for_stmt_no_short_if
%type <Stmt> stmt_no_trailing
%type <VarDecls> var_decl
%type <InitList> init_list
%type <InitVar>  init_var
%type <LocalInitList> local_init_list
%type <LocalInitVar> local_init_var
%type <SwitchClauses> switch_clauses

// Expressions
%type <Exp> exp_opt bool_exp_opt exp comma_free_exp
%type <Exp> exp1 exp2 exp3 exp35 exp4 exp5 exp6 exp7 exp8 exp9 exp10 exp11
%type <Primop> exp4op exp5op exp6op exp7op exp8op exp9op exp10op exp11op
%type <Exp> exp12 exp13 exp14 exp15
%type <Primop> exp15op
%type <ExpList> exps
%type <LabelledExpList> labelled_exps

// Patterns
%type <PrimPat> prim_pat
%type <PrimPatList> prim_pat_list

// Misc and Lexer
%type <StringOpt> id_opt
%type <String> pid
%type <String> ID CONSTSTRING const_string
%type <Char>   CONSTCHAR
%type <Int>    CONSTINT
%type <Int>    TUPLEOFFSET
%type <Float>  CONSTFLOAT
%type <Double> CONSTDBL
%%

prog: 
  top_decls { $$ = $!1; success = true; parse_result = $1; }
;

//////////////////////// Top Level Declarations //////////////////////
top_decls:
  top_decl { $$ = $!1; }
| PREFIX pid SEMICOLON top_decls 
  { $$= ^$(^list(^top_decl(^raw_top_decl.PrefixDecl(^($2,$4)),
                           SURROUND(@1, @4)),
                  null));
  }
| OPEN pid SEMICOLON top_decls
  { $$= ^$(^list(^top_decl(^raw_top_decl.OpenDecl(^($2,$4)),
                           SURROUND(@1, @4)),
                  null));
  }
| top_decl top_decls
 {
  $$=^$(List::append($1,$2));
 }
;
top_decl: 
  prefix_decl     { $$ = ^$(^list($1,null)); }
| open_decl       { $$ = ^$(^list($1,null)); }
| func_decl       { $$ = ^$(^list($1,null)); }
| struct_decl     { $$ = ^$(^list($1,null)); }
| union_decl      { $$ = ^$(^list($1,null)); }
| abs_decl        { $$ = ^$(^list($1,null)); }
| exn_decl        { $$ = ^$(^list($1,null)); }
| global_var_decl { $$ = $!1; }
| extern_decl     { $$ = $!1; }
;
prefix_decl:
 PREFIX pid LBRACE top_decls RBRACE 
 { $$= ^$(^top_decl(^raw_top_decl.PrefixDecl(^($2,$4)),
                    SURROUND(@1, @4)));
 }
;
open_decl:
 OPEN pid LBRACE top_decls RBRACE 
 { $$= ^$(^top_decl(^raw_top_decl.OpenDecl(^($2,$4)),
                    SURROUND(@1, @4)));
 }
;
func_decl:
 static_opt prim_type pid type_mods block
  { 
    Gcdfec::seg sg = SURROUND(@1,@5);
    fndecl fd = make_fndecl(sg, $1, $2, $3, $4, $5);
    $$=^$(^top_decl(^raw_top_decl.FunDecl(fd),sg));
  }
;
struct_decl:
 scope_opt question_opt STRUCT type_params pid LBRACE struct_field_decls RBRACE
 { $$ = ^$(^top_decl(^raw_top_decl.StructDecl(^structdecl($1,$5,$4,$2,$7)),
                     SURROUND(@1,@8)));
 }
;
union_decl:
 scope_opt UNION type_params pid LBRACE union_field_decls RBRACE
 { $$ = ^$(^top_decl(^raw_top_decl.UnionDecl(^uniondecl($1,$4,$3,$6)),
                     SURROUND(@1,@7)));
 }
;                   
abs_decl:
  scope_opt ABSTYPE type_params pid abstype_params EQUALS param_decl SEMICOLON
 { $$ = ^$(^top_decl(^raw_top_decl.AbsDecl(^absdecl($1,$4,$3,$5,snd($7))),
                     SURROUND(@1,@8)));
 }
;
exn_decl:
  scope_opt EXCEPTION pid LPAREN param_decl RPAREN SEMICOLON
 { $$ = ^$(^top_decl(^raw_top_decl.ExceptionDecl(^($3,$1,$5.2)),
                      SURROUND(@1,@7)));
 }
| scope_opt EXCEPTION pid SEMICOLON
 { $$ = ^$(^top_decl(^raw_top_decl.ExceptionDecl(^($3,$1,^typ.VoidType)),
                      SURROUND(@1,@4)));
 }
;
global_var_decl:
 static_opt prim_type init_list SEMICOLON
  { scope sc = $1 ? ^scope.Static : ^scope.Public;
    seg   sg = SURROUND(@1,@4);
    
    $$ = ^$(List::map_c(make_global_decl, ^(sc,sg), 
			List::map_c(make_var_decl, $2, $3)));
  }
;
extern_decl:
  EXTERN var_decl
  { 
    seg sg = SURROUND(@1,@2);
    $$ = ^$(List::map_c(make_extern_val, sg, $2));
  }
| EXTERN pid question_opt type_params SEMICOLON
  { $$ = ^$(^list(^top_decl(^raw_top_decl.ExternType(^($2,$4,$3)),
                           SURROUND(@1,@5)),
                  null));
  }
;

//////////////////////////// Types and Declarations ////////////////////////
type_params:
                      { $$=nullStringList; }// empty case
| LESSTHAN tyvar_list { $$=$!2;   }
;
tyvar_list:
  ID GREATERTHAN      { $$=^$(^list($1,null)); }
| ID COMMA tyvar_list { $$=^$(^list($1,$3));   }
;
abstype_params:
  LBRACKET abstyvar_list { $$=$!2; }
;
abstyvar_list:
  ID RBRACKET            { $$=^$(^list($1,null)); }
| ID COMMA abstyvar_list { $$=^$(^list($1,$3)); }
;
type_mods:
  { $$=nullTypeModList; }//empty case
| type_mods LBRACKET RBRACKET 
       { $$=^$(^list(^type_modifier.Array(null), $1)); }
| type_mods LBRACKET exp RBRACKET
       { $$=^$(^list(^type_modifier.Array(^Opt($3)), $1)); }
| type_mods LCBRACKET CONSTINT RCBRACKET
       { $$= ^$(^list(^type_modifier.CArray(^($3,false)), $1)); }
| type_mods QUESTION LCBRACKET CONSTINT RCBRACKET
       { $$= ^$(^list(^type_modifier.CArray(^($4,true)), $1)); }
| type_mods convention type_params LPAREN RPAREN 
       { $$=^$(^list(^type_modifier.Params(^($2,$3,null)), $1)); }
| type_mods convention type_params LPAREN param_list RPAREN
       { $$=^$(^list(^type_modifier.Params(^($2,$3,$5)), $1)); }
;
param_list:
  param_decl                  { $$=^$(^list($1,null)); }
| param_decl COMMA param_list { $$=^$(^list($1,$3));  }
;
param_decl:
  prim_type ID type_mods { $$=^$(^(^Opt($2), make_type($1, $3))); }
| prim_type type_mods    { $$=^$(^(null,     make_type($1, $2))); }
named_param_list:
  named_param_decl                        { $$=^$(^list($1,null)); }
| named_param_decl COMMA named_param_list { $$ =^$(^list($1,$3));  }
;
named_param_decl:
  prim_type ID type_mods { $$=^$(^($2, make_type($1,$3))); }
;
num_type:
  INT   { $$ = ^$(^size_of.B4); }
| SHORT { $$ = ^$(^size_of.B2); }
| BYTE  { $$ = ^$(^size_of.B1); }
;
signed_type:
  SIGNED   num_type { $$ = ^$(^typ.IntType(^(true,  $2))); }
| UNSIGNED num_type { $$ = ^$(^typ.IntType(^(false, $2))); }
| num_type          { $$ = ^$(^typ.IntType(^(true,  $1))); }
;
prim_type:
  VOID                           { $$ = ^$(^typ.VoidType); }
| signed_type                    { $$ = $!1; }
| BOOL                           { $$ = ^$(^typ.BooleanType); }
| CHAR                           { $$ = ^$(^typ.CharType); }
| STRING                         { $$ = ^$(^typ.StringType); }
| FLOAT                          { $$ = ^$(^typ.FloatType); }
| DOUBLE                         { $$ = ^$(^typ.DoubleType); }
| TIMES LPAREN RPAREN            { $$ = ^$(^typ.TupleType(null)); }
| TIMES LPAREN param_list RPAREN 
    { $$ = ^$(^typ.TupleType(List::map(snd_stropt_typ,$3))); }
| EXN                            { $$ = ^$(^typ.ExnType); }
| LESSTHAN param_list GREATERTHAN pid_or_array 
    { $$= ^$(name_or_array($4,List::map(snd_stropt_typ,$2),SURROUND(@1,@4))); }
| LESSLESS param_list GREATERTHAN pid_or_array GREATERTHAN pid_or_array
    { $$= ^$(name_or_array($6,
			   ^list(name_or_array($4,
					       List::map(snd_stropt_typ,$2),
					       SURROUND(@1,@4)),
				 null),SURROUND(@1,@6))); 
    }
| LESSLESS param_list GREATERTHAN pid_or_array COMMA param_list GREATERTHAN pid_or_array
    { $$=^$(name_or_array($8,
			  ^list(name_or_array($4,List::map(snd_stropt_typ,$2),
					      SURROUND(@1,@4)),
				List::map(snd_stropt_typ,$6)),
			  SURROUND(@1,@8))); 
    }
| pid { $$ = ^$(^typ.MutableTyp(^(^typ.UnresolvedTyId(^($1,null))))); }
;
static_opt:
         { $$=^$(false);  } // empty case
| STATIC { $$=^$(true); }
;
scope_opt:
           { $$=^$(^scope.Public); } // empty case
| STATIC   { $$=^$(^scope.Static); }
| ABSTRACT { $$=^$(^scope.Abstract); }
| EXTERN   { $$=^$(^scope.Extern); }
| PUBLIC   { $$=^$(^scope.Public); }
;
question_opt:
           { $$=^$(false);  } // empty case
| QUESTION { $$=^$(true); }
;
const_opt:
        { $$=^$(^capability.ReadWrite);  } // empty case
| CONST { $$=^$(^capability.ReadOnly); }
;
iddecl:
  prim_type ibl { $$=^$(List::map_c(make_id_decl, $1, $2)); }
;
ibl:
  ID type_mods           { $$=^$(^list(^($1,$2),null)); }
| ID type_mods COMMA ibl { $$=^$(^list(^($1,$2),$4));   }
;
struct_field_decls:
                                       { $$=^$(null); } // empty case
| struct_field_decl struct_field_decls { $$=^$(List::append($1,$2)); }
;
struct_field_decl:
  const_opt iddecl SEMICOLON 
      { $$=^$(List::map_c(make_struct_field_decl, $1, $2)); }
union_field_decls:
                                       { $$=^$(null); } // empty case
| union_field_decl union_field_decls { $$=^$(List::append($1,$2)); }
;
union_field_decl:
  iddecl SEMICOLON { $$=$!1; }
;
/////////////////////////// Statements /////////////////////////////
block:
  LBRACE stmts RBRACE { $$=$!2; }
;
stmts:
  stmt { $$=$!1; }
| var_decl stmts { $$=^$(make_var_stmt($1,$2,             SURROUND(@1,@2))); }
| ID COLON stmts { $$=^$(^stmt(^raw_stmt.Label(^($1,$3)), SURROUND(@1,@3))); }
| stmt stmts     { $$=^$(^stmt(^raw_stmt.Seq(^($1,$2)),   SURROUND(@1,@2))); }
;
var_decl:
  prim_type_or_wild local_init_list SEMICOLON 
  { $$=^$(List::map_c(make_local_var_decl, $1, $2)); }
;
prim_type_or_wild:
  prim_type  { $$=$!1; }
| UNDERSCORE { $$=^$(new_evar(^var_class.Any)); }
;
local_init_list:
  local_init_var { $$=^$(^list($1,null)); }
| local_init_var COMMA local_init_list { $$=^$(^list($1,$3)); }
;
local_init_var:
  pid type_mods { $$=^$(^($1,$2,^exp_or_fun.None)); }
| pid type_mods EQUALS comma_free_exp { $$=^$(^($1,$2,^exp_or_fun.Exp($4))); }
| pid type_mods block { $$=^$(^($1,$2,^exp_or_fun.Block($3))); }
;
init_list:
  init_var                 { $$=^$(^list($1,null)); }
| init_var COMMA init_list { $$=^$(^list($1,$3));   }
;
init_var:
  pid type_mods                       { $$=^$(^($1,$2,null));     }
| pid type_mods EQUALS comma_free_exp { $$=^$(^($1,$2,^Opt($4))); }
;
stmt:
  stmt_no_trailing { $$=$!1; }
| IF LPAREN exp RPAREN stmt 
     { $$=^$(^stmt(^raw_stmt.IfThenElse(^($3,$5, 
                                    ^stmt(^raw_stmt.Skip,DUMMYLOC))),
              SURROUND(@1,@5))); 
     }
| IF LPAREN exp RPAREN stmt_no_short_if ELSE stmt
    { $$=^$(^stmt(^raw_stmt.IfThenElse(^($3,$5,$7)), SURROUND(@1,@7))); }
| WHILE LPAREN exp RPAREN stmt
    { $$=^$(^stmt(^raw_stmt.While(^($3,$5)),         SURROUND(@1,@5))); }
| for_stmt { $$=$!1;}
| CUT stmt 
    { $$=^$(^stmt(^raw_stmt.Cut($2),                 SURROUND(@1,@2))); }
| SPLICE stmt
    { $$=^$(^stmt(^raw_stmt.Splice($2),              SURROUND(@1,@2))); }
| TRY stmt HANDLE pid stmt
    { $$=^$(^stmt(^raw_stmt.TryHandle(^($2,$4,$5)),  SURROUND(@1,@2))); }
| TRY stmt CATCH LBRACE switch_clauses RBRACE 
    { switch_clause cs = $5.1;
      <stmt>Opt d = $5.2;
      Gcdfec::seg loc = SURROUND(@1,@6);
      switch cs {
      case UnionClauses(cs):
	$$=^$(^stmt(^raw_stmt.TryCatchFinally
	              (^($2,List::map(convert_union_clause,cs),d,null)),
                    loc));
      default: abort("case is not an exception pattern within catch",loc);
      }
    }
| TRY stmt FINALLY stmt
    { $$=^$(^stmt(^raw_stmt.TryCatchFinally(^($2,null,null,^Opt($4))),
                  SURROUND(@1,@4)));
    }
| WITH ID abstype_params EQUALS exp DO stmt
    { $$=^$(^stmt(^raw_stmt.With(^($2,null,$3,$5,$7)), SURROUND(@1,@7))); }  
;
stmt_no_short_if:
  stmt_no_trailing { $$=$!1; }
| IF LPAREN exp RPAREN stmt_no_short_if ELSE stmt_no_short_if
    { $$=^$(^stmt(^raw_stmt.IfThenElse(^($3,$5,$7)), SURROUND(@1,@7))); }
| WHILE LPAREN exp RPAREN stmt_no_short_if
    { $$=^$(^stmt(^raw_stmt.While(^($3,$5)),         SURROUND(@1,@5))); }
| for_stmt_no_short_if { $$=$!1;}
| CUT stmt_no_short_if
    { $$=^$(^stmt(^raw_stmt.Cut($2),                 SURROUND(@1,@2))); }
| SPLICE stmt_no_short_if
    { $$=^$(^stmt(^raw_stmt.Splice($2),              SURROUND(@1,@2))); }
| TRY stmt HANDLE pid stmt_no_short_if
    { $$=^$(^stmt(^raw_stmt.TryHandle(^($2,$4,$5)),  SURROUND(@1,@2))); }
| TRY stmt CATCH LBRACE switch_clauses RBRACE 
    { switch_clause cs = $5.1;
      <stmt>Opt d = $5.2;
      Gcdfec::seg loc = SURROUND(@1,@6);
      switch cs {
      case UnionClauses(cs):
	$$=^$(^stmt(^raw_stmt.TryCatchFinally
	              (^($2,List::map(convert_union_clause,cs),d,null)),
                    loc));
      default: abort("case is not an exception pattern within catch",loc);
      }
    }
| TRY stmt FINALLY stmt_no_short_if
    { $$=^$(^stmt(^raw_stmt.TryCatchFinally(^($2,null,null,^Opt($4))),
                  SURROUND(@1,@4)));
    }
| WITH ID abstype_params EQUALS exp DO stmt_no_short_if
    { $$=^$(^stmt(^raw_stmt.With(^($2,null,$3,$5,$7)), SURROUND(@1,@7))); }  
;
for_stmt:
  FOR LPAREN exp_opt SEMICOLON bool_exp_opt SEMICOLON exp_opt RPAREN stmt
      { $$=^$(^stmt(^raw_stmt.For(^($3,$5,$7,$9)), SURROUND(@1,@9))); }
| FOR LPAREN var_decl bool_exp_opt SEMICOLON exp_opt RPAREN stmt
      { $$=^$(make_var_stmt
         ($3, 
          ^stmt(^raw_stmt.For(^(make_exp(^raw_exp.Nop, SURROUND(@1,@8)),
	                        $4, $6, $8)),
                  SURROUND(@1,@8)),
	  SURROUND(@1,@8)));
      }
;
for_stmt_no_short_if:
 FOR LPAREN exp_opt SEMICOLON bool_exp_opt SEMICOLON exp_opt RPAREN stmt_no_short_if
      { $$=^$(^stmt(^raw_stmt.For(^($3,$5,$7,$9)), SURROUND(@1,@9))); }
| FOR LPAREN var_decl bool_exp_opt SEMICOLON exp_opt RPAREN stmt_no_short_if
      { $$=^$(make_var_stmt
         ($3, 
          ^stmt(^raw_stmt.For(^(make_exp(^raw_exp.Nop, SURROUND(@1,@8)),
	                        $4, $6, $8)),
                  SURROUND(@1,@8)),
	  SURROUND(@1,@8)));
      }
;
exp_opt:
      { $$=^$(make_exp(^raw_exp.Nop, DUMMYLOC)); } // empty case
| exp { $$=$!1; }
;
bool_exp_opt:
      { $$=^$(make_exp(^raw_exp.Const(^cnst.Bool(true)), 
                   DUMMYLOC)); } // empty case
| exp { $$=$!1; }
;
switch_clauses:
                      { $$=^$(^(^switch_clause.AnyClause,null)); } // empty case
| DEFAULT COLON stmts { $$=^$(^(^switch_clause.AnyClause,^Opt($3))); }
| CASE pid LPAREN prim_pat RPAREN COLON stmts switch_clauses
      { $$=^$(add_union_clause($2,^pattern.Prim($4),$7,$8,SURROUND(@1,@8))); }
| CASE pid TIMES LPAREN RPAREN COLON stmts switch_clauses
      { $$=^$(add_union_clause($2,^pattern.Tuple(null),$7,$8,
                               SURROUND(@1,@8))); }
| CASE pid TIMES LPAREN prim_pat_list RPAREN COLON stmts switch_clauses
      { $$=^$(add_union_clause($2,^pattern.Tuple($5),$8,$9,
                               SURROUND(@1,@9))); }
| CASE pid COLON stmts switch_clauses
      { $$=^$(add_union_clause($2,^pattern.None,$4,$5,SURROUND(@1,@5))); }
| CASE CONSTINT COLON stmts switch_clauses
      { $$=^$(add_int_clause($2, $4, $5,             SURROUND(@1,@5))); }
| CASE MINUS CONSTINT COLON stmts switch_clauses
      { $$=^$(add_int_clause(0-$3, $5, $6,           SURROUND(@1,@6))); }
| CASE CONSTCHAR COLON stmts switch_clauses
      { $$=^$(add_char_clause($2, $4, $5,            SURROUND(@1,@5))); }
;
stmt_no_trailing:
  block { $$=$!1; }
| RETURN SEMICOLON 
      { $$=^$(^stmt(^raw_stmt.Return(null),     SURROUND(@1,@2))); }
| RETURN exp SEMICOLON
      { $$=^$(^stmt(^raw_stmt.Return(^Opt($2)), SURROUND(@1,@3))); }
| BREAK id_opt SEMICOLON
      { $$=^$(^stmt(^raw_stmt.Break($2),        SURROUND(@1,@3))); }
| CONTINUE id_opt SEMICOLON
      { $$=^$(^stmt(^raw_stmt.Continue($2),     SURROUND(@1,@3))); }
| PRINTF LPAREN const_string RPAREN SEMICOLON
      { $$=^$(expand_printf(SURROUND(@1,@5),
                            make_exp(^raw_exp.Var("tal_stdout"),
			    SURROUND(@1,@1)),$3,null)); }
| PRINTF LPAREN const_string COMMA exps RPAREN SEMICOLON
      { $$=^$(expand_printf(SURROUND(@1,@7),
                            make_exp(^raw_exp.Var("tal_stdout"),
			    SURROUND(@1,@1)),$3,$5)); }
| FPRINTF LPAREN pid COMMA const_string RPAREN SEMICOLON
      { $$=^$(expand_printf(SURROUND(@1,@7),
			    make_exp(^raw_exp.Var($3),
			    SURROUND(@3,@3)),$5,null)); }
| FPRINTF LPAREN pid COMMA const_string COMMA exps RPAREN SEMICOLON
      { $$=^$(expand_printf(SURROUND(@1,@9),
			    make_exp(^raw_exp.Var($3),
			    SURROUND(@3,@3)),$5,$7)); }
| exp SEMICOLON
      { $$=^$(^stmt(^raw_stmt.Exp($1),          SURROUND(@1,@2))); }
| SEMICOLON
      { $$=^$(^stmt(^raw_stmt.Skip,             SURROUND(@1,@1))); }
| SWITCH exp LBRACE switch_clauses RBRACE
    { 
      raw_stmt rs;
      
      switch $4.1 {
      case IntClauses(cs):
	bool t = $4.2 == null;
	if (t) {
	// poptype.ml bug: if ($4.2 == null) {
	  err(^Poperr::parseError.Syntax("switch requires default"), 
	      SURROUND(@1,@5));
          rs = ^raw_stmt.Skip;
        } else 
          rs = ^raw_stmt.IntSwitch(^($2,cs,$4.2.v));
      case CharClauses(cs):
	bool t = $4.2 == null;
	if (t) {
	// poptype.ml bug: if ($4.2 == null) {
	  err(^Poperr::parseError.Syntax("switch requires default"), 
	      SURROUND(@1,@5));
          rs = ^raw_stmt.Skip;
        }
        rs = ^raw_stmt.CharSwitch(^($2,cs,$4.2.v));
      case UnionClauses(cs):
        rs = ^raw_stmt.UnionSwitch(^($2,
				     List::map(convert_union_clause,cs),$4.2));
      case AnyClause:
        err(^Poperr::parseError.Syntax("No non-default clauses in switch"), 
	    SURROUND(@1,@5));
        rs = ^raw_stmt.Skip;
      }
      $$=^$( ^stmt(rs,SURROUND(@1,@5)));
   }
| DO stmt WHILE LPAREN exp RPAREN SEMICOLON
      { $$=^$(^stmt(^raw_stmt.Do(^($2,$5)),    SURROUND(@1,@7))); }
| RDTSC exp COLON exp SEMICOLON
      { $$=^$(^stmt(^raw_stmt.Rdtsc(^($2,$4)),SURROUND(@1,@5))); }
;
//////////////////////////// Expressions ///////////////////////////

exp1:
  LPAREN exp RPAREN { $$=$!2; }
| pid          
    { $$=^$(make_exp(^raw_exp.Var($1),                 SURROUND(@1,@1))); }
| CONSTINT     
    { $$=^$(make_exp(^raw_exp.Const(^cnst.Int($1)),    SURROUND(@1,@1))); }
| CONSTTRUE 
    { $$=^$(make_exp(^raw_exp.Const(^cnst.Bool(true)),   SURROUND(@1,@1))); }
| CONSTFALSE 
    { $$=^$(make_exp(^raw_exp.Const(^cnst.Bool(false)),   SURROUND(@1,@1))); }
| const_string  
    { $$=^$(make_exp(^raw_exp.Const(^cnst.String($1)), SURROUND(@1,@1))); }
| CONSTCHAR    
    { $$=^$(make_exp(^raw_exp.Const(^cnst.Char($1)),   SURROUND(@1,@1))); }
| CONSTFLOAT
    { $$=^$(make_exp(^raw_exp.Const(^cnst.Float($1)), SURROUND(@1,@1))); } 
| CONSTDBL
    { $$=^$(make_exp(^raw_exp.Const(^cnst.Double($1)), SURROUND(@1,@1))); }
| LBRACE COLON prim_type type_mods RBRACE
    { $$= ^$(make_exp(^raw_exp.ConstArray(^(null,^Opt(make_type($3,$4)))),
                      SURROUND(@1,@5))); }
| LBRACE exps RBRACE
   { $$=^$(make_exp(^raw_exp.ConstArray(^($2,null)),   SURROUND(@1,@3))); }
| LCBRACE exps RCBRACE
   { $$ = ^$(make_exp(^raw_exp.ConstCArray($2), SURROUND(@1,@3))); }
| NULL
   { $$=^$(make_exp(^raw_exp.Const(^cnst.Null),        SURROUND(@1,@1))); }
| SIZE LPAREN exp RPAREN 
   { $$=^$(make_exp(^raw_exp.Primop(^(^primop.Size,^list($3,null))), 
                    SURROUND(@1,@4))); }
| ORD LPAREN exp RPAREN 
   { $$=^$(make_exp(^raw_exp.Primop(^(^primop.Ord,^list($3,null))), 
                    SURROUND(@1,@4))); }
| CHR LPAREN exp RPAREN 
   { $$=^$(make_exp(^raw_exp.Primop(^(^primop.Chr,^list($3,null))), 
                    SURROUND(@1,@4))); }
| RAISE pid LPAREN RPAREN
   { $$=^$(make_exp(^raw_exp.Raise(make_exp(^raw_exp.NewExn(^($2,null)), 
                                   SURROUND(@1,@4))), 
                    SURROUND(@1,@4))); }
| RAISE pid LPAREN exp RPAREN
   { $$=^$(make_exp(^raw_exp.Raise(make_exp(^raw_exp.NewExn(^($2,^Opt($4))), 
                                   SURROUND(@1,@5))), 
                    SURROUND(@1,@5))); }
| RAISE LPAREN exp RPAREN
  { $$=^$(make_exp(^raw_exp.Raise($3),                 SURROUND(@1,@4))); }
| CODEGEN LPAREN prim_type pid convention LPAREN named_param_list RPAREN type_mods block RPAREN
  { $$=^$(make_exp
	  (^raw_exp.Codegen(^fndecl(false,$5,$4,null,make_type($3,$9),$7,$10)), 
	  SURROUND(@1,@11))); }
| CODEGEN LPAREN prim_type pid convention LPAREN RPAREN type_mods block RPAREN
  { $$=^$(make_exp
	  (^raw_exp.Codegen(^fndecl(false,$5,$4,null,make_type($3,$8),null,$9)),
	  SURROUND(@1,@10))); }
| FILL LPAREN exp RPAREN
  { $$=^$(make_exp(^raw_exp.Fill($3),                  SURROUND(@1,@4))); }
| FUN  prim_type prim_pat type_mods block
  { string name;
    switch $3 {
    case Var(p): name = p.1;
    case Wild(_): name = "_";
    }
    Gcdfec::seg loc = SURROUND(@1,@5);
    fndecl fd = make_fndecl(loc,true,$2,name,$4,$5);
    $$=^$(make_exp(^raw_exp.Fun(fd),loc));
  }
| SPRINTF LPAREN const_string RPAREN
  { $$=^$(make_exp(^raw_exp.Const(^cnst.String($3)), SURROUND(@1,@4))); }
| SPRINTF LPAREN const_string COMMA exps RPAREN
  { $$=^$(expand_sprintf(SURROUND(@1,@6),$3,$5)); }
| exp1 LPAREN RPAREN
  { $$=^$(make_exp(^raw_exp.FunCall(^($1,null,null)),     SURROUND(@1,@3))); }
| exp1 LPAREN exps RPAREN
  { $$=^$(make_exp(^raw_exp.FunCall(^($1,null,$3)),       SURROUND(@1,@4))); }
| exp1 AT LESSTHAN param_list GREATERTHAN
  { $$=^$(make_exp(^raw_exp.TypInst(^($1,List::map(snd_stropt_typ,$4))), 
                   SURROUND(@1,@5))); }
| exp1 AT LESSLESS param_list GREATERTHAN pid_or_array GREATERTHAN GREATERTHAN
  { $$=^$(make_exp(^raw_exp.TypInst(^($1,^list(name_or_array($6,List::map(snd_stropt_typ,$4),SURROUND(@3,@5)),null))),SURROUND(@1,@9)));
  }							     
| exp1 AT LESSLESS param_list GREATERTHAN pid_or_array COMMA param_list GREATERTHAN
  { $$=^$(make_exp(^raw_exp.TypInst(^($1,^list(name_or_array($6,List::map(snd_stropt_typ,$4),SURROUND(@3,@5)),List::map(snd_stropt_typ,$8)))),SURROUND(@1,@9)));
  }							     
| exp1 LBRACKET exp RBRACKET
  { $$=^$(make_exp(^raw_exp.Subscript(^($1,$3)),           SURROUND(@1,@4))); }
| exp1 LCBRACKET exp RCBRACKET
  { $$ = ^$(make_exp(^raw_exp.CSubscript(^($1,$3)),        SURROUND(@1,@4))); }
| exp1 DOT ID
  { $$=^$(make_exp(^raw_exp.StructMember(^($1,$3)),        SURROUND(@1,@3))); }
| exp1 TUPLEOFFSET
  { $$=^$(make_exp(^raw_exp.TupleMember(^($1,$2)),         SURROUND(@1,@2))); }
;
new_kw:
  NEW   {;} // no action
| CARET {;} // no action
;
exp2:
  exp1 { $$=$!1; }
| new_kw pid LPAREN RPAREN
   { $$=^$(make_exp(^raw_exp.NewStruct(^($2,null,null)), SURROUND(@1,@4))); }
| new_kw pid LPAREN exps RPAREN
   {$$=^$(make_exp(^raw_exp.NewStruct(^($2,null,map(make_labelled_exp,$4))),
                   SURROUND(@1,@5))); 
   }
| new_kw pid LBRACE RBRACE
   { $$=^$(make_exp(^raw_exp.NewStruct(^($2,null,null)), SURROUND(@1,@4))); }
| new_kw pid LBRACE labelled_exps RBRACE
   { $$=^$(make_exp(^raw_exp.NewStruct(^($2,null,$4)),SURROUND(@1,@5))); }
| new_kw pid DOT ID LPAREN exp RPAREN
   { $$=^$(make_exp(^raw_exp.NewUnion(^($2,null,$4,^Opt($6))), 
                    SURROUND(@1,@7))); }
| new_kw pid DOT ID 
   { $$=^$(make_exp(^raw_exp.NewUnion(^($2,null,$4,null)),
                    SURROUND(@1,@4))); }

| new_kw DOT ID LPAREN exp RPAREN
   { $$=^$(make_exp(^raw_exp.NewUnion(^("",null,$3,^Opt($5))), 
                    SURROUND(@1,@6))); }
| new_kw DOT ID 
   { $$=^$(make_exp(^raw_exp.NewUnion(^("",null,$3,null)),
                    SURROUND(@1,@3))); }

| new_kw LPAREN RPAREN
   { $$=^$(make_exp(^raw_exp.NewTuple(null), SURROUND(@1,@3))); }
| new_kw LPAREN exps RPAREN
   { $$=^$(make_exp(^raw_exp.NewTuple($3), SURROUND(@1,@4))); }
;
exp3:
   exp2 { $$=$!1; }
| exp3 PLUSPLUS
{ $$=^$(make_exp
	(^raw_exp.Primop
	(^(^primop.Minus, 
	   ^list(make_exp
	         (^raw_exp.AssignOp(^($1, ^Opt(^primop.Plus), 
			   	      make_exp(^raw_exp.Const(^cnst.Int(1)), 
				                SURROUND(@1,@2)))), 
	          SURROUND(@1,@2)), 
           ^list(make_exp(^raw_exp.Const(^cnst.Int(1)), SURROUND(@1,@2)), 
		 null)))), 
	SURROUND(@1,@2))); }
| exp3 MINUSMINUS
{ $$=^$(make_exp
	(^raw_exp.Primop
	(^(^primop.Plus, 
	   ^list(make_exp
	         (^raw_exp.AssignOp(^($1, ^Opt(^primop.Minus), 
			   	      make_exp(^raw_exp.Const(^cnst.Int(1)), 
				                SURROUND(@1,@2)))), 
	          SURROUND(@1,@2)), 
           ^list(make_exp(^raw_exp.Const(^cnst.Int(1)), SURROUND(@1,@2)), 
		 null)))), 
	SURROUND(@1,@2))); }
| MINUSMINUS exp3
{ $$=^$(make_exp(^raw_exp.AssignOp(^($2, ^Opt(^primop.Minus), 
			   	      make_exp(^raw_exp.Const(^cnst.Int(1)), 
				                SURROUND(@1,@2)))),
        SURROUND(@1,@2))); }
| PLUSPLUS exp3
{ $$=^$(make_exp(^raw_exp.AssignOp(^($2, ^Opt(^primop.Plus), 
			   	      make_exp(^raw_exp.Const(^cnst.Int(1)), 
				                SURROUND(@1,@2)))),
        SURROUND(@1,@2))); }
| PLUS exp3 { $$=$!2; }
| MINUS exp3
{  bool is_constant = false;
   switch $2.raw_exp {
   case Const(x): 
     switch x {
     case Int(i): 
       is_constant = true;
       $$=^$(make_exp(^raw_exp.Const(^cnst.Int(0-i)), SURROUND(@1,@2)));
     default: ;
     }
   default: ;
   }
   if (!is_constant)
     $$=^$(make_exp(^raw_exp.Primop
                     (^(^primop.Minus, 
		        ^list(make_exp(^raw_exp.Const(^cnst.Int(0)),
				                   SURROUND(@1,@2)),
				          ^list($2,null)))),
                    SURROUND(@1,@2)));}
| TILDE exp3 
   { $$=^$(make_exp(^raw_exp.Primop(^(^primop.Bitnot,^list($2,null))), 
                    SURROUND(@1,@2))); }
| BANG exp3 
   { $$=^$(make_exp(^raw_exp.Primop(^(^primop.Not,^list($2,null))), 
                    SURROUND(@1,@2))); }
;
exp35:
  exp3 { $$=$!1; }
| LPAREN COLON prim_type type_mods RPAREN exp35
  { $$=^$(make_exp(^raw_exp.Cast(^(make_type($3,$4), $6)), SURROUND(@1,@6))); }
;
exp4:
  exp35 { $$=$!1; }
| exp4 exp4op exp35 { $$=^$(make_binop($1,$2,$3,SURROUND(@1,@3))); }
;
exp5:
  exp4 { $$=$!1; }
| exp5 exp5op exp4 { $$=^$(make_binop($1,$2,$3,SURROUND(@1,@3))); }
;
exp6:
  exp5 { $$=$!1; }
| exp6 exp6op exp5 { $$=^$(make_binop($1,$2,$3,SURROUND(@1,@3))); }
;
exp7:
  exp6 { $$=$!1; }
| exp7 exp7op exp6 { $$=^$(make_binop($1,$2,$3,SURROUND(@1,@3))); }
;
exp8:
  exp7 { $$=$!1; }
| exp8 exp8op exp7 { $$=^$(make_binop($1,$2,$3,SURROUND(@1,@3))); }
;
exp9:
  exp8 { $$=$!1; }
| exp9 exp9op exp8 { $$=^$(make_binop($1,$2,$3,SURROUND(@1,@3))); }
;
exp10:
  exp9 { $$=$!1; }
| exp10 exp10op exp9 { $$=^$(make_binop($1,$2,$3,SURROUND(@1,@3))); }
;
exp11:
  exp10 { $$=$!1; }
| exp11 exp11op exp10  { $$=^$(make_binop($1,$2,$3,SURROUND(@1,@3))); }
;
exp4op:
  TIMES                    {$$=^$(^primop.Times);}
| DIV                      {$$=^$(^primop.Div);}
| PERCENT                  {$$=^$(^primop.Mod);}
;
exp5op:
  PLUS                     {$$=^$(^primop.Plus);}
| MINUS                    {$$=^$(^primop.Minus);}
;
exp6op:
  GREATERGREATER         {$$=^$(^primop.Bitarshift);}
| LESSLESS               {$$=^$(^primop.Bitlshift);}
| GREATERGREATERGREATER  {$$=^$(^primop.Bitlrshift);}
;
exp7op:
  LESSTHAN               {$$=^$(^primop.Lt);}
| GREATERTHAN            {$$=^$(^primop.Gt);}
| LESSTHANEQ             {$$=^$(^primop.Lte);}
| GREATERTHANEQ          {$$=^$(^primop.Gte);}
;
exp8op:
  EE                     {$$=^$(^primop.Eq);}
| NE                     {$$=^$(^primop.Neq);}
;
exp9op:
  AMPER                  {$$=^$(^primop.Bitand);}
;
exp10op:
  CARET                  {$$=^$(^primop.Bitxor);}
;
exp11op:
  PIPE                   {$$=^$(^primop.Bitor);}
;
exp12:
  exp11 { $$=$!1; }
| exp12 AMPERAMPER exp11 
   {$$=^$(make_exp
	  (^raw_exp.Conditional(^($1,$3,
				  make_exp(^raw_exp.Const(^cnst.Bool(false)), 
				           DUMMYLOC))), 
	   SURROUND(@1,@3))); }
;
exp13:
  exp12 { $$=$!1; }
| exp13 PIPEPIPE exp12 
   {$$=^$(make_exp
	  (^raw_exp.Conditional(^($1,
				  make_exp(^raw_exp.Const(^cnst.Bool(true)), 
				           DUMMYLOC),
				  $3)),
	   SURROUND(@1,@3))); }
;
exp14:
  exp13 { $$=$!1; }
| exp13 QUESTION exp COLON exp14
    { $$=^$(make_exp(^raw_exp.Conditional(^($1,$3,$5)), SURROUND(@1,@5))); }
;
exp15:
  exp14  { $$=$!1; }
| exp14 exp15op exp15
  {$$=^$(make_exp(^raw_exp.AssignOp(^($1,^Opt($2),$3)), SURROUND(@1,@3))); }
| exp14 EQUALS exp15
  {$$=^$(make_exp(^raw_exp.AssignOp(^($1,null,$3)),     SURROUND(@1,@3))); }
;
exp15op:
 PLUSEQUAL                {$$=^$(^primop.Plus);}
| MINUSEQUAL              {$$=^$(^primop.Minus);}
| TIMESEQUAL              {$$=^$(^primop.Times);}
| DIVEQUAL                {$$=^$(^primop.Div);}
| MODEQUAL                {$$=^$(^primop.Mod);}
| AMPEREQUAL              {$$=^$(^primop.Bitand);}
| PIPEEQUAL               {$$=^$(^primop.Bitor);}
| CARETEQUAL              {$$=^$(^primop.Bitxor);}
| LESSLESSEQ              {$$=^$(^primop.Bitlshift);}
| GREATERGREATEREQ        {$$=^$(^primop.Bitarshift);}
| GREATERGREATERGREATEREQ {$$=^$(^primop.Bitlrshift);}
;
comma_free_exp:
 exp15 { $$=$!1;}
;
exps:
  comma_free_exp            { $$=^$(^list($1,null)); }
| comma_free_exp COMMA exps { $$=^$(^list($1,$3));   }
;
labelled_exps:
  ID EQUALS comma_free_exp  { $$=^$(^list(^(^Opt($1),$3),null)); }
| ID EQUALS comma_free_exp COMMA labelled_exps 
  { $$=^$(^list(^(^Opt($1),$3),$5)); }
;
exp:
  comma_free_exp { $$=$!1; }
| comma_free_exp COMMA exps 
    { $$=^$(make_exp(^raw_exp.SeqExp(^list($1,$3)), SURROUND(@1,@3))); }
;
/////////////////////////// Patterns /////////////////////////////
prim_pat:
  ID         { $$=^$(^prim_pattern.Var(^($1,^typ.VoidType))); }
| UNDERSCORE { $$=^$(^prim_pattern.Wild(^(^typ.VoidType))); }
;
prim_pat_list:
  prim_pat                     { $$=^$(^list($1,null)); }
| prim_pat COMMA prim_pat_list { $$=^$(^list($1,$3)); }
;

////////////////////////////// Misc //////////////////////////
convention:
          { $$ = ^$(default_convention); }
| STDCALL { $$ = ^$(^.Stdcall); }
| CDECL   { $$ = ^$(^.Cdecl); }
;
id_opt:
     { $$=^$(null); } // empty case
| ID { $$=^$(^Opt($1)); }
;
pid: 
 ID                { $$=$!1; } 
| ID COLONCOLON pid { $$=^$(add_prefix($1,$3)); }
;
pid_or_array:
  pid              { $$=^$(^Opt($1)); }
| ARRAY            { $$=^$(null); }
;

const_string:
  CONSTSTRING      { $$ = $!1; }
| CONSTSTRING const_string { $$ = ^$(strconcat($1,$2)); }
; 

%%

prefix Popparse;

static void err(Poperr::parseError e, seg sg) {
  Gcdfec::post_error 
    (Gcdfec::mk_err_parse(sg, 
			  Poperr::error_message(^Poperr::errorb.Eparse(e))));
}
static a abort<a>(string str, seg sg) {
  Gcdfec::post_error (Gcdfec::mk_err_parse(sg,str));
  raise Gcdfec::Exit();
}

static exp make_exp(raw_exp re, seg sg) { 
  return ^exp(null, re, null, sg);
}
static exp make_binop(exp lft, primop oper, exp rgt, seg sg) {
  return make_exp(^raw_exp.Primop(^(oper,^list(lft,^list(rgt,null)))), sg);
}
static typ name_or_array(<string>Opt n,<typ>list args,seg sg) {
  if (n != null)
    return ^typ.NamedType(^(n.v,args));
  else if (args != null && args.tl == null) 
    return ^typ.ArrayType(^(args.hd,null));
  else return (abort("array type constructor takes one argument",sg));
}

// can't be static b/c YYSTYPE is exported
union type_modifier { 
  <exp>Opt Array; 
  *(int,bool)      CArray;
  *(convention, <string>list, <*(<string>Opt,typ)>list) Params;
}
union exp_or_fun {  // used in initialization expressions
  void None;
  exp  Exp;
  stmt Block;
}
union switch_clause {
  void                              AnyClause;
  <*(int, stmt)>list                IntClauses;
  <*(char,stmt)>list                CharClauses;
  <*(field_name,pattern,stmt)>list  UnionClauses;
}

static top_decl make_global_decl(*(scope,seg) x, *(var,typ,<exp>Opt) d) {
  return ^top_decl(^raw_top_decl.GlobalDecl(^(x.1, d.1, d.2, d.3)), 
                   x.2);
}
static top_decl make_extern_val(seg sg, *(var,typ,<exp>Opt) d) {
  if (d.3 != null)
    err(^Poperr::parseError.ExternNoInit, sg);
  return 
    ^top_decl(^raw_top_decl.ExternVal(^(d.1, d.2)), sg);
}
static *(string,typ) make_id_decl(typ t, *(string,<type_modifier>list) d) {
  return ^(d.1, make_type(t,d.2));
}
static *(string,capability,typ) make_struct_field_decl(capability c, 
                                                       *(string,typ) d) {
  return ^(d.1,c,d.2);
}
static stmt make_var_stmt (<*(var,typ,<exp>Opt)>list vs, stmt s, seg sg) {
  return List::fold_right_c(make_var_stmt_f, sg, vs, s);
}
static stmt make_var_stmt_f(seg sg, *(var,typ,<exp>Opt) v, stmt s) {
  return ^stmt(^raw_stmt.Decl(^(v.1,v.2,v.3,s)), sg);
}
static *(var,typ,<exp>Opt) 
  make_var_decl(typ t, *(var,<type_modifier>list,<exp>Opt) d) {
  return ^(d.1, make_type(t,d.2), d.3);
}
static *(var,typ,<exp>Opt) 
  make_local_var_decl(typ t, *(var,<type_modifier>list,exp_or_fun) d) {
  switch d.3 {
  case None: return ^(d.1,make_type(t,d.2),null);
  case Exp(e): return ^(d.1,make_type(t,d.2),^Opt(e));
  case Block(s): 
    fndecl fd = make_fndecl(s.loc,true,t,d.1,d.2,s);
    exp e = make_exp(^raw_exp.Fun(fd),s.loc);
    return ^(d.1,make_type(t,d.2),^Opt(e));
  }
}
static *(string,typ) deopt_param(seg sg, *(<string>Opt,typ) param) {
  if (param.1 == null)
    err(^Poperr::parseError.ParamNotNamed, sg);
  return ^(param.1.v, param.2);
}
static typ make_type(typ t, <type_modifier>list ms) {
  while (ms != null) {
    switch ms.hd {
    case Array(eopt):     
      t = ^typ.ArrayType(^(t,eopt));
    case CArray *(sz,opt):
      t = ^typ.CArrayType(^(t,sz,opt));
    case Params *(c,tvs,args): 
      t = ^typ.FnType(^(c,tvs, t, List::map(snd_stropt_typ,args)));
    }
    ms = ms.tl;
  }
  return t;
}
static *(switch_clause,<stmt>Opt) add_int_clause(int i, stmt s, 
				    *(switch_clause,<stmt>Opt) x, seg sg) {
  switch x.1 {
  case IntClauses(cs): 
    return ^(^switch_clause.IntClauses(^list(^(i,s),cs)), x.2);
  case AnyClause:
    return ^(^switch_clause.IntClauses(^list(^(i,s),null)), x.2);
  default: 
    err(^Poperr::parseError.SwitchClausesDontMatch, sg);
    return x;
  }
}
*(<field_name>Opt,exp) make_labelled_exp(exp e) {
  return ^(null,e);
}
static *(switch_clause,<stmt>Opt) add_char_clause(char c, stmt s, 
				     *(switch_clause,<stmt>Opt) x, seg sg) {
  switch x.1 {
  case CharClauses(cs): 
    return ^(^switch_clause.CharClauses(^list(^(c,s),cs)), x.2);
  case AnyClause:
    return ^(^switch_clause.CharClauses(^list(^(c,s),null)), x.2);
  default: 
    err(^Poperr::parseError.SwitchClausesDontMatch, sg);
    return x;
  }
}
static *(switch_clause,<stmt>Opt) add_union_clause
                   (field_name f, pattern p, stmt s, 
		   *(switch_clause,<stmt>Opt) x, seg sg) {
  switch x.1 {
  case UnionClauses(cs): 
    return ^(^switch_clause.UnionClauses(^list(^(f,p,s),cs)), x.2);
  case AnyClause:
    return ^(^switch_clause.UnionClauses(^list(^(f,p,s),null)), x.2);
  default: 
    err(^Poperr::parseError.SwitchClausesDontMatch, sg);
    return x;
  }
}
static switch_arm convert_union_clause(*(field_name,pattern p,stmt) x) {
  return ^switch_arm(x.1,x.2,x.3);
}

fndecl make_fndecl(Gcdfec::seg loc, bool is_static, typ ret_typ, 
		   string pid, <type_modifier>list ms, stmt block) {
  { 
    type_modifier       arg_mod;
    <string>list        tyvars;
    <*(string,typ)>list args;
    convention          convention = default_convention;
    if (ms == null)
      return abort("Illformed internal function",loc);
    if (ms.tl == null) {
      arg_mod = ms.hd;
      ms = null;
    } else { // dangerous imperative update...
      <type_modifier>list l = ms;
      while (l.tl.tl != null) l = l.tl;
      arg_mod = l.tl.hd;
      l.tl = null;
    }
    ret_typ = make_type(ret_typ,ms);
    switch arg_mod {
    case Array(_): return abort("Illformed inner function",loc);
    case CArray(_): return abort("Illformed inner function",loc);
    case Params *(c,tvs,params):
      args = List::map_c(deopt_param, loc, params);
      tyvars = tvs;
      convention = c;
    }
    return ^fndecl(is_static,convention,pid,tyvars,ret_typ,args,block);
  }
}


// can't use explicit instantiation in actions, so here we go:
static typ snd_str_typ   (*(string,typ) x) { return x.2; }
static typ snd_stropt_typ(*(<string>Opt,typ) x) { return x.2; }

// set by prog action
static <top_decl>list parse_result = null; 
static bool success = false;

////////////////////////// Expansion of Printf ////////////////////////
static union printf_desc 
{ string Literal;
  void   Int;
  void   Unsigned;
  void   Hex;
  void   Char;
  void   String;
  void   Float; // %f
  void   Double; // %g
}

static stmt make_seq(Gcdfec::seg loc,stmt s1,stmt s2) {
  return(^stmt(^raw_stmt.Seq(^(s1,s2)),loc));
}
static stmt skip_stmt(Gcdfec::seg loc) {
  return(^stmt(^raw_stmt.Skip,loc));
}
exp make_call_exp(Gcdfec::seg loc,string f,<exp>list args) {
  return make_exp(^raw_exp.FunCall(^(make_exp(^raw_exp.Var(f),loc),
				     null,args)),loc);
}
static stmt make_call_stmt(Gcdfec::seg loc,string f,<exp>list args) {
  return(^stmt(^raw_stmt.Exp(make_call_exp(loc,f,args)),loc));
}
static stmt print_int_stmt(Gcdfec::seg loc,exp fexp,exp iexp) {
  return(make_call_stmt(loc,"fprint_int",^list(fexp,^list(iexp,null))));
}
static stmt print_unsigned_stmt(Gcdfec::seg loc,exp fexp,exp iexp) {
  return(make_call_stmt(loc,"fprint_uint",^list(fexp,^list(iexp,null))));
}
static stmt print_hex_stmt(Gcdfec::seg loc,exp fexp,exp iexp) {
  return(make_call_stmt(loc,"fprint_hex",^list(fexp,^list(iexp,null))));
}
static stmt print_char_stmt(Gcdfec::seg loc,exp fexp,exp cexp) {
  return(make_call_stmt(loc,"fprint_char",^list(fexp,^list(cexp,null))));
}
static stmt print_string_stmt(Gcdfec::seg loc,exp fexp,exp sexp) {
  return(make_call_stmt(loc,"fprint_string",
			^list(fexp,^list(sexp,null))));
}
static stmt print_float_stmt(Gcdfec::seg loc,exp fexp,exp sexp) {
  return(make_call_stmt(loc,"fprint_float",
			^list(fexp,^list(sexp,null))));
}
static stmt print_double_stmt(Gcdfec::seg loc,exp fexp,exp sexp) {
  return(make_call_stmt(loc,"fprint_double",
			^list(fexp,^list(sexp,null))));
}

// break a string s into a list of printf descriptors
static <printf_desc>list printf_descriptors(Gcdfec::seg loc,string s) {
  <char>list chars = String::explode(s);
  <char>list string_chars = null;
  <printf_desc>list result = null;
  while (true) {
    if (chars == null) 
      if (string_chars == null) 
	break;
      else {
	string lit = String::implode(List::rev(string_chars));
	result = ^list(^printf_desc.Literal(lit),result);
	break;
      }
    else if ((chars.hd == '%') && 
	     (chars.tl != null) && 
	     (chars.tl.hd == '%')) {
      string_chars = ^list('%',string_chars);
      chars = chars.tl.tl;
      continue;
    } else if (chars.hd == '%' && chars.tl != null) {
      chars = chars.tl;
      char c = chars.hd;
      chars = chars.tl;
      if (string_chars != null) {
	string lit = String::implode(List::rev(string_chars));
	string_chars = null;
	result = ^list(^printf_desc.Literal(lit),result);
      }
      switch c {
      case 'd': result = ^list(^printf_desc.Int,result); 
      case 'u': result = ^list(^printf_desc.Unsigned,result);
      case 'x': result = ^list(^printf_desc.Hex,result);
      case 's': result = ^list(^printf_desc.String,result); 
      case 'c': result = ^list(^printf_desc.Char,result); 
      case 'f': result = ^list(^printf_desc.Float,result);
      case 'g': result = ^list(^printf_desc.Double,result);
      default: 
	string s1 = string_of_char(c);
	string s2 = strconcat("Bad format char ",s1);
	abort(s2,loc);
      }
      continue;
    } else {
      string_chars = ^list(chars.hd,string_chars);
      chars = chars.tl;
      continue;
    }
  }
  return(List::rev(result));
}

// Given an expression fdexp representing a file descriptor, a string
// that's a printf descriptor, and a list of expressions, generate
// a sequence of calls to print_int, print_string, print_char, etc.
// For example:  printf("foo %s bar %d baz %c\n",a,b,c) turns into
// fprint_string(tal_stdout,"foo "); fprint_string(tal_stdout,a);
// fprint_string(tal_stdout," bar "); fprint_int(tal_stdout,b);
// fprint_string(tal_stdout," baz "); fprint_char(tal_stdout,c);
// fprint_string(tal_stdout,"\n");
static stmt 
expand_printf(Gcdfec::seg loc,exp fdexp,string desc_string,<exp>list exps) {
  <printf_desc>list descs = printf_descriptors(loc,desc_string);
  <stmt>list stmts = null;
  while (true) {
    if ((descs == null) && (exps == null))
      break;
    else if ((descs == null) && (exps != null))
      abort("too many arguments to printf",loc);
    else if (descs != null) {
      printf_desc pd = descs.hd;
      descs = descs.tl;
      switch pd {
      case Literal(s):
	exp sexp = make_exp(^raw_exp.Const(^cnst.String(s)),loc);
	stmts = ^list(print_string_stmt(loc,fdexp,sexp),stmts);
	continue;
      case Int:
	if (exps == null)
	  abort("too few arguments to printf",loc);
	stmts = ^list(print_int_stmt(loc,fdexp,exps.hd),stmts);
	exps = exps.tl;
	continue;
      case Unsigned:
	if (exps == null)
	  abort("too few arguments to printf",loc);
	stmts = ^list(print_unsigned_stmt(loc,fdexp,exps.hd),stmts);
	exps = exps.tl;
      case Hex:
	if (exps == null)
	  abort("too few arguments to printf",loc);
	stmts = ^list(print_hex_stmt(loc,fdexp,exps.hd),stmts);
	exps = exps.tl;
      case Char:
	if (exps == null)
	  abort("too few arguments to printf",loc);
	stmts = ^list(print_char_stmt(loc,fdexp,exps.hd),stmts);
	exps = exps.tl;
	continue;
      case String:
	if (exps == null)
	  abort("too few arguments to printf",loc);
	stmts = ^list(print_string_stmt(loc,fdexp,exps.hd),stmts);
	exps = exps.tl;
	continue;
      case Float:
	if(exps == null) 
	  abort("too few arguments to printf",loc);
	stmts = ^list(print_float_stmt(loc,fdexp,exps.hd),stmts);
	exps = exps.tl;
	continue;
      case Double:
	if(exps == null) 
	  abort("too few arguments to printf",loc);
	stmts = ^list(print_double_stmt(loc,fdexp,exps.hd),stmts);
	exps = exps.tl;
	continue;
      }
    }
  }
  stmts = List::rev(stmts);
  return(List::fold_right_c(make_seq,loc,stmts,skip_stmt(loc)));
}
static exp cons_exp(Gcdfec::seg loc,exp hd, exp tl) {
  return make_call_exp (loc,"List?cons",^list(hd,^list(tl,null)));
}
// similar, but for sprintf.  Here, we just append all of the strings
// via String::strconcat_l. 
static exp expand_sprintf(Gcdfec::seg loc,string desc_string,<exp>list exps) {
  <printf_desc>list descs = printf_descriptors(loc,desc_string);
  exp arg = make_exp(^raw_exp.Const(^cnst.Null),loc);
  // we build up the expression in reverse order
  descs = List::rev(descs);
  exps = List::rev(exps);
  while (true) {
    if ((descs == null) && (exps == null)) 
      break;
    else if ((descs == null) && (exps != null))
      abort("too many arguments to sprintf",loc);
    else if (descs != null) {
      printf_desc pd = descs.hd;
      descs = descs.tl;
      switch pd {
      case Literal(s):
	arg = cons_exp(loc,make_exp(^raw_exp.Const(^cnst.String(s)),loc),arg);
	continue;
      case Int:
	if (exps == null) abort("too few arguments to sprintf",loc);
	arg = cons_exp(loc,make_call_exp(loc,"Core?string_of_int",
					 ^list(exps.hd,null)),arg);
      case Unsigned:
	if (exps == null) abort("too few arguments to sprintf",loc);
	arg = cons_exp(loc,make_call_exp(loc,"Core?string_of_uint",
					 ^list(exps.hd,null)),arg);
      case Hex:
	if (exps == null) abort("too few arguments to sprintf",loc);
	arg = cons_exp(loc,make_call_exp(loc,"Core?hex_string_of_uint",
					 ^list(exps.hd,null)),arg);
      case Char:
	if (exps == null) abort("too few arguments to sprintf",loc);
	arg = cons_exp(loc,make_call_exp(loc,"Core?string_of_char",
					 ^list(exps.hd,null)),arg);
      case String:
	if (exps == null) abort("too few arguments to sprintf",loc);
	arg = cons_exp(loc,exps.hd,arg);
      case Float:
	if (exps == null) abort("too few arguments to sprintf",loc);
	arg = cons_exp(loc,make_call_exp(loc,"Core?string_of_float",
					 ^list(exps.hd,null)),arg);
      case Double:
	if (exps == null) abort("too few arguments to sprintf",loc);
	arg = cons_exp(loc,make_call_exp(loc,"Core?string_of_double",
					 ^list(exps.hd,null)),arg);
      }
      exps = exps.tl;
    } // end if
  } // end while
  return make_call_exp(loc,"String?strconcat_l",^list(arg,null));
}
////////////////////////// Entry Points ////////////////////////
< < <FILE>function_lexbuf_state>lexbuf>Opt lbuf = null;
exception ParseError;

<top_decl>list parse_program(FILE f) { 
  yylloc = ^yyltype(0,0,0,0,0,""); // this should be part of pop_bison.simple
  lbuf = ^Opt(from_file(f));
  parse_result = null;
  success      = false;
  yyparse();
  if (!success) raise ParseError();
  return parse_result;
}
