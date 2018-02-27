/* parse.y:  Copyright (c) 2000, Greg Morrisett, all rights reserved.
 *
 * An adaptation of the OSI C grammar for Cyclone 2.0.  This grammar
 * is adapted from the proposed C9X standard, but the productions are
 * arranged as in Kernighan and Ritchie's "The C Programming
 * Language (ANSI C)", Second Edition, pages 234-239.
 *
 * The grammar has 1 shift-reduce conflict, due to the "dangling else"
 * problem.  It is properly resolved by Bison.  */

%{
#include "core.h"
#include "lexing.h"
#include "list.h"
#include "string.h"
#include "set.h"
#include "gcdfec.h"
#include "absyn.h"
#include "synpp.h"
#include "pp.h"
#include "fn.h"
open Core;
open Lexing;
open List;
open Absyn;
open Gcdfec;
open Parse;
open Fn;

/* Typedef processing must be split between the parser and lexer.
   These functions are called by the parser to communicate typedefs
   to the lexer, so that the lexer can distinguish typedef names from
   identifiers. */
extern void Lex::register_typedef(qvar s);
extern void Lex::enter_namespace(string);
extern void Lex::leave_namespace();
extern void Lex::enter_using(qvar);
extern void Lex::leave_using();

#define LOC(s,e) Gcdfec::seg_of_abs(s.first_line,e.last_line)
%}

/* ANSI C keywords */
%token AUTO REGISTER STATIC EXTERN TYPEDEF VOID CHAR SHORT INT LONG FLOAT
%token DOUBLE SIGNED UNSIGNED CONST VOLATILE RESTRICT
%token STRUCT UNION CASE DEFAULT INLINE
%token IF ELSE SWITCH WHILE DO FOR GOTO CONTINUE BREAK RETURN SIZEOF ENUM
/* Cyc:  CYCLONE additional keywords */
%token BOOL BOXED_BOOL BOXED_CHAR BOXED_SHORT BOXED_INT BOXED_LONG
%token BOXED_FLOAT BOXED_DOUBLE NULL LET THROW TRY CATCH
%token WHERE NEW ABSTRACT FALLTHRU USING NAMESPACE XENUM
%token TRUE FALSE
%token FILL CODEGEN CUT SPLICE
%token PRINTF FPRINTF XPRINTF SIZE
/* double and triple-character tokens */
%token PTR_OP INC_OP DEC_OP LEFT_OP RIGHT_OP LE_OP GE_OP EQ_OP NE_OP
%token AND_OP OR_OP MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN
%token SUB_ASSIGN LEFT_ASSIGN RIGHT_ASSIGN AND_ASSIGN
%token XOR_ASSIGN OR_ASSIGN ELLIPSIS LEFT_RIGHT
/* identifiers and constants */
%token IDENTIFIER INTEGER_CONSTANT STRING
%token CHARACTER_CONSTANT FLOATING_CONSTANT
/* Cyc: type variables, qualified identifers and typedef names */
%token TYPE_VAR QUAL_IDENTIFIER QUAL_TYPEDEF_NAME
/* the union type for all productions -- for now a placeholder */
%union {
  void                                Okay;
  *(sign,int)                         Int;
  char                                Char;
  short                               Short;
  bool                                Bool;
  string                              String;
  <string>Opt                         StringOpt;
  string                              Float; /* Left in string form for now */
  typ                                 Type;
  <typ>list                           TypeList;
  exp                                 Exp;
  <exp>list                           ExpList;
  primop                              Primop;
  <primop>Opt                         PrimopOpt;
  qvar                                QualId;
  stmt                                Stmt;
  <switch_clause>list                 SwitchClauseList;
  pat                                 Pattern;
  <pat>list                           PatternList;
  fndecl                              FnDecl;
  <decl>list                          DeclList;
  decl_spec                           DeclSpec;
  *(declarator,<exp>Opt)              InitDecl;
  <*(declarator,<exp>Opt)>list        InitDeclList;
  storage_class                       StorageClass;
  type_specifier                      TypeSpecifier;
  *(tqual,<type_specifier>list)       Qual_SpecList;
  tqual                               TypeQual;
  <*(string,tqual,typ)>list           StructFieldDeclList;
  <<*(string,tqual,typ)>list>list     StructFieldDeclListList;
  declarator                          Declarator;
  <declarator>list                    DeclaratorList;
  abstractdeclarator                  AbstractDeclarator;
  enumfield                           EnumeratorField;
  <enumfield>list                     EnumeratorFieldList;
  *(<var>Opt,tqual,typ)               ParamDecl;
  <*(<var>Opt,tqual,typ)>list         ParamDeclList;
  *(<*(<var>Opt,tqual,typ)>list,bool) ParamDeclListBool;
  struct_or_union                     StructOrUnion;
  <var>list                           IdList;
  designator                          Designator;
  <designator>list                    DesignatorList;
  <type_modifier>list                 TypeModifierList;
  <*(<designator>list,exp)>list       InitializerList;
  *(<designator>list,pat)             FieldPattern;
  <*(<designator>list,pat)>list       FieldPatternList;
  blockitem                           BlockItem;
}
/* types for productions */
%type <Int> INTEGER_CONSTANT
%type <String> FLOATING_CONSTANT
%type <Char> CHARACTER_CONSTANT
%type <String> IDENTIFIER TYPE_VAR STRING
%type <String> namespace_action
%type <Exp> primary_expression postfix_expression unary_expression
%type <Exp> cast_expression constant multiplicative_expression
%type <Exp> additive_expression shift_expression relational_expression
%type <Exp> equality_expression and_expression exclusive_or_expression
%type <Exp> inclusive_or_expression logical_and_expression
%type <Exp> logical_or_expression conditional_expression
%type <Exp> assignment_expression expression constant_expression
%type <Exp> initializer
%type <ExpList> argument_expression_list argument_expression_list0
%type <InitializerList> initializer_list
%type <Primop> unary_operator format_primop
%type <PrimopOpt> assignment_operator
%type <QualId> QUAL_IDENTIFIER QUAL_TYPEDEF_NAME qual_opt_identifier
%type <QualId> using_action
%type <Stmt> statement labeled_statement
%type <Stmt> compound_statement block_item_list
%type <BlockItem> block_item
%type <Stmt> expression_statement selection_statement iteration_statement
%type <Stmt> jump_statement
%type <SwitchClauseList> switch_clauses
%type <Pattern> pattern
%type <PatternList> tuple_pattern_list tuple_pattern_list0
%type <FieldPattern> field_pattern
%type <FieldPatternList> field_pattern_list field_pattern_list0
%type <FnDecl> function_definition function_definition2
%type <DeclList> declaration declaration_list
%type <DeclList> prog
%type <DeclList> translation_unit translation_unit_opt external_declaration
%type <DeclSpec> declaration_specifiers
%type <InitDecl> init_declarator
%type <InitDeclList> init_declarator_list init_declarator_list0
%type <StorageClass> storage_class_specifier
%type <TypeSpecifier> type_specifier
%type <TypeSpecifier> struct_or_union_specifier enum_specifier
%type <StructOrUnion> struct_or_union
%type <TypeQual> type_qualifier type_qualifier_list
%type <StructFieldDeclList> struct_declaration_list struct_declaration
%type <StructFieldDeclListList> struct_declaration_list0
%type <TypeModifierList> pointer
%type <Declarator> declarator direct_declarator struct_declarator
%type <DeclaratorList> struct_declarator_list struct_declarator_list0
%type <AbstractDeclarator> abstract_declarator direct_abstract_declarator
%type <EnumeratorField> enumerator
%type <EnumeratorFieldList> enumerator_list
%type <Qual_SpecList> specifier_qualifier_list
%type <IdList> identifier_list identifier_list0
%type <ParamDecl> parameter_declaration type_name
%type <ParamDeclList> parameter_list
%type <ParamDeclListBool> parameter_type_list
%type <TypeList> type_name_list type_params_opt
%type <DesignatorList> designation designator_list
%type <Designator> designator
/* start production */
%start prog
%%

prog:
  translation_unit
    { $$ = $!1; parse_result = $1; }

translation_unit:
  external_declaration
    { $$=$!1; }
| external_declaration translation_unit
    { $$=^$(List::imp_append($1,$2)); }
/* Cyc: added using and namespace */
/* NB: using_action calls Lex::enter_using */
| using_action ';' translation_unit
    { $$=^$(^list(^decl(^raw_decl.UsingDecl(^($1,$3)),LOC(@1,@3)),
                                                             null));
      Lex::leave_using();
    }
| using_action '{' translation_unit unusing_action translation_unit_opt
    { $$=^$(^list(^decl(^raw_decl.UsingDecl(^($1,$3)),LOC(@1,@4)),$5));
    }
/* NB: namespace_action calls Lex::enter_namespace */
| namespace_action ';' translation_unit
    { $$=^$(^list(^decl(^raw_decl.NamespaceDecl(^($1,$3)),LOC(@1,@3)),
                  null));
      Lex::leave_namespace();
    }
| namespace_action '{' translation_unit unnamespace_action translation_unit_opt
    { $$=^$(^list(^decl(^raw_decl.NamespaceDecl(^($1,$3)),LOC(@1,@4)),
                  $5));
    }
;

translation_unit_opt:
    /* empty */    { $$=^$(null); }
| translation_unit { $$=$!1; }

external_declaration:
  function_definition
    { _ d = new_decl(^raw_decl.FnDecl($1),LOC(@1,@1));
      $$=^$(^list(d,null)); }
| declaration
    { $$=$!1; }
;

function_definition:
  declarator compound_statement
    { $$=^$(make_function(null,$1,null,$2,LOC(@1,@2))); }
| declaration_specifiers declarator compound_statement
    { $$=^$(make_function(^Opt($1),$2,null,$3,LOC(@1,@3))); }
| declarator declaration_list compound_statement
    { $$=^$(make_function(null,$1,$2,$3,LOC(@1,@3))); }
| declaration_specifiers declarator declaration_list compound_statement
    { $$=^$(make_function(^Opt($1),$2,$3,$4,LOC(@1,@4))); }
;

/* Used for nested functions; the optional declaration_specifiers
   would cause parser conflicts */
function_definition2:
  declaration_specifiers declarator compound_statement
    { $$=^$(make_function(^Opt($1),$2,null,$3,LOC(@1,@3))); }
| declaration_specifiers declarator declaration_list compound_statement
    { $$=^$(make_function(^Opt($1),$2,$3,$4,LOC(@1,@4))); }
;

using_action:
  USING qual_opt_identifier { Lex::enter_using($2); $$=$!2; }
;

unusing_action:
  '}' { Lex::leave_using(); }
;

namespace_action:
  NAMESPACE IDENTIFIER { Lex::enter_namespace($2); $$=$!2; }
;

unnamespace_action:
  '}' { Lex::leave_namespace(); }
;

/***************************** DECLARATIONS *****************************/
declaration:
  declaration_specifiers ';'
    { $$=^$(make_declarations(^Opt($1),null,LOC(@1,@1))); }
| declaration_specifiers init_declarator_list ';'
    { $$=^$(make_declarations(^Opt($1),$2,LOC(@1,@2))); }
/* Cyc:  let declaration */
| LET pattern '=' expression ';'
    { $$=^$(^list(let_decl($2,null,$4,LOC(@1,@5)),null)); }
;

declaration_list:
  declaration
    { $$=$!1; }
| declaration_list declaration
    { $$=^$(List::imp_append($1,$2)); }
;

declaration_specifiers:
  storage_class_specifier
    { $$=^$(^decl_spec(^Opt($1),empty_tqual(),null,false)); }
| storage_class_specifier declaration_specifiers
    { if ($2.storage_class!=null)
        warn("Only one storage class is allowed in a declaration",LOC(@1,@2));
      $$=^$(^decl_spec(^Opt($1),$2.tqual,$2.type_specs,false)); }
| type_specifier
    { $$=^$(^decl_spec(null,empty_tqual(),
                       ^list($1,null),false)); }
| type_specifier declaration_specifiers
    { $$=^$(^decl_spec($2.storage_class,$2.tqual,
                       ^list($1,$2.type_specs),false)); }
| type_qualifier
    { $$=^$(^decl_spec(null,$1,null,false)); }
| type_qualifier declaration_specifiers
    { $$=^$(^decl_spec($2.storage_class,
                       combine_tqual($1,$2.tqual),
                       $2.type_specs,
                       false)); }
| INLINE
    { $$=^$(^decl_spec(null,empty_tqual(),null,true)); }
| INLINE declaration_specifiers
    { $$=^$(^decl_spec($2.storage_class,$2.tqual,
                       $2.type_specs,true)); }
;

storage_class_specifier:
  AUTO
    { $$=^$(^storage_class.Auto); }
| REGISTER
    { $$=^$(^storage_class.Register); }
| STATIC
    { $$=^$(^storage_class.Static); }
| EXTERN
    { $$=^$(^storage_class.Extern); }
| TYPEDEF
    { $$=^$(^storage_class.Typedef); }
/* Cyc:  exception and abstract specifiers */
| ABSTRACT
    { $$=^$(^storage_class.Abstract); }
;

/***************************** TYPES *****************************/
/* we could be parsing a type or a type declaration (e.g., struct)
 * so we return a type_specifier value -- either a type, a type
 * qualifier, an integral type qualifier (signed, long, etc.)
 * or a declaration.
 */
type_specifier:
  VOID
    { $$=^$(type_spec(^typ.VoidType,LOC(@1,@1))); }
| CHAR
    { $$=^$(type_spec(^typ.IntType(^(^sign.Unsigned, ^size_of.B1,
                                     ^boxed.Unboxed)),LOC(@1,@1))); }
| SHORT
    { $$=^$(^type_specifier.Short(^(LOC(@1,@1),false))); }
| INT
    { $$=^$(type_spec(^typ.IntType(^(^sign.Signed, ^size_of.B4,
                                     ^boxed.Unboxed)),LOC(@1,@1))); }
| LONG
    { $$=^$(^type_specifier.Long(^(LOC(@1,@1),false))); }
| FLOAT
    { $$=^$(type_spec(^typ.FloatType(^boxed.Unboxed),LOC(@1,@1))); }
| DOUBLE
    { $$=^$(type_spec(^typ.DoubleType(^boxed.Unboxed),LOC(@1,@1))); }
| SIGNED
    { $$=^$(^type_specifier.Signed(LOC(@1,@1))); }
| UNSIGNED
    { $$=^$(^type_specifier.Unsigned(LOC(@1,@1))); }
| struct_or_union_specifier
    { $$=$!1; }
| enum_specifier
    { $$=$!1; }
/* Cyc: added optional type parameters to typedef'd names */
| QUAL_TYPEDEF_NAME type_params_opt
    { $$=^$(type_spec(^typ.TypedefType(^($1,$2,null)),LOC(@1,@2))); }
/* Cyc: everything below here is an addition */
| TYPE_VAR
    { $$=^$(type_spec(^typ.VarType($1),LOC(@1,@1))); }
| BOOL
    { $$=^$(type_spec(^typ.BoolType(^boxed.Unboxed),LOC(@1,@1))); }
| BOXED_BOOL
    { $$=^$(type_spec(^typ.BoolType(^boxed.Boxed),LOC(@1,@1))); }
| BOXED_CHAR
    { $$=^$(type_spec(^typ.IntType(^(^sign.Unsigned, ^size_of.B1,
                                     ^boxed.Boxed)),LOC(@1,@1))); }
| BOXED_SHORT
    { $$=^$(^type_specifier.Short(^(LOC(@1,@1),true))); }
| BOXED_INT
    { $$=^$(type_spec(^typ.IntType(^(^sign.Signed, ^size_of.B4,
                                     ^boxed.Boxed)),LOC(@1,@1))); }
| BOXED_LONG
    { $$=^$(^type_specifier.Long(^(LOC(@1,@1),true))); }
| BOXED_FLOAT
    { $$=^$(type_spec(^typ.FloatType(^boxed.Boxed),LOC(@1,@1))); }
| BOXED_DOUBLE
    { $$=^$(type_spec(^typ.DoubleType(^boxed.Boxed),LOC(@1,@1))); }
| '$' '(' parameter_list ')'
    { $$=^$(type_spec(^typ.TupleType(List::map_c(get_tqual_typ,
                                                 LOC(@3,@3),
                                                 List::imp_rev($3))),
                      LOC(@1,@4))); }
;

type_qualifier:
  CONST
    { $$=^$(^tqual(true,false,false)); }
| VOLATILE
    { $$=^$(^tqual(false,true,false)); }
| RESTRICT
    { $$=^$(^tqual(false,false,true)); }

/* parsing of struct and union specifiers. */
struct_or_union_specifier:
  struct_or_union '{' struct_declaration_list '}'
    { decl d;
      switch ($1) {
      case Struct:
        d = struct_decl(^scope.Public,null,null,^Opt($3),LOC(@1,@4));
      case Union:
        unimp2("unions",LOC(@1,@4));
        d = new_decl(^raw_decl.UnionDecl,LOC(@1,@4));
      }
      $$=^$(^type_specifier.Decl(d));
      unimp2("anonymous structs/unions",LOC(@1,@4));
    }
/* Cyc:  type_params_opt are added */
| struct_or_union qual_opt_identifier type_params_opt
  '{' struct_declaration_list '}'
    { _ ts = List::map_c(typ2tvar,LOC(@3,@3),$3);
      decl d;
      switch ($1) {
      case Struct:
        d = struct_decl(^scope.Public,^Opt($2),ts,^Opt($5),LOC(@1,@6));
      case Union:
        unimp2("unions",LOC(@1,@6));
        d = new_decl(^raw_decl.UnionDecl,LOC(@1,@6));
      }
      $$=^$(^type_specifier.Decl(d));
    }
// Hack to allow struct/union names and typedef names to overlap
| struct_or_union QUAL_TYPEDEF_NAME type_params_opt
  '{' struct_declaration_list '}'
    { _ ts = List::map_c(typ2tvar,LOC(@3,@3),$3);
      decl d;
      switch ($1) {
      case Struct:
        d = struct_decl(^scope.Public,^Opt($2),ts,^Opt($5),LOC(@1,@6));
      case Union:
        unimp2("unions",LOC(@1,@6));
        d = new_decl(^raw_decl.UnionDecl,LOC(@1,@6));
      }
      $$=^$(^type_specifier.Decl(d));
    }
/* Cyc:  type_params_opt are added */
| struct_or_union qual_opt_identifier type_params_opt
    { switch ($1) {
      case Struct:
        $$=^$(type_spec(^typ.StructType(^(^Opt($2),$3)),LOC(@1,@3)));
      case Union:
        unimp2("unions",LOC(@1,@3));
        $$=^$(^type_specifier.Decl(new_decl(^raw_decl.UnionDecl,
                                            LOC(@1,@3))));
      }
    }
// Hack to allow struct/union names and typedef names to overlap
| struct_or_union QUAL_TYPEDEF_NAME type_params_opt
    { switch ($1) {
      case Struct:
        $$=^$(type_spec(^typ.StructType(^(^Opt($2),$3)),LOC(@1,@3)));
      case Union:
        unimp2("unions",LOC(@1,@3));
        $$=^$(^type_specifier.Decl(new_decl(^raw_decl.UnionDecl,
                                            LOC(@1,@3))));
      }
    }
;

type_params_opt:
  /* empty */
    { $$=^$(null); }
| '<' type_name_list '>'
    { $$=^$(List::imp_rev($2)); }
| '<' type_name_list RIGHT_OP
    { /* RIGHT_OP is >>, we've seen one char too much, step back */
      lbuf.v.lex_curr_pos -= 1;
      $$=^$(List::imp_rev($2)); }
;

struct_or_union:
  STRUCT
    { $$=^$(^struct_or_union.Struct); }
| UNION
    { $$=^$(^struct_or_union.Union); }
;

struct_declaration_list:
  struct_declaration_list0
    { $$=^$(List::flatten(List::imp_rev($1))); }
;

/* NB: returns list in reverse order */
struct_declaration_list0:
  struct_declaration
    { $$=^$(^list($1,null)); }
| struct_declaration_list0 struct_declaration
    { $$=^$(^list($2,$1)); }
;

init_declarator_list:
  init_declarator_list0
    { $$=^$(List::imp_rev($1)); }
;

/* NB: returns list in reverse order */
init_declarator_list0:
  init_declarator
    { $$=^$(^list($1,null)); }
| init_declarator_list0 ',' init_declarator
    { $$=^$(^list($3,$1)); }
;

init_declarator:
  declarator
    { $$=^$(^($1,null)); }
| declarator '=' initializer
    { $$=^$(^($1,^Opt($3))); }
;

struct_declaration:
  specifier_qualifier_list struct_declarator_list ';'
    {
      /* when we collapse the specifier_qualifier_list and
       * struct_declarator_list, we get a list of (1) optional id,
       * (2) type, and (3) in addition any nested struct, union,
       * or enum declarations.  For now, we warn about the nested
       * declarations.  We must check that each id is actually present
       * and then convert this to a list of struct fields: (1) id,
       * (2) tqual, (3) type. */
      tqual tq = $1.1;
      <type_specifier>list tss = $1.2;
      _ ts_info = collapse_type_specifiers(tss,LOC(@1,@1));
      if (ts_info.2 != null)
        warn("struct declaration contains nested type declaration",LOC(@1,@1));
      _ t = ts_info.1;
      _ info = apply_tmss(tq,t,$2);
      $$=^$(List::map_c(make_struct_field,LOC(@1,@2),info));
    }
;

specifier_qualifier_list:
  type_specifier
    { $$=^$(^(empty_tqual(),^list($1,null))); }
| type_specifier specifier_qualifier_list
    { $$=^$(^($2.1,^list($1,$2.2))); }
| type_qualifier
    { $$=^$(^($1,null)); }
| type_qualifier specifier_qualifier_list
    { $$=^$(^(combine_tqual($1,$2.1),$2.2)); }

struct_declarator_list:
  struct_declarator_list0
    { $$=^$(List::imp_rev($1)); }
;

/* NB: returns list in reverse order */
struct_declarator_list0:
  struct_declarator
    { $$=^$(^list($1,null)); }
| struct_declarator_list0 ',' struct_declarator
    { $$=^$(^list($3,$1)); }
;

struct_declarator:
  declarator
    { $$=$!1; }
| ':' constant_expression
    { /* FIX: TEMPORARY TO TEST PARSING */
      unimp2("bit fields",LOC(@1,@2));
      $$=^$(^declarator(^(null,""),null)); }
| declarator ':' constant_expression
    { unimp2("bit fields",LOC(@1,@2));
      $$=$!1; }
;

enum_specifier:
  ENUM '{' enumerator_list '}'
    { decl d  = enum_decl(^scope.Public,null,null,^Opt($3),LOC(@1,@4));
      $$ = ^$(^type_specifier.Decl(d));
      unimp2("anonymous enums",LOC(@1,@4));
    }
/* Cyc: added type params */
| ENUM qual_opt_identifier type_params_opt '{' enumerator_list '}'
    { _ ts = List::map_c(typ2tvar,LOC(@3,@3),$3);
      decl d  = enum_decl(^scope.Public,^Opt($2),ts,^Opt($5),LOC(@1,@6));
      $$ = ^$(^type_specifier.Decl(d));
    }
| ENUM qual_opt_identifier type_params_opt
    { $$=^$(type_spec(^typ.EnumType(^(^Opt($2),$3)),LOC(@1,@3)));
    }
| XENUM qual_opt_identifier '{' enumerator_list '}'
    { decl d  = xenum_decl(^scope.Public,$2,$4,LOC(@1,@5));
      $$ = ^$(^type_specifier.Decl(d));
    }
| XENUM qual_opt_identifier
    { $$=^$(type_spec(^typ.XenumType($2),LOC(@1,@2)));
    }
;

enumerator_list:
  enumerator
    { $$=^$(^list($1,null)); }
| enumerator ';'
    { $$=^$(^list($1,null)); }
| enumerator ',' enumerator_list
    { $$=^$(^list($1,$3)); }
| enumerator ';' enumerator_list
    { $$=^$(^list($1,$3)); }
;

enumerator:
  qual_opt_identifier
    { $$=^$(^enumfield($1,null,null,null,LOC(@1,@1))); }
| qual_opt_identifier '=' constant_expression
    { $$=^$(^enumfield($1,^Opt($3),null,null,LOC(@1,@3))); }
/* Cyc: value-carrying enumerators */
| qual_opt_identifier type_params_opt '(' parameter_list ')'
    { _ typs = List::map_c(get_tqual_typ,LOC(@4,@4),
                           List::imp_rev($4));
      _ tvs = List::map_c(typ2tvar,LOC(@2,@2),$2);
      $$=^$(^enumfield($1,null,tvs,typs,LOC(@1,@5))); }

declarator:
  direct_declarator
    { $$=$!1; }
| pointer direct_declarator
    { $$=^$(^declarator($2.id,List::append($1,$2.tms))); }
;

direct_declarator:
  qual_opt_identifier
    { $$=^$(^declarator($1,null)); }
| '(' declarator ')'
    { $$=$!2; }
| direct_declarator '[' ']'
    { $$=^$(^declarator($1.id,^list(^type_modifier.Carray,$1.tms))); }
| direct_declarator '[' assignment_expression ']'
    { $$=^$(^declarator($1.id,^list(^type_modifier.ConstArray($3),$1.tms))); }
| direct_declarator '(' parameter_type_list ')'
    { $$=^$(^declarator($1.id,
                        ^list(^type_modifier.Function(^funcparams.WithTypes($3)),
                        $1.tms)));
    }
| direct_declarator '(' ')'
    { $$=^$(^declarator($1.id,
                        ^list(^type_modifier.Function(^funcparams.WithTypes(^(null,false))),
                        $1.tms)));
    }
| direct_declarator '(' identifier_list ')'
    { $$=^$(^declarator($1.id,
                        ^list(^type_modifier.Function(^funcparams.NoTypes($3)),
                        $1.tms)));
    }
/* Cyc: new kind of array */
| direct_declarator '[' '?' ']'
    { $$=^$(^declarator($1.id,^list(^type_modifier.Array,$1.tms))); }
/* Cyc: added type parameters */
| direct_declarator '<' type_name_list '>'
    { _ ts = List::map_c(typ2tvar,LOC(@2,@4),List::imp_rev($3));
      $$=^$(^declarator($1.id,
                        ^list(^type_modifier.TypeParams(ts),$1.tms)));
    }
| direct_declarator '<' type_name_list RIGHT_OP
    { /* RIGHT_OP is >>, we've seen one char too much, step back */
      lbuf.v.lex_curr_pos -= 1;
      _ ts = List::map_c(typ2tvar,LOC(@2,@4),List::imp_rev($3));
      $$=^$(^declarator($1.id,
                        ^list(^type_modifier.TypeParams(ts),$1.tms)));
    }
;

pointer:
  '*'
    { $$=^$(^list(^type_modifier.Pointer(^(true,empty_tqual())),null)); }
| '*' type_qualifier_list
    { $$=^$(^list(^type_modifier.Pointer(^(true,$2)),null)); }
| '*' pointer
    { $$=^$(^list(^type_modifier.Pointer(^(true,empty_tqual())),$2)); }
| '*' type_qualifier_list pointer
    { $$=^$(^list(^type_modifier.Pointer(^(true,$2)),$3)); }
| '@'
    { $$=^$(^list(^type_modifier.Pointer(^(false,empty_tqual())),null)); }
| '@' type_qualifier_list
    { $$=^$(^list(^type_modifier.Pointer(^(false,$2)),null)); }
| '@' pointer
    { $$=^$(^list(^type_modifier.Pointer(^(false,empty_tqual())),$2)); }
| '@' type_qualifier_list pointer
    { $$=^$(^list(^type_modifier.Pointer(^(false,$2)),$3)); }
;

type_qualifier_list:
  type_qualifier
    { $$ = $!1; }
| type_qualifier_list type_qualifier
    { $$ = ^$(combine_tqual($1,$2)); }
;

parameter_type_list:
  parameter_list
    { $$=^$(^(List::imp_rev($1),false)); }
| parameter_list ',' ELLIPSIS
    { $$=^$(^(List::imp_rev($1),true));
    }
;

/* NB: returns list in reverse order */
parameter_list:
  parameter_declaration
    { $$=^$(^list($1,null)); }
| parameter_list ',' parameter_declaration
    { $$=^$(^list($3,$1)); }
;

/* TODO: differs from grammar in K&R */
parameter_declaration:
  specifier_qualifier_list declarator
    { _ tss = $1.2;
      _ ts_info = collapse_type_specifiers(tss,LOC(@1,@1));
      if (ts_info.2 != null)
        warn("parameter contains nested type declaration",LOC(@1,@1));
      _ t = ts_info.1;
      _ tq = $1.1;
      _ tms = $2.tms;
      _ t_info = apply_tms(tq,t,tms);
      if (t_info.3 != null)
        err("parameter with bad type params",LOC(@2,@2));
      _ q = $2.id;
      if (q.1 != null)
        err("parameter cannot be qualified with a module name",LOC(@1,@1));
      _ idopt = ^Opt(q.2);
      $$=^$(^(idopt,t_info.1,t_info.2));
    }
| specifier_qualifier_list
    { _ tss = $1.2;
      _ ts_info = collapse_type_specifiers(tss,LOC(@1,@1));
      if (ts_info.2 != null)
        warn("nested type declaration, ignoring",LOC(@1,@1));
      _ t = ts_info.1;
      _ tq = $1.1;
      $$=^$(^(null,tq,t));
    }
| specifier_qualifier_list abstract_declarator
    { _ tss = $1.2;
      _ ts_info = collapse_type_specifiers(tss,LOC(@1,@1));
      if (ts_info.2 != null)
        warn("nested type declaration, ignoring",LOC(@1,@1));
      _ t = ts_info.1;
      _ tq = $1.1;
      _ tms = $2.tms;
      _ t_info = apply_tms(tq,t,tms);
      if (t_info.3 != null)
        // Ex: int (@)<`a>
        warn("bad type params, ignoring",LOC(@2,@2));
      $$=^$(^(null,t_info.1,t_info.2));
    }
/*
| type_name
    { $$=$!1; }
*/
;

identifier_list:
  identifier_list0
    { $$=^$(List::imp_rev($1)); }
;

/* NB: returns list in reverse order */
identifier_list0:
  IDENTIFIER
    { $$=^$(^list($1,null)); }
| identifier_list0 ',' IDENTIFIER
    { $$=^$(^list($3,$1)); }
;

initializer:
  assignment_expression
    { $$=$!1; }
| '{' '}'
    { $$=^$(new_exp(^raw_exp.UnresolvedMem(^(null,null)),
                    LOC(@1,@2))); }
| '{' initializer_list '}'
    { $$=^$(new_exp(^raw_exp.UnresolvedMem(^(null,List::imp_rev($2))),
                    LOC(@1,@3))); }
| '{' initializer_list ',' '}'
    { $$=^$(new_exp(^raw_exp.UnresolvedMem(^(null,List::imp_rev($2))),
                    LOC(@1,@3))); }
;

/* NB: returns list in reverse order */
initializer_list:
  initializer
    { $$=^$(^list(^(null,$1),null)); }
| designation initializer
    { $$=^$(^list(^($1,$2),null)); }
| initializer_list ',' initializer
    { $$=^$(^list(^(null,$3),$1)); }
| initializer_list ',' designation initializer
    { $$=^$(^list(^($3,$4),$1)); }
;

designation:
  designator_list '='
    {$$=^$(List::imp_rev($1));}
;

/* NB: returns list in reverse order */
designator_list:
  designator
    { $$=^$(^list($1,null)); }
| designator_list designator
    { $$=^$(^list($2,$1)); }
;

designator:
  '[' constant_expression ']'
    {$$ = ^$(^designator.ArrayElement($2));}
| '.' IDENTIFIER
    {$$ = ^$(^designator.FieldName($2));}
;

type_name:
  specifier_qualifier_list
    { _ tss = $1.2;
      _ ts_info = collapse_type_specifiers(tss,LOC(@1,@1));
      if (ts_info.2 != null)
        warn("nested type declaration, ignoring",LOC(@1,@1));
      _ t = ts_info.1;
      _ tq = $1.1;
      $$=^$(^(null,tq,t));
    }
| specifier_qualifier_list abstract_declarator
    { _ tss = $1.2;
      _ ts_info = collapse_type_specifiers(tss,LOC(@1,@1));
      if (ts_info.2 != null)
        warn("nested type declaration, ignoring",LOC(@1,@1));
      _ t = ts_info.1;
      _ tq = $1.1;
      _ tms = $2.tms;
      _ t_info = apply_tms(tq,t,tms);
      if (t_info.3 != null)
        // Ex: int (@)<`a>
        warn("bad type params, ignoring",LOC(@2,@2));
      $$=^$(^(null,t_info.1,t_info.2));
    }
;

/* Cyc: new */
/* NB: returns list in reverse order */
type_name_list:
  type_name
    { _ x = $1;
      $$=^$(^list(x.3,null)); }
| type_name_list ',' type_name
    { _ x = $3;
      $$=^$(^list(x.3,$1)); }
;

abstract_declarator:
  pointer
    { $$=^$(^abstractdeclarator($1)); }
| direct_abstract_declarator
    { $$=$!1; }
| pointer direct_abstract_declarator
    { $$=^$(^abstractdeclarator(List::append($1,$2.tms))); }
;

direct_abstract_declarator:
  '(' abstract_declarator ')'
    { $$=$!2; }
| '[' ']'
    { $$=^$(^abstractdeclarator(^list(^type_modifier.Carray,null))); }
| direct_abstract_declarator '[' ']'
    { $$=^$(^abstractdeclarator(^list(^type_modifier.Carray,$1.tms))); }
| '[' assignment_expression ']'
    { $$=^$(^abstractdeclarator(^list(^type_modifier.ConstArray($2),null))); }
| direct_abstract_declarator '[' assignment_expression ']'
    { $$=^$(^abstractdeclarator(^list(^type_modifier.ConstArray($3),$1.tms))); }
| '(' ')'
    { $$=^$(^abstractdeclarator(^list(^type_modifier.Function(^funcparams.WithTypes(^(null,false))),null)));
    }
| '(' parameter_type_list ')'
    { $$=^$(^abstractdeclarator(^list(^type_modifier.Function(^funcparams.WithTypes($2)),null)));
    }
| direct_abstract_declarator '(' ')'
    { $$=^$(^abstractdeclarator(^list(^type_modifier.Function(^funcparams.WithTypes(^(null,false))),$1.tms)));
    }
| direct_abstract_declarator '(' parameter_type_list ')'
    { $$=^$(^abstractdeclarator(^list(^type_modifier.Function(^funcparams.WithTypes($3)),$1.tms)));
    }
/* Cyc: new */
| '[' '?' ']'
    { $$=^$(^abstractdeclarator(^list(^type_modifier.Array,null))); }
| direct_abstract_declarator '[' '?' ']'
    { $$=^$(^abstractdeclarator(^list(^type_modifier.Array,$1.tms))); }
/* Cyc: new */
| direct_abstract_declarator '<' type_name_list '>'
    { _ ts = List::map_c(typ2tvar,LOC(@2,@4),List::imp_rev($3));
      $$=^$(^abstractdeclarator(^list(^type_modifier.TypeParams(ts),$1.tms)));
    }
| direct_abstract_declarator '<' type_name_list RIGHT_OP
    { /* RIGHT_OP is >>, we've seen one char too much, step back */
      lbuf.v.lex_curr_pos -= 1;
      _ ts = List::map_c(typ2tvar,LOC(@2,@4),List::imp_rev($3));
      $$=^$(^abstractdeclarator(^list(^type_modifier.TypeParams(ts),$1.tms)));
    }
;

/***************************** STATEMENTS *****************************/
statement:
  labeled_statement
    { $$=$!1; }
| expression_statement
    { $$=$!1; }
| compound_statement
    { $$=$!1; }
| selection_statement
    { $$=$!1; }
| iteration_statement
    { $$=$!1; }
| jump_statement
    { $$=$!1; }
/* Cyc: cut and splice */
| CUT statement
    { $$=^$(new_stmt(^raw_stmt.Cut($2),LOC(@1,@2))); }
| SPLICE statement
    { $$=^$(new_stmt(^raw_stmt.Splice($2),LOC(@1,@2))); }
;

/* Cyc: Unlike C, we do not treat case and default statements as
   labeled */
labeled_statement:
  IDENTIFIER ':' statement
    { $$=^$(new_stmt(^raw_stmt.Label(^($1,$3)),LOC(@1,@3))); }
;

expression_statement:
  ';'
    { $$=^$(skip_stmt(LOC(@1,@1))); }
| expression ';'
    { $$=^$(exp_stmt($1,LOC(@1,@2))); }
;

compound_statement:
  '{' '}'
     { $$=^$(skip_stmt(LOC(@1,@2))); }
| '{' block_item_list '}'
     { $$=$!2; }
;

block_item_list:
  block_item
    { switch ($1) {
      case TopDecls(ds):
        $$=^$(flatten_declarations(ds,skip_stmt(LOC(@1,@1))));
      case FnDecl(fd):
        $$=^$(flatten_decl(new_decl(^raw_decl.FnDecl(fd),LOC(@1,@1)),
                           skip_stmt(LOC(@1,@1))));
      case Stmt(s):
        $$=^$(s);
      }
    }
| block_item block_item_list
    { switch ($1) {
      case TopDecls(ds):
        $$=^$(flatten_declarations(ds,$2));
      case FnDecl(fd):
        $$=^$(flatten_decl(new_decl(^raw_decl.FnDecl(fd),LOC(@1,@1)),
                           $2));
      case Stmt(s):
        $$=^$(seq_stmt(s,$2,LOC(@1,@2)));
      }
    }
;

block_item:
  declaration
    { $$=^$(^blockitem.TopDecls($1)); }
| statement
    { $$=^$(^blockitem.Stmt($1)); }
/* Cyc: nested function definitions.
   The initial (return) type is required,
   to avoid parser conflicts. */
| function_definition2
    { $$=^$(^blockitem.FnDecl($1)); }

/* This has the standard shift-reduce conflict which is properly
   resolved. */
selection_statement:
  IF '(' expression ')' statement
    { $$=^$(ifthenelse_stmt($3,$5,skip_stmt(DUMMYLOC),LOC(@1,@5))); }
| IF '(' expression ')' statement ELSE statement
    { $$=^$(ifthenelse_stmt($3,$5,$7,LOC(@1,@7))); }
/* Cyc: the body of the switch statement cannot be an arbitrary
 * statement; it must be a list of switch_clauses */
| SWITCH '(' expression ')' '{' switch_clauses '}'
    { $$=^$(new_stmt(^raw_stmt.Switch(^($3,$6)),LOC(@1,@7))); }
| TRY statement CATCH '{' switch_clauses '}'
    { $$=^$(new_stmt(^raw_stmt.TryCatch(^($2,$5)),LOC(@1,@6))); }
;

/* Cyc: unlike C, we only allow default or case statements within
 * switches.  Also unlike C, we support a more general form of pattern
 * matching within cases. */
switch_clauses:
  /* empty */
    { $$=^$(null); }
| DEFAULT ':' block_item_list
    { $$=^$(^list(^switch_clause(new_pat(^raw_pat.Wild,LOC(@1,@1)),
                                 null,$3,LOC(@1,@3)),
                  null));}
| CASE pattern ':' switch_clauses
    { $$=^$(^list(^switch_clause($2,null,skip_stmt(LOC(@3,@3)),
                                 LOC(@1,@4)),$4)); }
| CASE pattern ':' block_item_list switch_clauses
    { $$=^$(^list(^switch_clause($2,null,$4,LOC(@1,@4)),$5)); }
| CASE pattern WHERE expression ':' switch_clauses
    { $$=^$(^list(^switch_clause($2,^Opt($4),skip_stmt(LOC(@5,@5)),
                                 LOC(@1,@6)),$6)); }
| CASE pattern WHERE expression ':' block_item_list switch_clauses
    { $$=^$(^list(^switch_clause($2,^Opt($4),$6,LOC(@1,@7)),$7)); }
;

iteration_statement:
  WHILE '(' expression ')' statement
    { $$=^$(while_stmt($3,$5,LOC(@1,@5))); }
| DO statement WHILE '(' expression ')' ';'
    { $$=^$(do_stmt($2,$5,LOC(@1,@7))); }
| FOR '(' ';' ';' ')' statement
    { $$=^$(for_stmt(signed_int_exp(0,DUMMYLOC),bool_exp(true,DUMMYLOC),
                     signed_int_exp(0,DUMMYLOC),$6,LOC(@1,@6))); }
| FOR '(' ';' ';' expression ')' statement
    { $$=^$(for_stmt(signed_int_exp(0,DUMMYLOC),bool_exp(true,DUMMYLOC),
                             $5,$7,LOC(@1,@7))); }
| FOR '(' ';' expression ';' ')' statement
    { $$=^$(for_stmt(signed_int_exp(0,DUMMYLOC),$4,
                     signed_int_exp(0,DUMMYLOC),$7,LOC(@1,@7)));}
| FOR '(' ';' expression ';' expression ')' statement
    { $$=^$(for_stmt(signed_int_exp(0,DUMMYLOC),$4,$6,$8,LOC(@1,@7))); }
| FOR '(' expression ';' ';' ')' statement
    { $$=^$(for_stmt($3,bool_exp(true,DUMMYLOC),
                     signed_int_exp(0,DUMMYLOC),$7,LOC(@1,@7))); }
| FOR '(' expression ';' ';' expression ')' statement
    { $$=^$(for_stmt($3,bool_exp(true,DUMMYLOC),$6,$8,LOC(@1,@8))); }
| FOR '(' expression ';' expression ';' ')' statement
    { $$=^$(for_stmt($3,$5,signed_int_exp(0,DUMMYLOC),$8,LOC(@1,@8))); }
| FOR '(' expression ';' expression ';' expression ')' statement
    { $$=^$(for_stmt($3,$5,$7,$9,LOC(@1,@9))); }
| FOR '(' declaration ';' ')' statement
    { _ decls = $3;
      _ s = for_stmt(signed_int_exp(0,DUMMYLOC),bool_exp(true,DUMMYLOC),
                     signed_int_exp(0,DUMMYLOC),$6,LOC(@1,@6));
      $$=^$(flatten_declarations(decls,s));
    }
| FOR '(' declaration expression ';' ')' statement
    { _ decls = $3;
      _ s = for_stmt(signed_int_exp(0,DUMMYLOC),$4,
                     signed_int_exp(0,DUMMYLOC),$7,LOC(@1,@7));
      $$=^$(flatten_declarations(decls,s));
    }
| FOR '(' declaration ';' expression ')' statement
    { _ decls = $3;
      _ s = for_stmt(signed_int_exp(0,DUMMYLOC),bool_exp(true,DUMMYLOC),
                     $5,$7,LOC(@1,@7));
      $$=^$(flatten_declarations(decls,s));
    }
| FOR '(' declaration expression ';' expression ')' statement
    { _ decls = $3;
      _ s = for_stmt(signed_int_exp(0,DUMMYLOC),$4,$6,$8,LOC(@1,@8));
      $$=^$(flatten_declarations(decls,s));
    }
;

jump_statement:
  GOTO IDENTIFIER ';'
    { $$=^$(new_stmt(^raw_stmt.Goto($2),LOC(@1,@2)));
    }
| CONTINUE ';'
    { $$=^$(new_stmt(^raw_stmt.Continue,LOC(@1,@1)));}
| BREAK ';'
    { $$=^$(new_stmt(^raw_stmt.Break,LOC(@1,@1)));}
| RETURN ';'
    { $$=^$(new_stmt(^raw_stmt.Return(null),LOC(@1,@1)));}
| RETURN expression ';'
    { $$=^$(new_stmt(^raw_stmt.Return(^Opt($2)),LOC(@1,@2)));}
/* Cyc:  explicit fallthru for switches */
| FALLTHRU ';'
    { $$=^$(new_stmt(^raw_stmt.Fallthru,LOC(@1,@1)));}
;

/***************************** PATTERNS *****************************/
/* Cyc:  patterns */
pattern:
  '_'
    {$$=^$(new_pat(^raw_pat.Wild,LOC(@1,@1)));}
| '(' pattern ')'
    { $$=$!2; }
| INTEGER_CONSTANT
    {$$=^$(new_pat(^raw_pat.Int($1),LOC(@1,@1)));}
| '-' INTEGER_CONSTANT
    {$$=^$(new_pat(^raw_pat.Int(^(^sign.Signed,-(snd($2)))),LOC(@1,@2)));}
| FLOATING_CONSTANT
    {$$=^$(new_pat(^raw_pat.Float($1),LOC(@1,@1)));}
/* TODO: we should allow negated floating constants too */
| CHARACTER_CONSTANT
    {$$=^$(new_pat(^raw_pat.Char($1),LOC(@1,@1)));}
| NULL
    {$$=^$(new_pat(^raw_pat.Null,LOC(@1,@1)));}
| TRUE
    {$$=^$(new_pat(^raw_pat.Bool(true),LOC(@1,@1)));}
| FALSE
    {$$=^$(new_pat(^raw_pat.Bool(false),LOC(@1,@1)));}
| qual_opt_identifier
    { $$=^$(new_pat(^raw_pat.UnknownId($1),LOC(@1,@1))); }
| qual_opt_identifier type_params_opt '(' tuple_pattern_list ')'
    { _ tvs = List::map_c(typ2tvar,LOC(@2,@2),$2);
      $$=^$(new_pat(^raw_pat.UnknownCall(^($1,tvs,$4)),LOC(@1,@5)));
    }
| '$' '(' tuple_pattern_list ')'
    {$$=^$(new_pat(^raw_pat.Tuple($3),LOC(@1,@4)));}
| qual_opt_identifier type_params_opt '{' '}'
    { _ tvs = List::map_c(typ2tvar,LOC(@2,@2),$2);
      $$=^$(new_pat(^raw_pat.UnknownFields(^($1,tvs,null)),LOC(@1,@4)));
    }
| qual_opt_identifier type_params_opt '{' field_pattern_list '}'
    { _ tvs = List::map_c(typ2tvar,LOC(@2,@2),$2);
      $$=^$(new_pat(^raw_pat.UnknownFields(^($1,tvs,$4)),LOC(@1,@5)));
    }
| '&' pattern
    {$$=^$(new_pat(^raw_pat.Pointer($2),LOC(@1,@2)));}
| '*' IDENTIFIER
    {$$=^$(new_pat(^raw_pat.Reference($2),LOC(@1,@2)));}
;

tuple_pattern_list:
  /* empty */
    { $$=^$(null); }
| tuple_pattern_list0
    { $$=^$(List::imp_rev($1)); }
;

/* NB: returns list in reverse order */
tuple_pattern_list0:
  pattern
    {$$=^$(^list($1,null));}
| tuple_pattern_list0 ',' pattern
    {$$=^$(^list($3,$1));}
;

field_pattern:
  pattern
    {$$=^$(^(null,$1));}
| designation pattern
    {$$=^$(^($1,$2));}

field_pattern_list:
  field_pattern_list0
    {$$=^$(List::imp_rev($1));}
;

field_pattern_list0:
  field_pattern
    {$$=^$(^list($1,null));}
| field_pattern_list0 ',' field_pattern
    {$$=^$(^list($3,$1)); }
;
/*
struct_pattern_list:
  struct_pattern_list0
    {$$=^$(List::imp_rev($1));}
;

struct_pattern_list0:
  struct_field_pattern
    {$$=^$(^list($1,null));}
| struct_pattern_list0 ',' struct_field_pattern
    {$$=^$(^list($3,$1)); }
;

struct_field_pattern:
  IDENTIFIER
    {$$=^$(^($1,new_pat(^raw_pat.Var($1),LOC(@1,@1))));}
| IDENTIFIER '=' pattern
    {$$=^$(^($1,$3));}
;
*/

/***************************** EXPRESSIONS *****************************/
expression:
  assignment_expression
    { $$=$!1; }
| expression ',' assignment_expression
    { $$=^$(seq_exp($1,$3,LOC(@1,@3))); }
;

assignment_expression:
  conditional_expression
    { $$=$!1; }
| unary_expression assignment_operator assignment_expression
    { $$=^$(assignop_exp($1,$2,$3,LOC(@1,@3))); }
;

assignment_operator:
  '='
    { $$=^$(null); }
| MUL_ASSIGN
    { $$=^$(^Opt(^primop.Times)); }
| DIV_ASSIGN
    { $$=^$(^Opt(^primop.Div)); }
| MOD_ASSIGN
    { $$=^$(^Opt(^primop.Mod)); }
| ADD_ASSIGN
    { $$=^$(^Opt(^primop.Plus)); }
| SUB_ASSIGN
    { $$=^$(^Opt(^primop.Minus)); }
| LEFT_ASSIGN
    { $$=^$(^Opt(^primop.Bitlshift)); }
| RIGHT_ASSIGN
    { $$=^$(^Opt(^primop.Bitlrshift)); }
| AND_ASSIGN
    { $$=^$(^Opt(^primop.Bitand)); }
| XOR_ASSIGN
    { $$=^$(^Opt(^primop.Bitxor)); }
| OR_ASSIGN
    { $$=^$(^Opt(^primop.Bitor)); }
;

conditional_expression:
  logical_or_expression
    { $$=$!1; }
| logical_or_expression '?' expression ':' conditional_expression
    { $$=^$(conditional_exp($1,$3,$5,LOC(@1,@5))); }
;

constant_expression:
  conditional_expression
    { $$=$!1; }
;

logical_or_expression:
  logical_and_expression
    { $$=$!1; }
| logical_or_expression OR_OP logical_and_expression
    { $$=^$(conditional_exp($1,true_exp(DUMMYLOC),$3,LOC(@1,@3))); }
;

logical_and_expression:
  inclusive_or_expression
    { $$=$!1; }
| logical_and_expression AND_OP inclusive_or_expression
    { $$=^$(conditional_exp($1,$3,false_exp(DUMMYLOC),LOC(@1,@3))); }
;

inclusive_or_expression:
  exclusive_or_expression
    { $$=$!1; }
| inclusive_or_expression '|' exclusive_or_expression
    { $$=^$(prim2_exp(^primop.Bitor,$1,$3,LOC(@1,@3))); }
;

exclusive_or_expression:
  and_expression
    { $$=$!1; }
| exclusive_or_expression '^' and_expression
    { $$=^$(prim2_exp(^primop.Bitxor,$1,$3,LOC(@1,@3))); }
;

and_expression:
  equality_expression
    { $$=$!1; }
| and_expression '&' equality_expression
    { $$=^$(prim2_exp(^primop.Bitand,$1,$3,LOC(@1,@3))); }
;

equality_expression:
  relational_expression
    { $$=$!1; }
| equality_expression EQ_OP relational_expression
    { $$=^$(prim2_exp(^primop.Eq,$1,$3,LOC(@1,@3))); }
| equality_expression NE_OP relational_expression
    { $$=^$(prim2_exp(^primop.Neq,$1,$3,LOC(@1,@3))); }
;

relational_expression:
  shift_expression
    { $$=$!1; }
| relational_expression '<' shift_expression
    { $$=^$(prim2_exp(^primop.Lt,$1,$3,LOC(@1,@3))); }
| relational_expression '>' shift_expression
    { $$=^$(prim2_exp(^primop.Gt,$1,$3,LOC(@1,@3))); }
| relational_expression LE_OP shift_expression
    { $$=^$(prim2_exp(^primop.Lte,$1,$3,LOC(@1,@3))); }
| relational_expression GE_OP shift_expression
    { $$=^$(prim2_exp(^primop.Gte,$1,$3,LOC(@1,@3))); }
;

shift_expression:
  additive_expression
    { $$=$!1; }
| shift_expression LEFT_OP additive_expression
    { $$=^$(prim2_exp(^primop.Bitlshift,$1,$3,LOC(@1,@3))); }
| shift_expression RIGHT_OP additive_expression
  { $$=^$(prim2_exp(^primop.Bitlrshift,$1,$3,LOC(@1,@3))); }
;

additive_expression:
  multiplicative_expression
    { $$=$!1; }
| additive_expression '+' multiplicative_expression
    { $$=^$(prim2_exp(^primop.Plus,$1,$3,LOC(@1,@3))); }
| additive_expression '-' multiplicative_expression
    { $$=^$(prim2_exp(^primop.Minus,$1,$3,LOC(@1,@3))); }
;

multiplicative_expression:
  cast_expression
    { $$=$!1; }
| multiplicative_expression '*' cast_expression
    { $$=^$(prim2_exp(^primop.Times,$1,$3,LOC(@1,@3))); }
| multiplicative_expression '/' cast_expression
    { $$=^$(prim2_exp(^primop.Div,$1,$3,LOC(@1,@3))); }
| multiplicative_expression '%' cast_expression
    { $$=^$(prim2_exp(^primop.Mod,$1,$3,LOC(@1,@3))); }
;

cast_expression:
  unary_expression
    { $$=$!1; }
| '(' type_name ')' cast_expression
    { $$=^$(new_exp(^raw_exp.Cast(^($2.3,$4)),LOC(@1,@4))); }
;

unary_expression:
  postfix_expression
    { $$=$!1; }
| INC_OP unary_expression
    { $$=^$(new_exp(^raw_exp.Increment(^($2,^incrementor.PreInc)),
                    LOC(@1,@2))); 
    }
| DEC_OP unary_expression
    { $$=^$(new_exp(^raw_exp.Increment(^($2,^incrementor.PreDec)),
                    LOC(@1,@2))); 
    }
| '&' cast_expression
    { $$=^$(new_exp(^raw_exp.Address($2),LOC(@1,@2))); }
| '*' cast_expression
    { $$=^$(new_exp(^raw_exp.Deref($2),LOC(@1,@2))); }
| '+' cast_expression
    { $$ = $!2; }
| '-' cast_expression
    { $$ = ^$(prim2_exp(^primop.Minus,signed_int_exp(0,LOC(@1,@2)),
                        $2,LOC(@1,@2))); }
| unary_operator cast_expression
    { $$=^$(prim1_exp($1,$2,LOC(@1,@2))); }
| SIZEOF '(' type_name ')'
    { $$=^$(new_exp(^raw_exp.Sizeof($3.3),LOC(@1,@4))); }
| SIZEOF unary_expression
    { $$=^$(prim1_exp(^primop.Size,$2,LOC(@1,@2))); }
/* Cyc: throw, size, printf, fprintf, sprintf */
| THROW unary_expression
    { $$=^$(new_exp(^raw_exp.Throw($2),LOC(@1,@2))); }
| SIZE unary_expression
    { $$ = ^$(prim1_exp(^primop.Size,$2,LOC(@1,@2))); }
| format_primop '(' argument_expression_list ')'
    { $$=^$(new_exp(^raw_exp.Primop(^($1,$3)),LOC(@1,@4))); }
;

format_primop:
  PRINTF { $$=^$(^primop.Printf); }
| FPRINTF { $$=^$(^primop.Fprintf); }
| XPRINTF { $$=^$(^primop.Xprintf); }
;

unary_operator:
  '~'
    { $$=^$(^primop.Bitnot); }
| '!'
    { $$=^$(^primop.Not); }
;

postfix_expression:
  primary_expression
    { $$= $!1; }
| postfix_expression '[' expression ']'
    { $$=^$(subscript_exp($1,$3,LOC(@1,@4))); }
| postfix_expression '(' ')'
    { $$=^$(unknowncall_exp($1,null,LOC(@1,@3))); }
| postfix_expression '(' argument_expression_list ')'
    { $$=^$(unknowncall_exp($1,$3,LOC(@1,@4))); }
| postfix_expression '.' IDENTIFIER
    { $$=^$(structmember_exp($1,$3,LOC(@1,@3))); }
// Hack to allow typedef names and field names to overlap
| postfix_expression '.' QUAL_TYPEDEF_NAME
    { qvar q = $3;
      if (q.1 != null)
	err("struct field name is qualified",LOC(@3,@3));
      $$=^$(structmember_exp($1,q.2,LOC(@1,@3)));
    }
| postfix_expression PTR_OP IDENTIFIER
    { $$=^$(structarrow_exp($1,$3,LOC(@1,@3))); }
// Hack to allow typedef names and field names to overlap
| postfix_expression PTR_OP QUAL_TYPEDEF_NAME
    { qvar q = $3;
      if (q.1 != null)
	err("struct field is qualified with module name",LOC(@3,@3));
      $$=^$(structarrow_exp($1,q.2,LOC(@1,@3)));
    }
| postfix_expression INC_OP
    { $$=^$(new_exp(^raw_exp.Increment(^($1,^incrementor.PostInc)),
                    LOC(@1,@2))); 
    }
| postfix_expression DEC_OP
    { $$=^$(new_exp(^raw_exp.Increment(^($1,^incrementor.PostDec)),
                    LOC(@1,@2))); 
    }
| '(' type_name ')' '{' initializer_list '}'
    { $$=^$(new_exp(^raw_exp.CompoundLit(^($2,List::rev($5))),LOC(@1,@6))); }
| '(' type_name ')' '{' initializer_list ',' '}'
    { $$=^$(new_exp(^raw_exp.CompoundLit(^($2,List::rev($5))),LOC(@1,@7))); }
/* Cyc: expressions to build arrays */
| NEW '{' '}'
  /* empty arrays */
    { $$=^$(new_exp(^raw_exp.Array(null),LOC(@1,@3))); }
  /* constant-sized arrays */
| NEW '{' initializer_list '}'
    { $$=^$(new_exp(^raw_exp.Array(List::imp_rev($3)),LOC(@1,@3))); }
| NEW '{' initializer_list ',' '}'
    { $$=^$(new_exp(^raw_exp.Array(List::imp_rev($3)),LOC(@1,@4))); }
  /* array comprehension */
| NEW '{' FOR IDENTIFIER '<' expression ':' expression '}'
    { $$=^$(new_exp(^raw_exp.Comprehension(^($4,$6,$8)),LOC(@1,@9))); }
/* Cyc: added fill and codegen */
| FILL '(' expression ')'
    { $$=^$(new_exp(^raw_exp.Fill($3),LOC(@1,@4))); }
| CODEGEN '(' function_definition ')'
    { $$=^$(new_exp(^raw_exp.Codegen($3),LOC(@1,@4))); }
;

primary_expression:
  qual_opt_identifier
    /* This could be an ordinary identifier, a struct tag, or an enum or
       xenum constructor */
    { $$=^$(new_exp(^raw_exp.UnknownId($1),LOC(@1,@1))); }
| constant
    { $$= $!1; }
| STRING
    { $$=^$(new_exp(^raw_exp.Const(^cnst.String($1)),LOC(@1,@1))); }
| '(' expression ')'
    { $$= $!2; }
/* Cyc: stop instantiation */
| qual_opt_identifier LEFT_RIGHT
    { $$=^$(new_exp(^raw_exp.NoInstantiate(new_exp(^raw_exp.UnknownId($1),
                                                   LOC(@1,@1))),
                    LOC(@1,@2))); }
/* Cyc: tuple expressions */
| '$' '(' argument_expression_list ')'
    { $$=^$(new_exp(^raw_exp.Tuple($3),LOC(@1,@4))); }
/* Cyc: structure expressions */
| qual_opt_identifier '{' initializer_list '}'
    { $$=^$(new_exp(^raw_exp.Struct(^($1,null,List::rev($3),null)),
                    LOC(@1,@4))); }
/* Cyc: compound statement expressions, as in gcc */
| '(' '{' block_item_list '}' ')'
    { $$=^$(new_exp(^raw_exp.StmtExp($3),LOC(@1,@5))); }
;

argument_expression_list:
  argument_expression_list0
    { $$=^$(List::imp_rev($1)); }
;

/* NB: returns list in reverse order */
argument_expression_list0:
  assignment_expression
    { $$=^$(^list($1,null)); }
| argument_expression_list0 ',' assignment_expression
    { $$=^$(^list($3,$1)); }
;

/* NB: We've had to move enumeration constants into primary_expression
   because the lexer can't tell them from ordinary identifiers */
constant:
  INTEGER_CONSTANT
    { $$=^$(int_exp($1,LOC(@1,@1))); }
| CHARACTER_CONSTANT
    { $$=^$(new_exp(^raw_exp.Const(^cnst.Char($1)),LOC(@1,@1))); }
| FLOATING_CONSTANT
    { $$=^$(new_exp(^raw_exp.Const(^cnst.Float($1)),LOC(@1,@1))); }
/* Cyc: boolean constants and null */
| TRUE
    { $$=^$(new_exp(^raw_exp.Const(^cnst.Bool(true)),LOC(@1,@1))); }
| FALSE
    { $$=^$(new_exp(^raw_exp.Const(^cnst.Bool(false)),LOC(@1,@1))); }
| NULL
    { $$=^$(new_exp(^raw_exp.Const(^cnst.Null),LOC(@1,@1))); }
;

qual_opt_identifier:
  IDENTIFIER      { $$=^$(^(null,$1)); }
| QUAL_IDENTIFIER { $$=$!1; }
;

%%
/**************** Definitions and Helper Functions ****************/
prefix Parse;

union struct_or_union {
  void Struct, Union;
}

union blockitem {
  <decl>list TopDecls;
  stmt           Stmt;
  fndecl         FnDecl;
}

union type_specifier {
  seg         Signed;
  seg         Unsigned;
  *(seg,bool) Short;  /* true -> boxed */
  *(seg,bool) Long;   /* true -> boxed */
  *(typ,seg)  Type;   /* int, bool, `a, list<`a>, etc. */
  decl        Decl;
}

union storage_class {
  void Typedef, Extern, Static, Auto, Register, Abstract;
}

struct decl_spec {
  <storage_class>Opt   storage_class;
  tqual                tqual;
  <type_specifier>list type_specs;
  bool                 is_inline;
}

struct declarator {
  qvar                id;
  <type_modifier>list tms;
}

struct abstractdeclarator {
  <type_modifier>list tms;
}

/* Error functions */
static void err(string msg, seg sg) {
  Gcdfec::post_error(Gcdfec::mk_err_parse(sg,msg));
}
static a abort<a>(string msg,seg sg) {
  err(msg,sg);
  raise Gcdfec::Exit();
}
static a unimp<a>(string msg,seg sg) {
  return abort(sprintf("%s unimplemented",msg),sg);
}
static void unimp2(string msg,seg sg) {
  printf("%s: Warning: Cyclone does not yet support %s\n",
         Gcdfec::string_of_seg(sg),
         msg);
  return;
}

static void warn(string msg,seg sg) {
  printf("%s: Warning: %s\n",
         Gcdfec::string_of_seg(sg),
         msg);
  return;
}

*(field_name,typ) drop_tqual(*(field_name,tqual,typ) f) {
  return ^(f.1,f.3);
}

*(var,tqual,typ)
make_struct_field(seg loc,*(qvar,tqual,typ,<tvar>list) field) {
  if (field.4 != null)
    err("bad type params in struct field",loc);
  _ qid = field.1;
  if (qid.1 != null)
    err("struct field cannot be qualified with a module name",loc);
  return ^(qid.2,field.2,field.3);
}

*(<var>Opt,tqual,typ)
  make_param(seg loc,*(<qvar>Opt,tqual,typ,<tvar>list) field) {

  _ idopt;
  if (field.1 != null) {
    idopt = ^Opt(field.1.v.2);
    if (field.1.v.1 != null)
      err("parameter cannot be qualified with a module name",loc);
  } else {
    idopt = null;
  }
  if (field.4 != null)
    abort("parameter should have no type parameters",loc);
  return ^(idopt,field.2,field.3);
}

/* Functions for creating abstract syntax */
static type_specifier type_spec(typ t,seg loc) {
  return ^type_specifier.Type(^(t,loc));
}

/* Functions for converting strange C-like declarative constructs into
 * abstract syntax.  */

/* given an optional variable, tqual, type, and list of type
 * variables, return the tqual and type and check that the type
 * variables are null -- used when we have a tuple type specification.  */
static *(tqual,typ) get_tqual_typ(seg loc,*(<var>Opt,tqual,typ) t) {
  return ^(t.2,t.3);
}

/* Given a variable option, tqual, type, and type var list, make sure
 * the variable is present and the type var list is empty and return
 * the variable, tqual and type.  Used in producing function
 * arguments. */
static *(var,tqual,typ)
make_fn_args(seg loc,*(<qvar>Opt,tqual,typ) t) {
  if (t.1 == null)
    return abort("missing variable in function prototype",loc);
  if (t.1.v.1 != null)
    err("function arguments cannot be qualified with a module name",loc);
  return ^(t.1.v.2,t.2,t.3);
}

// For sanity-checking of old-style parameter declarations
bool exists_param(string x, <string>list params) {
  while (params!=null) {
    if (strcmp(x,params.hd)==0) return true;
    params=params.tl;
  }
  return false;
}
void only_vardecl(<string>list params,decl x) {
  switch (x.raw_decl) {
  case VarDecl(vd):
    if (vd.initializer!=null)
      abort("initializers are not allowed in parameter declarations",x.loc);
    if (vd.name.1 != null)
      err("module names not allowed on parameter declarations",x.loc);
    if (!exists_param(vd.name.2,params))
      abort(sprintf("%s is not listed as a parameter",vd.name.2),x.loc);
  case LetDecl(_):
    abort("let declaration appears in parameter type",x.loc);
  case FnDecl(_):
    abort("function declaration appears in parameter type",x.loc);
  case StructDecl(_):
    abort("struct declaration appears in parameter type",x.loc);
  case UnionDecl:
    abort("union declaration appears in parameter type",x.loc);
  case EnumDecl(_):
    abort("enum declaration appears in parameter type",x.loc);
  case TypedefDecl(_):
    abort("typedef appears in parameter type",x.loc);
  case XenumDecl(_):
    abort("xenum declaration appears in parameter type",x.loc);
  case NamespaceDecl(_):
    abort("namespace declaration appears in parameter type",x.loc);
  case UsingDecl(_):
    abort("using declaration appears in parameter type",x.loc);
  }
  return;
}

// For old-style function definitions,
// get a parameter type from a list of declarations
*(<var>Opt,tqual,typ) get_param_type(*(<decl>list,seg) env, string x) {
  _ tdl = env.1;
  _ loc = env.2;
  if (tdl==null)
    return(abort(sprintf("missing type for parameter %s",x),loc));
  switch (tdl.hd.raw_decl) {
  case VarDecl(vd):
    if (vd.name.1 != null)
      err("module name not allowed on parameter",loc);
    if (strcmp(vd.name.2,x)==0)
      return ^(^Opt(vd.name.2),vd.tqual,vd.typ);
    else return get_param_type(^(tdl.tl,loc),x);
  default:
    // This should never happen, because we use only_vardecl first
    return(abort("non-variable declaration",tdl.hd.loc));
  }
}

bool is_typeparam(type_modifier tm) {
  switch (tm) {
  case TypeParams(_):
    return true;
  default:
    return false;
  }
}

// Convert an old-style function into a new-style function
<type_modifier>list
oldstyle2newstyle(<type_modifier>list tms, <decl>list tds, seg loc) {

  // Not an old-style function
  if (tds==null) return tms;

  // If no function is found, or the function is not innermost, then
  // this is not a function definition; it is an error.  But, we
  // return silently.  The error will be caught by make_function,
  // below.
  if (tms==null) return null;

  switch (tms.hd) {
    case Function(args): {
      // Is this the innermost function??
      if (tms.tl==null ||
          (is_typeparam(tms.tl.hd) && tms.tl.tl==null)) {
        // Yes
        switch (args) {
        case WithTypes(_):
          warn("function declaration with both new- and old-style parameter declarations; ignoring old-style",loc);
          return tms;
        case NoTypes(ids):
          List::iter_c(only_vardecl,ids,tds);
          _ env = ^(tds,loc);
          return
            ^list(^type_modifier.Function
            (^funcparams.WithTypes(^(List::map_c(get_param_type,env,ids),
                                     false))),
            null);
        }
      } else
        // No, keep looking for the innermost function
        return ^list(tms.hd,oldstyle2newstyle(tms.tl,tds,loc));
    }
    default:
      return ^list(tms.hd,oldstyle2newstyle(tms.tl,tds,loc));
  }
}

/* make a top-level function declaration out of a
 * declaration-specifier (return type, etc.),
 * a declarator (the function name and args),
 * a declaration list (for old-style function definitions),
 * and a statement */
static fndecl
make_function(<decl_spec>Opt dso, declarator d,
              <decl>list tds, stmt body, seg loc) {
  // Handle old-style parameter declarations
  if (tds!=null) {
    d = ^declarator(d.id,oldstyle2newstyle(d.tms,tds,loc));
  }

  scope scope = ^scope.Public;
  <type_specifier>list tss = null;
  tqual tq = empty_tqual();
  bool is_inline = false;

  if (dso != null) {
    tss = dso.v.type_specs;
    tq = dso.v.tqual;
    is_inline = dso.v.is_inline;
    /* Examine storage class; like C, we allow both static and extern */
    if (dso.v.storage_class != null)
      switch (dso.v.storage_class.v) {
      case Extern: scope = ^scope.Extern;
      case Static: scope = ^scope.Static;
      default: err("bad storage class on function",loc);
      }
  }
  _ ts_info = collapse_type_specifiers(tss,loc);
  if (ts_info.2 != null)
    warn("nested type declaration within function prototype",loc);
  typ t = ts_info.1;
  *(tqual,typ,<tvar>list) info = apply_tms(tq,t,d.tms);
  if (info.3 != null)
    // Example:   `a f<`b><`a>(`a x) {...}
    // Here info.3 will be the list `b.
    warn("bad type params, ignoring",loc);
  _ fn_name = d.id;
  _ fn_tqual = info.1;
  _ fn_type = info.2;
  /* fn_type had better be a FnType */
  switch (fn_type) {
    case FnType*(tvs,ret_type,args,varargs):
      _ args = List::map_c(fnargspec_to_arg,loc,args);
      return ^fndecl {scope=scope,name=fn_name,tvs=tvs,
                      is_inline=is_inline,ret_type=ret_type,
                      args=args,varargs=varargs,body=body};
    default:
      return (abort("declarator is not a function prototype",loc));
  }
}

static *(var,tqual,typ)
fnargspec_to_arg(seg loc,*(<var>Opt,tqual,typ) t) {
  if (t.1 == null) {
    err("missing argument variable in function prototype",loc);
    return ^("?",t.2,t.3);
  } else
    return ^(t.1.v,t.2,t.3);
}

/* given a numeric type convert the sign to s -- destructive */
static typ change_signed(bool s,typ t,seg seg) {
  switch (t) {
    case IntType(p):
      p.1 = (s ? ^sign.Signed : ^sign.Unsigned);
    default: abort(sprintf("parse:make_sign_s:  typ is %s",
			   Synpp::typ2string(t)),
		   seg);
  }
  return t;
}

/* given a numeric type, sz (B[1|2|4|8]), and a boxed-ness, check
 * that the type is also a boxed integral type and change the size to
 * match sz -- destructive. */
static typ change_size(size_of sz, bool is_boxed, typ t,seg seg) {
  switch (t) {
  case IntType(p):
    if (p.3 == ^boxed.Boxed && !is_boxed)
      abort("unboxed size qualifier on boxed integral type",seg);
    else if (p.3 == ^boxed.Unboxed && is_boxed)
      abort("boxed size qualifier on unboxed integral type",seg);
    else
      p.2 = sz;
  default:
    abort("size qualifier on non-integral type",seg);
  }
  return t;
}

/* create a new (boxed) unification variable */
static typ new_box_evar(tvar v) {
  return new_evar(^kind.BoxKind);
}

/* Given a type-specifier list, determines the type and any declared
 * structs, unions, or enums.  Most of this is just collapsing
 * combinations of [un]signed, short, long, int, char, etc.  We're
 * probably more permissive than is strictly legal here.  For
 * instance, one can write "unsigned const int" instead of "const
 * unsigned int" and so forth.  I don't think anyone will care...
 * (famous last words.)  */
static *(typ,<decl>Opt)
collapse_type_specifiers (<type_specifier>list ts, seg loc) {

  /* flags that record what we've seen in the qualifiers */
  <bool>Opt signopt = null;      /* true->signed, false->unsigned */
  <typ>Opt topt = null;        /* the underlying type (if any) */
  <decl>Opt declopt = null;      /* any hidden declarations */
  bool is_short = false;         /* short or Short */
  bool is_long = false;          /* long or Long */
  bool is_longlong = false;      /* long long or Long Long */
  bool is_boxed = false;         /* e.g., is_short -> Short, etc. */

  typ the_type;                  /* the final type */

  seg last_loc = loc;
  string msg1 = "at most one type may appear within a type specifier";
  string msg2 =
    "const or volatile may only appear once within a type specifier";
  string msg3 = "type specifier includes more than one declaration";
  string msg4 = "sign specifier may only appear once within a type specifier";

  while (ts != null) {
    switch (ts.hd) {
    case Signed(loc):
      last_loc = loc;
      if (signopt != null) err(msg4,loc);
      if (topt != null) err("signed qualifier must come before type",loc);
      signopt = ^Opt(true);
    case Unsigned(loc):
      last_loc = loc;
      if (signopt != null) err(msg4,loc);
      if (topt != null) err("signed qualifier must come before type",loc);
      signopt = ^Opt(false);
    case Short*(loc,box):
      last_loc = loc;
      if (is_short || is_long || is_longlong)
        err("integral size may only appear once within a type specifier",loc);
      else if (topt != null)
        err("short modifier must come before base type",loc);
      else is_short = true;
      is_boxed = box;
    case Long*(loc,box):
      last_loc = loc;
      if (is_short || is_longlong)
        err("extra long in type specifier",loc);
      else if (topt != null)
        err("long modifier must come before base type",loc);
      if (is_long) {
        /* if we've already seen long once, then we have long long (B8) */
        if (box != is_boxed)
          err("must use long long or LONG LONG",loc);
        is_longlong = true;
        is_long = false;
      } else {
        /* otherwise, it's just a long (B4) */
        is_long = true;
        is_boxed = box;
      }
    case Type*(t,loc):
      last_loc = loc;
      if (topt == null) topt = ^Opt(t); else err(msg1,loc);
    case Decl(d):
      // we've got a struct or enum declaration embedded in here -- return
      // the declaration as well as a copy of the type -- this allows
      // us to declare structs as a side effect inside typedefs
      last_loc = d.loc;
      if (declopt == null && topt == null) {
        switch (d.raw_decl) {
        case StructDecl(sd):
          _ args = List::map(tvar2typ,sd.tvs);
          topt = ^Opt(^typ.StructType(^(sd.name,args)));
          if (sd.fields!=null) declopt = ^Opt(d);
        case EnumDecl(ed):
          _ args = List::map(tvar2typ,ed.tvs);
          topt = ^Opt(^typ.EnumType(^(ed.name,args)));
          if (ed.fields!=null) declopt = ^Opt(d);
        case XenumDecl(xed):
          topt = ^Opt(^typ.XenumType(xed.name));
          if (xed.fields != null) declopt = ^Opt(d);
        case UnionDecl:
          // FIX: TEMPORARY SO WE CAN TEST THE PARSER
          topt = ^Opt(^typ.UnionType);
        default:
          abort("bad declaration within type specifier",d.loc);
        }
      } else err(msg3,d.loc);
    }
    ts = ts.tl;
  }
  /* sanity check */
  if ((is_short && (is_long || is_longlong)) || (is_long && is_longlong))
    abort("parse.y:collapse_type_specifiers:sanity check failed!",last_loc);
  /* it's okay to not have an explicit type as long as we have some
   * combination of signed, unsigned, short, long, or longlong */
  if (topt == null) {
    sign s = (signopt == null || signopt.v) ? ^sign.Signed : ^sign.Unsigned;
    boxed box = (is_boxed ? ^boxed.Boxed : ^boxed.Unboxed);
    if (is_short) {
      the_type = ^typ.IntType(^(s, ^size_of.B2, box));
    } else if (is_long) {
      the_type = ^typ.IntType(^(s, ^size_of.B4, box));
    } else if (is_longlong) {
      the_type = ^typ.IntType(^(s, ^size_of.B8, box));
    } else if (signopt != null) {
      the_type = ^typ.IntType(^(s, ^size_of.B4, box));
    } else {
      the_type = abort("missing type within specifier",last_loc);
    };
  } else {
    the_type = topt.v;
    if (signopt != null)
      the_type = change_signed(signopt.v,the_type,last_loc);
    if (is_short)
      the_type = change_size(^size_of.B2,is_boxed,the_type,last_loc);
    else if (is_long)
      the_type = change_size(^size_of.B4,is_boxed,the_type,last_loc);
    else if (is_longlong)
      the_type = change_size(^size_of.B8,is_boxed,the_type,last_loc);
  }
  return ^(the_type, declopt);
}

<*(qvar,tqual,typ,<tvar>list)>list
apply_tmss(tqual tq,typ t,<declarator>list ds) {
  if (ds==null) return null;
  _ d = ds.hd;
  _ q = d.id;
  *(tqual,typ,<tvar>list) p = apply_tms(tq,t,d.tms);
  return
    ^list(^(q,p.1,p.2,p.3),
          apply_tmss(tq,t,ds.tl));
}

*(tqual,typ,<tvar>list)
apply_tms(tqual tq,typ t,<type_modifier>list tms)
{
  if (tms==null) return ^(tq,t,null);
  switch (tms.hd) {
    case Carray: {
      return
        apply_tms(empty_tqual(),
                  ^typ.ArrayType(^(t,tq,^array_kind.UntaggedArray)),
                  tms.tl);
    }
    case Array: {
      return
        apply_tms(empty_tqual(),
                  ^typ.ArrayType(^(t,tq,^array_kind.TaggedArray)),
                  tms.tl);
    }
    case ConstArray(e): {
      return
        apply_tms(empty_tqual(),
                  ^typ.ArrayType(^(t,tq,^array_kind.FixedArray(e))),
                  tms.tl);
    }
    case Function(args): {
      switch (args) {
      case WithTypes(args):
        <tvar>list typvars = null;
        // functions consume type parameters
        if (tms.tl != null) {
          switch (tms.tl.hd) {
          case TypeParams(ts):
            typvars = ts;
            tms=tms.tl; // skip TypeParams on call of apply_tms below
          default:
            // nothing
            ;
          }
        }
        // special case where the parameters are void, e.g., int f(void)
        if (!args.2                // not vararg function
            && args.1 != null      // not empty arg list
            && args.1.tl == null   // not >1 arg
            && args.1.hd.1 == null // not f(void x)
            && args.1.hd.3 == ^typ.VoidType)
          args = ^(null,false);
        // Note, we throw away the tqual argument.  An example where
        // this comes up is "const int f(char c)"; it doesn't really
        // make sense to think of the function as returning a const
        // (or volatile, or restrict).  The result will be copied
        // anyway.  TODO: maybe we should issue a warning.  But right
        // now we don't have a loc so the warning will be confusing.
        return
          apply_tms(empty_tqual(),
                    function_t(typvars,t,args.1,args.2),
                    tms.tl);
      case NoTypes(_):
        // TODO: this is a confusing warning because we don't have a loc
        return(abort("function declaration without parameter types",
                     DUMMYLOC));
      }
    }
    case TypeParams(ts): {
      // If we are the last type modifier, this could be the list of
      // type parameters to a typedef:
      // typedef struct foo<`a,int> foo_t<`a>
      if (tms.tl==null)
        return ^(tq,t,ts);
      // Otherwise, it is an error in the program if we get here;
      // TypeParams should already have been consumed by an outer
      // Function (see last case).  TODO: this is a confusing warning
      // because we don't have a loc
      return(abort("type parameters must appear before function arguments in declarator",
                   DUMMYLOC));
    }
    case Pointer*(nullable,tq2): {
      return
        apply_tms(tq2,^typ.PointerType(^(t,new_conref(nullable),tq)),tms.tl);
    }
  }
}

/* convert a list of types to a list of typevars -- the parser can't
 * tell lists of types apart from lists of typevars easily so we parse
 * them as types and then convert them back to typevars.  See
 * productions "struct_or_union_specifier" and "enum_specifier"; */
static var typ2tvar(seg loc, typ t) {
  switch (t) {
  case VarType(x): return x;
  default: return abort("expecting a list of type variables, not types",loc);
  }
}

static typ tvar2typ(var v) {
  return ^typ.VarType(v);
}

/* given a local declaration and a statement produce a decl statement */
static stmt flatten_decl(decl d,stmt s) {
  return new_stmt(^raw_stmt.Decl(^(d,s)),seg_join(d.loc,s.loc));
}
/* given a list of local declarations and a statement, produce a big
   decl statement. */
static stmt flatten_declarations(<decl>list ds, stmt s){
  return List::fold_right(flatten_decl,ds,s);
}

/* Given a declaration specifier list (a combination of storage class
   [typedef, extern, static, etc.] and type specifiers (signed, int,
   `a, const, etc.), and a list of declarators and initializers,
   produce a list of top-level declarations.  By far, this is the most
   involved function and thus I expect a number of subtle errors. */
static <decl>list
make_declarations(<decl_spec>Opt dso,<*(declarator,<exp>Opt)>list ids,
                  seg loc) {
  <type_specifier>list tss = null;
  <scope>Opt scopeopt = null; /* Extern, Static, Auto, Register, Abstract */
  bool istypedef = false;     /* Typedef */
  tqual tq = empty_tqual();

  if (dso != null) {
    tss = dso.v.type_specs;
    tq = dso.v.tqual;

    if (dso.v.is_inline)
      err("inline is only allowed on function definitions",loc);

    /* figure out the scope and what kind of declaration applies
       to each declarator. */
    if (dso.v.storage_class != null)
      switch (dso.v.storage_class.v) {
      case Typedef:
        if (istypedef)
          err("extra typedef keyword in declaration",loc);
        istypedef = true;
      case Extern:
        if (scopeopt != null) err("extra extern keyword in declaration",loc);
        scopeopt = ^Opt(^scope.Extern);
      case Static:
        if (scopeopt != null) err("extra static keyword in declaration",loc);
        scopeopt = ^Opt(^scope.Static);
      case Auto:
        if (scopeopt != null) err("extra auto keyword in declaration",loc);
        scopeopt = ^Opt(^scope.Public);
      case Register:
        if (scopeopt != null) err("extra register keyword in declaration",loc);
        scopeopt = ^Opt(^scope.Public);
      case Abstract:
        if (scopeopt != null) err("extra abstract keyword in declaration",loc);
        scopeopt = ^Opt(^scope.Abstract);
      }
  }
  scope s = (scopeopt == null ? ^scope.Public : scopeopt.v);

  /* separate the declarators from their initializers */
  _ pair = List::split(ids);
  <declarator>list declarators = pair.1;
  <<exp>Opt>list exprs = pair.2;
  /* check to see if there are no initializers -- useful later on */
  bool exps_empty = true;
  for (<<exp>Opt>list es = exprs; es != null; es = es.tl)
    if (es.hd != null) {
      exps_empty = false;
      break;
    }

  if (tss == null) {
    err("missing type specifiers in declaration",loc);
    return null;
  }
  /* Collapse the type specifiers to get the base type and any
   * optional nested declarations */
  _ ts_info = collapse_type_specifiers(tss,loc);
  if (declarators == null) {
    /* here we have a type declaration -- either a struct, union,
       enum, or xenum as in: "struct Foo { ... };" */
    typ t = ts_info.1;
    <decl>Opt declopt = ts_info.2;
    if (declopt != null) {
      decl d = declopt.v;
      switch (d.raw_decl) {
      case StructDecl(sd): sd.scope = s;
      case EnumDecl(ed)  : ed.scope = s;
      case XenumDecl(ed) : ed.scope = s;
      default: err("bad declaration",loc); return null;
      }
      return ^list(d,null);
    } else {
      switch (t) {
      case StructType*(n,ts):
        _ ts = List::map_c(typ2tvar,loc,ts);
        _ sd = ^structdecl{scope = s, name = n, tvs = ts, fields = null};
        return ^list(^decl(^raw_decl.StructDecl(sd),loc),null);
      case EnumType*(n,ts):
        _ ts = List::map_c(typ2tvar,loc,ts);
        _ ed = ^enumdecl{scope = s, name = n, tvs = ts, fields = null};
        return ^list(^decl(^raw_decl.EnumDecl(ed),loc),null);
      case XenumType(n):
        _ ed = ^xenumdecl{scope=s,
                          name=n,
                          fields=null};
        return ^list(^decl(^raw_decl.XenumDecl(ed),loc),null);
      case UnionType:
        /* FIX: TEMPORARY SO WE CAN TEST THE PARSER */
        return ^list(^decl(^raw_decl.UnionDecl,loc),null);
      default: err("missing declarator",loc); return null;
      }
    }
  } else {
    /* declarators != null */
    typ t = ts_info.1;
    <*(qvar,tqual,typ,<tvar>list)>list
       fields = apply_tmss(tq,t,declarators);
    if (istypedef) {
      /* we can have a nested struct, union, enum, or xenum
         declaration within the typedef as in:
         typedef struct Foo {...} t; */
      if (!exps_empty) err("initializer in typedef declaration",loc);
      <decl>list decls =
         List::map_c(v_typ_to_typedef,loc,fields);
      if (ts_info.2 != null) {
        decl d = ts_info.2.v;
        switch (d.raw_decl) {
        case StructDecl(sd): sd.scope = s;
        case EnumDecl(ed)  : ed.scope = s;
        case XenumDecl(ed) : ed.scope = s;
        default:
          err("declaration within typedef is not a struct, enum, or xenum",
              loc);
        }
        decls = ^list(d,decls);
      }
      return decls;
    } else {
      /* here, we have a bunch of variable declarations */
      if (ts_info.2 != null)
        unimp2("nested type declaration within declarator",loc);
      <decl>list decls = null;
      <*(qvar,tqual,typ,<tvar>list)>list ds;
      for (ds = fields; ds != null; ds = ds.tl) {
        _ q = ds.hd;
        if (q.4 != null)
          warn("bad type params, ignoring",loc);
        _ x = q.1;
        _ tq = q.2;
        _ t = q.3;
        if (exprs == null)
          abort("unexpected null in parse!",loc);
        _ eopt = exprs.hd;
        exprs = exprs.tl;
        _ vd = ^vardecl {scope = s, name = x, tqual = tq, typ = t,
                         initializer = eopt};
        _ d = ^decl(^raw_decl.VarDecl(vd),loc);
        decls = ^list(d,decls);
      }
      return (List::imp_rev(decls));
    }
  }
}

/* convert an (optional) variable, tqual, type, and type
   parameters to a typedef declaration.  As a side effect, register
   the typedef with the lexer.  */
/* TJ: the tqual should make it into the typedef as well,
   e.g., typedef const int CI; */
static decl
v_typ_to_typedef(seg loc, *(qvar,tqual,typ,<tvar>list) t) {

  qvar x = t.1;
  /* tell the lexer that x is a typedef identifier */
  Lex::register_typedef(x);
  _ td = ^typedefdecl {name = x, tvs = t.4, defn = t.3};
  return new_decl(^raw_decl.TypedefDecl(td),loc);
}

<<<FILE>function_lexbuf_state>lexbuf>Opt lbuf = null;
exception ParseError;

static <decl>list parse_result = null;

<decl>list parse_file(FILE f) {
  parse_result = null;
  yylloc = ^yyltype(0,0,0,0,0,"");
  lbuf = ^Opt(from_file(f));
  yyparse();
  return parse_result;
}
