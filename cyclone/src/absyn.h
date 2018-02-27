#ifndef ABSYN_H
#define ABSYN_H

#include "core.h"
#include "list.h"
#include "gcdfec.h"

prefix Absyn {
open Absyn {
open List;
open Core;

#define var          string
#define qvar         *(<var>list,var)
#define tvar         string
#define typedef_name qvar
#define field_name   string
#define types        <typ>list
#define opt_types    <<typ>list>Opt
#define exps         <exp>list
#define DUMMYLOC     Gcdfec::dummy_seg

extern union scope { void Static, Public, Extern, Abstract; }
extern struct tqual {
  bool q_const;
  bool q_volatile;
  bool q_restrict;
}
extern union size_of    { void B1, B2, B4, B8; }
extern union kind       { void BoxKind, RegKind, MemKind; }
extern union array_kind { void UntaggedArray, TaggedArray; exp FixedArray; }
extern union sign       { void Signed, Unsigned; }
extern union boxed      { void Boxed, Unboxed; }

// Conrefs are used to specify equality constraints.  When unifying,
// we can equate two conrefs (through forwarding) or constrain a conref
// to be equal to a particular value.  For instance, whether a pointer
// type is nullable or not (* vs @) is specified as a <bool>conref.
// When we see a "&" pattern, we generate a PointerType with an unconstrained
// <bool>conref.  During unification, the conref is constrained by the
// type of the expression we are matching on.  We could probably use
// conref's elsewhere (e.g., the boxed qualifier on Int, Bool, Float,
// and Double types, the sign and size_of for the Int type, etc.)  We
// could also treat Evar's as <typ>conrefs but the constraints on Evars
// aren't just equality (i.e., we can constrain a MemKind Evar to be equal
// to a BoxKind Evar...)
extern struct <a>conref { <a>constraint v; }
extern union <a>constraint {
  a         Eq;
  <a>conref Forward;
  void      None;
}
extern <a>conref new_conref<a>(a x);
extern <a>conref empty_conref<a>();

extern union typ {
  // BoxKind types
  void                       VoidType;
  *(kind,<typ>Opt,int)       Evar;         // int is unique id
  tvar                       VarType;      // `a
  *(<typedef_name>Opt,types) EnumType;     // enum Foo<t1,...,tn>
  typedef_name               XenumType;    // xenum Foo
  *(typ,<bool>conref,tqual)  PointerType;  // typ:MemKind, true -> nullable
  // RegKind types unless boxed
  *(sign, size_of, boxed)    IntType;      // (B4:BoxKind), rest are RegKind
  boxed                      BoolType;     // Reg
  boxed                      FloatType;    // Reg
  boxed                      DoubleType;   // Reg
  // MemKind types
  *(typ,tqual,array_kind)    ArrayType;    // typ:Mem
  *(<tvar>list,typ,<*(<var>Opt,tqual,typ)>list,bool) FnType; // bool indicates varargs
  <*(tqual,typ)>list         TupleType;    // typs:Mem
  *(<typedef_name>Opt,types) StructType;   // Mem
  // Initially, the <typ>Opt is null.  the elaborator fills it in with the
  // typedef definition, substituting the parameters.  always call compress
  // to avoid this level of indirection before comparing types.  
  *(typedef_name,types,<typ>Opt) TypedefType;  // Mem
  void                       UnionType;    // temporary so we can test parser
}

// funcparams and type_modifier are useful alternate
// forms for parsing and printing typ's
extern union funcparams {
  <string>list                        NoTypes;
  *(<*(<var>Opt,tqual,typ)>list,bool) WithTypes;
}
extern union type_modifier {
  void          Carray;
  void          Array;
  exp           ConstArray;
  *(bool,tqual) Pointer;
  funcparams    Function;
  <tvar>list    TypeParams;
}

extern union cnst {
  char        Char; // Fix: needs sign
  *(sign,int) Int;
  short       Short; // Fix: needs sign
//  long long Long;
  string      Float; // Left in string form for now
  void        Null;
  bool        Bool;
  string      String;
}

extern union primop {
  void Plus, Times, Minus, Div, Mod,
    Eq, Neq, Gt, Lt, Gte, Lte, Not,
    Bitnot, Bitand, Bitor, Bitxor, Bitlshift, Bitlrshift, Bitarshift,
    Size, Printf, Fprintf, Xprintf; 
}

extern union incrementor {
  void PreInc, PostInc, PreDec, PostDec;
}

extern union raw_exp {
  cnst                   Const;
  qvar                   Var;
  qvar                   UnknownId;
  *(primop, exps)        Primop;
  *(exp,<primop>Opt,exp) AssignOp;     // e.g., x += e
  *(exp,incrementor)     Increment;    // e.g., e++, e--, --e, ++e
  *(exp,exp,exp)         Conditional;
  *(exp,exp)             SeqExp;
  *(exp,exps)            UnknownCall;  // fun-call, struct, or enum expression
  *(exp,exps)            FnCall;
  exp                    Throw;
  // expressions of polymorphic type are implicitly instantiated by the
  // elaborator unless explicitly blocked by NoInstantiate
  exp                    NoInstantiate;
  *(exp,opt_types)       Instantiate;
  *(typ,exp)             Cast;
  exp                    Address;      // &e
  typ                    Sizeof;
  // various elim forms -- also left-hand-sides for assignments
  exp                    Deref;        // *e
  *(exp, field_name)     StructMember; // e.f
  *(exp, field_name)     StructArrow; // e->f
  *(exp, exp)            Subscript;
  // raw, unboxed memory values
  exps                   Tuple;
  *(*(<string>Opt,tqual,typ),
    <*(<designator>list,exp)>list) CompoundLit;
  <*(<designator>list,exp)>list Array;
  *(var,exp,exp)         Comprehension;
  *(typedef_name,opt_types,<*(<designator>list,exp)>list,<structdecl>Opt) Struct;
  *(qvar, opt_types, opt_types, <exp>list, enumdecl, enumfield) Enum;
  *(qvar,opt_types,<exp>list, xenumdecl,enumfield) Xenum;
  *(<typedef_name>Opt,<*(<designator>list,exp)>list) UnresolvedMem;
  stmt                   StmtExp;
  fndecl                 Codegen;
  exp                    Fill;
}
extern struct exp {
  <typ>Opt    topt;
  raw_exp     raw_exp;
  <var>list   assigned_to; // made during type checking, used in Poptranslate
  Gcdfec::seg loc;
}
extern union raw_stmt {
  void                        Skip;
  exp                         Exp;
  *(stmt,stmt)                Seq;
  <exp>Opt                    Return;
  *(exp,stmt,stmt)            IfThenElse;
  *(exp,stmt)                 While;
  void                        Break;
  void                        Continue;
  var                         Goto;
  *(exp,exp,exp,stmt)         For;
  *(exp,<switch_clause>list)  Switch;
  void                        Fallthru;
  *(decl,stmt)                Decl;
  stmt                        Cut;
  stmt                        Splice;
  *(var,stmt)                 Label;
  *(stmt,exp)                 Do;
  *(stmt,<switch_clause>list) TryCatch;
}
extern union raw_pat {
  void                            Wild;
  void                            Null;
  *(sign,int)                     Int;
  char                            Char;
  string                          Float; // Left in string form for now
  bool                            Bool;
  <pat>list                       Tuple;
  // use to read-through a pattern (eliminates the */@)
  pat                             Pointer;
  // use to create a pointer value (introduces the @)
  var                             Reference;
  // UnknownId, UnknownCall, and UnknownFields produced by parser
  qvar                            UnknownId;
  *(qvar,<tvar>list,<pat>list)    UnknownCall;
  *(qvar,<tvar>list,<*(<designator>list,pat)>list) UnknownFields;
  // Typechecker resolves Unknown* to Var,Struct,Enum,Xenum
  var                             Var;
  *(structdecl,opt_types,<tvar>list,<*(<designator>list,pat)>list) Struct;
  *(qvar,opt_types,<tvar>list,<pat>list,enumdecl,enumfield) Enum;
  *(qvar,<tvar>list,<pat>list,xenumdecl,enumfield) Xenum;
}
extern struct pat {
  raw_pat     raw_pat;
  <typ>Opt    topt;
  Gcdfec::seg loc;
}
extern struct switch_clause {
  pat         pat;
  <exp>Opt    where_clause;
  stmt        body;
  Gcdfec::seg loc;
}
extern struct stmt {
  raw_stmt    raw_stmt;
  Gcdfec::seg loc;
}
extern struct fndecl {
  scope                  scope;
  bool                   is_inline;
  qvar                   name;
  <tvar>list             tvs;
  typ                    ret_type;
  <*(var,tqual,typ)>list args;
  bool                   varargs;
  stmt                   body;
}
extern struct structdecl {
  scope                              scope;
  <typedef_name>Opt                  name;
  <tvar>list                         tvs;
  <<*(field_name,tqual,typ)>list>Opt fields; // when null, abstract
}
extern struct enumfield {
  qvar               name;     // constructor name
  <exp>Opt           tag;      // value of the constructor
  <tvar>list         tvs;      // existentially-bound type variables
  <*(tqual,typ)>list typs;     // types carried by constructor
  Gcdfec::seg        loc;
}
extern struct enumdecl {
  scope                scope;
  <typedef_name>Opt    name;
  <tvar>list           tvs;
  <<enumfield>list>Opt fields; // when null, abstract
}
extern struct xenumdecl {
  // If fields == null this defines a new xenum, else new fields
  scope           scope;
  typedef_name    name;
  <enumfield>list fields;
}
extern struct typedefdecl {
  typedef_name name;
  <var>list    tvs;
  typ          defn;
}
extern struct vardecl {
  scope    scope;
  qvar     name;
  tqual    tqual;
  typ      typ;
  <exp>Opt initializer;
}
extern union raw_decl {
  vardecl                 VarDecl;
  fndecl                  FnDecl;
  *(pat,<typ>Opt,exp)     LetDecl;
  structdecl              StructDecl;
  void                    UnionDecl; // void because unimplemented
  enumdecl                EnumDecl;
  xenumdecl               XenumDecl;
  typedefdecl             TypedefDecl;
  *(var,<decl>list)       NamespaceDecl;
  *(qvar,<decl>list)      UsingDecl;
}
extern struct decl {
  raw_decl raw_decl;
  Gcdfec::seg  loc;
}
extern union designator {
  exp ArrayElement;
  var FieldName;
}

// compare variables 
extern int qvar_cmp(qvar, qvar);
extern int varlist_cmp(<var>list, <var>list);

///////////////////////// Constructors ////////////////////////////
extern tqual combine_tqual(tqual x,tqual y);
extern tqual empty_tqual();

/////////////////////////////// Kinds /////////////////////////////
extern kind box_k, reg_k, mem_k;

////////////////////////////// Types //////////////////////////////
// return a fresh type variable of the given kind 
extern typ new_evar(kind);
extern typ wildtyp();
// unboxed, unsigned types
extern typ uchar_t, ushort_t, uint_t, ulong_t;
// unboxed, signed types
extern typ schar_t, sshort_t, sint_t, slong_t;
// unboxed float, double, bool
extern typ float_t, double_t, bool_t;
// boxed, unsigned types
extern typ uChar_t, uShort_t, uInt_t, uLong_t;
// boxed, signed types
extern typ sChar_t, sShort_t, sInt_t, sLong_t;
// boxed float, double, bool
extern typ Float_t, Double_t, Bool_t;
// exception type
extern typ exn_t;
// void type
extern typ void_t;
// string (char[?])
extern typ string_t();
// FILE
extern typ file_t();
// pointers
extern typ nullableptr_t(typ t, tqual tq);
extern typ void_star_t();
extern typ pureptr_t(typ t, tqual tq);
// structs
extern typ strct(var name);

/////////////////////////////// Expressions ////////////////////////
extern exp new_exp(raw_exp, Gcdfec::seg);
extern exp const_exp(cnst, Gcdfec::seg);
extern exp null_exp(Gcdfec::seg);
extern exp bool_exp(bool, Gcdfec::seg);
extern exp true_exp(Gcdfec::seg);
extern exp false_exp(Gcdfec::seg);
extern exp int_exp(*(sign,int), Gcdfec::seg);
extern exp signed_int_exp(int, Gcdfec::seg);
extern exp uint_exp(unsigned int, Gcdfec::seg);
extern exp char_exp(char c, Gcdfec::seg);
extern exp float_exp(string f, Gcdfec::seg);
extern exp string_exp(string s, Gcdfec::seg);
extern exp var_exp(qvar, Gcdfec::seg);
extern exp unknownid_exp(qvar, Gcdfec::seg);
extern exp primop_exp(primop, <exp>list es, Gcdfec::seg);
extern exp prim1_exp(primop, exp, Gcdfec::seg);
extern exp prim2_exp(primop, exp, exp, Gcdfec::seg);
extern exp times_exp(exp, exp, Gcdfec::seg);
extern exp eq_exp(exp, exp, Gcdfec::seg);
extern exp neq_exp(exp, exp, Gcdfec::seg);
extern exp gt_exp(exp, exp, Gcdfec::seg);
extern exp lt_exp(exp, exp, Gcdfec::seg);
extern exp gte_exp(exp, exp, Gcdfec::seg);
extern exp lte_exp(exp, exp, Gcdfec::seg);
extern exp assignop_exp(exp, <primop>Opt, exp, Gcdfec::seg);
extern exp assign_exp(exp, exp, Gcdfec::seg);
extern exp post_inc_exp(exp, Gcdfec::seg);
extern exp post_dec_exp(exp, Gcdfec::seg);
extern exp pre_inc_exp(exp, Gcdfec::seg);
extern exp pre_dec_exp(exp, Gcdfec::seg);
extern exp conditional_exp(exp, exp, exp, Gcdfec::seg);
extern exp and_exp(exp, exp, Gcdfec::seg); // &&
extern exp or_exp(exp, exp, Gcdfec::seg); // ||
extern exp seq_exp(exp, exp, Gcdfec::seg);
extern exp unknowncall_exp(exp, <exp>list, Gcdfec::seg);
extern exp fncall_exp(exp, <exp>list, Gcdfec::seg);
extern exp throw_exp(exp, Gcdfec::seg);
extern exp noinstantiate_exp(exp, Gcdfec::seg);
extern exp instantiate(exp, <typ>list, Gcdfec::seg);
extern exp cast_exp(typ, exp, Gcdfec::seg);
extern exp address_exp(exp, Gcdfec::seg);
extern exp sizeof_exp(typ t, Gcdfec::seg);
extern exp deref_exp(exp, Gcdfec::seg);
extern exp structmember_exp(exp, field_name, Gcdfec::seg);
extern exp structarrow_exp(exp, field_name, Gcdfec::seg);
extern exp subscript_exp(exp, exp, Gcdfec::seg);
extern exp tuple_exp(<exp>list, Gcdfec::seg);
extern exp stmt_exp(stmt, Gcdfec::seg);
extern exp null_pointer_exn_exp(Gcdfec::seg);
extern exp array_exp(<exp>list, Gcdfec::seg);
extern exp unresolvedmem_exp(<typedef_name>Opt,
                             <*(<designator>list,exp)>list,Gcdfec::seg);
/////////////////////////// Statements ///////////////////////////////
extern stmt new_stmt(raw_stmt s, Gcdfec::seg loc);
extern stmt skip_stmt(Gcdfec::seg loc);
extern stmt exp_stmt(exp e,Gcdfec::seg loc);
extern stmt seq_stmt(stmt s1, stmt s2, Gcdfec::seg loc);
extern stmt seq_stmts(<stmt>list, Gcdfec::seg loc);
extern stmt return_stmt(<exp>Opt e,Gcdfec::seg loc);
extern stmt ifthenelse_stmt(exp e,stmt s1,stmt s2,Gcdfec::seg loc);
extern stmt while_stmt(exp e,stmt s,Gcdfec::seg loc);
extern stmt break_stmt(Gcdfec::seg loc);
extern stmt continue_stmt(Gcdfec::seg loc);
extern stmt for_stmt(exp e1,exp e2,exp e3,stmt s, Gcdfec::seg loc);
extern stmt switch_stmt(exp e, <switch_clause>list, Gcdfec::seg loc);
extern stmt fallthru_stmt(Gcdfec::seg loc);
extern stmt decl_stmt(decl d, stmt s, Gcdfec::seg loc); 
extern stmt declare_stmt(qvar, typ, <exp>Opt init, stmt, Gcdfec::seg loc);
extern stmt cut_stmt(stmt s, Gcdfec::seg loc);
extern stmt splice_stmt(stmt s, Gcdfec::seg loc);
extern stmt label_stmt(var v, stmt s, Gcdfec::seg loc);
extern stmt do_stmt(stmt s, exp e, Gcdfec::seg loc);
extern stmt trycatch_stmt(stmt s, <switch_clause>list scs, Gcdfec::seg loc);
extern stmt goto_stmt(var lab, Gcdfec::seg loc);
extern stmt assign_stmt(exp e1, exp e2, Gcdfec::seg loc);

/////////////////////////// Patterns //////////////////////////////
extern pat new_pat(raw_pat p, Gcdfec::seg s);

////////////////////////// Declarations ///////////////////////////
extern decl new_decl(raw_decl r, Gcdfec::seg loc);
extern decl let_decl(pat p, <typ>Opt t_opt, exp e, Gcdfec::seg loc);
extern vardecl new_vardecl(qvar x, typ t, <exp>Opt init);
extern vardecl static_vardecl(qvar x, typ t, <exp>Opt init);
extern decl struct_decl(scope s,<typedef_name>Opt n,<var>list ts,
			<<*(field_name,tqual,typ)>list>Opt fs,
			Gcdfec::seg loc);
extern decl enum_decl(scope s,<typedef_name>Opt n,<var>list ts,
		      <<enumfield>list>Opt fs,Gcdfec::seg loc);
extern decl xenum_decl(scope s,typedef_name n,<enumfield>list fs,
		       Gcdfec::seg loc);

/* return true if p is printf, sprintf, fprintf */
extern bool is_format_prim(primop p);

extern typ function_t(<tvar>list, typ, <*(<var>Opt,tqual,typ)>list, bool);
extern typ pointer_expand(typ);
//extern typ pointer_abbrev(typ);
extern bool is_lvalue(exp);

}}
#endif
