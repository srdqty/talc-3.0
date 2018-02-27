#ifndef POPSYNTAX_H
#define POPSYNTAX_H

#include "list.h"
#include "core.h"
#include "gcdfec.h"

prefix Popsyntax {
open   Popsyntax {
open   List      {
open   Core      {

#define var        string
#define type_name  string
#define field_name string

extern union scope      { void Static, Public, Extern, Abstract; }
extern union capability { void ReadOnly, ReadWrite; }
extern union convention { void Stdcall, Cdecl; }
extern union size_of    { void B1, B2, B4; }
// Any == any type at all, any kind
// Byte4 == any 4 byte type
// Option == any possibly null pointer type.
extern union var_class  { void Any, Option, Byte4; }
// voids,floats,and doubles have kind Kother, everything else is K4
// Kany is a superkind.
extern union kind       { void Kany, K4, Kother; }

extern union typ {
  void                       VoidType;
  *(var_class,<typ>Opt,int)  Evar; // int is unique id
  var                        VarType;
  *(bool, size_of)           IntType;
  void                       BooleanType;
  void                       StringType;
  void                       CharType;

  void                       FloatType;
  void                       DoubleType;

  *(typ,<exp>Opt)            ArrayType; // exp is optional size
  *(typ,int,bool)            CArrayType; // int is size, bool is optional?
  *(convention,<var>list,typ,<typ>list) FnType;
  <typ>list                  TupleType;
  *(type_name,<typ>list)     NamedType;
  void                       ExnType;
  *(typ)                     MutableTyp;
  *(type_name,<typ>list)     UnresolvedTyId;
}

extern union cnst {
  int    Int;
  bool   Bool;
  string String;
  char   Char;
  void   Null;

  float Float;
  double Double;

}

extern union primop { 
  void Plus, Times, TimesU, Minus, Div, DivU, Mod, ModU,
    Eq, Neq, Gt, GtU, Lt, LtU, Gte, GteU, Lte, LteU, Not,
    Bitnot, Bitand, Bitor, Bitxor, Bitlshift, Bitlrshift, Bitarshift,
    Size, Ord, Chr;
  // IL operations only
  *(bool,size_of,size_of) Resize; // Used for cast. bool == is src signed!
  //    Conversions between ints, floats, and doubles.
  void ItoF, ItoD, FtoI, DtoI, FtoD, DtoF; 
  // standard floating point operations.
  void  PlusF, MinusF, TimesF, DivF, EqF, NeqF, GtF, GteF, LtF, LteF;
  // built-in operators.
  // nullary
  void PiF, Log2_eF, Log2_10F, Log10_2F, Loge_2F, OneF, ZeroF;
  // unary
  void CosF, SinF, TanF, SqrtF, F2xm1F, FabsF, FroundF;
  // binary
  void AtanF, FremF, Fyl2xF, Fyl2xp1F;
}

extern union raw_exp {
  cnst                      Const;
  *(<exp>list, <typ>Opt)    ConstArray;
  <exp>list                 ConstCArray;
  var                       Var;
  *(primop, <exp>list)      Primop;
  *(exp,exp,exp)            Conditional;
  *(exp,<primop>Opt,exp)    AssignOp;
  *(exp,< <typ>list>Opt, <exp>list)                   FunCall;
  *(exp,<typ>list)                                    TypInst;
  // either all field names are present or non are
  *(type_name, <<typ>list>Opt, <*(<field_name>Opt,exp)>list) NewStruct;
  *(exp, field_name)                                  StructMember;
  *(type_name, < <typ>list>Opt, field_name, <exp>Opt) NewUnion;
  *(exp,field_name) UnionMember;
  <exp>list         NewTuple;
  *(exp,int)        TupleMember;
  *(exp,exp)        Subscript;
  *(exp,exp)        CSubscript;
  // Cyclone
  fndecl            Codegen;
  exp               Fill;
  // End Cyclone
  *(var, <exp>Opt)  NewExn;
  exp               Raise;
  <exp>list         SeqExp;
  void              Nop;
  *(typ,exp)        Cast;
  *(type_name,<<typ>list>Opt,<<typ>list>Opt,exp)     NewAbstype;
  fndecl            Fun; // nested function definition
}  
extern struct exp {
  <typ>Opt    typ;
  raw_exp     raw_exp;
  <var>list   assigned_to; //made during type-checking, used in Poptranslate
  Gcdfec::seg loc;
}

extern union raw_stmt {
  void                Skip;
  exp                 Exp;
  *(stmt,stmt)        Seq;
  <exp>Opt            Return;
  *(exp,stmt,stmt)    IfThenElse;
  *(exp,stmt)         While;
  <var>Opt            Break;
  <var>Opt            Continue;
  *(exp,exp,exp,stmt) For;
  *(exp,<*(int,stmt)>list,stmt)     IntSwitch;
  *(exp,<*(char,stmt)>list,stmt)    CharSwitch;
  *(exp,<switch_arm>list,<stmt>Opt) UnionSwitch;
  *(exp,<switch_arm>list,<stmt>Opt) ExnSwitch; // default isn't really optional
  *(var,typ,<exp>Opt,stmt)          Decl;
  // Cyclone
  stmt             Cut;
  stmt             Splice;
  // End Cyclone
  *(var,stmt)      Label;
  *(stmt,exp)      Do;
  *(stmt,var,stmt) TryHandle;
  //                      default   finally
  *(stmt,<switch_arm>list,<stmt>Opt,<stmt>Opt) TryCatchFinally;
  *(var,<typ>Opt,<var>list,exp,stmt) With;
  *(exp,exp) Rdtsc;
}
extern union prim_pattern {
  *(typ)     Wild;
  *(var,typ) Var;
}
extern union pattern {
  void               None;
  prim_pattern       Prim;
  <prim_pattern>list Tuple;
}
extern struct switch_arm { 
  field_name field;
  pattern    pat;
  stmt       body;
}
extern struct stmt {
  raw_stmt    raw_stmt;
  Gcdfec::seg loc;
}
extern struct fndecl {
  bool              is_static;
  convention        convention;
  var               name;
  <var>list         tyvars;
  typ               ret_type;
  <*(var,typ)>list  args;
  stmt              body;
}
extern struct structdecl {
  scope     scope;
  type_name name;
  <var>list tyvars;
  bool      possibly_null;
  <*(field_name,capability,typ)>list fields;
}
extern struct uniondecl {
  scope     scope;
  type_name name;
  <var>list tyvars;
  <*(field_name,typ)>list fields;
}
extern struct absdecl {
  scope     scope;
  type_name name;
  <var>list all_tyvars;
  <var>list exist_tyvars;
  typ       defn;
}
extern union raw_top_decl {
  fndecl                      FunDecl;
  structdecl                  StructDecl;
  uniondecl                   UnionDecl;
  absdecl                     AbsDecl;
  *(var,scope,typ)            ExceptionDecl;
  *(type_name,<var>list,bool) ExternType; // bool indicates option
  *(var,typ)                  ExternVal;
  *(scope,var,typ,<exp>Opt)   GlobalDecl;
  *(var,<top_decl>list)       PrefixDecl;
  *(var,<top_decl>list)       OpenDecl;
}
extern struct top_decl {
  raw_top_decl raw_top_decl;
  Gcdfec::seg  loc;
}

extern string null_exception_name;
extern string union_exception_name;
extern string array_exception_name;
extern top_decl make_predefined_exn(string);
extern top_decl make_memType();

extern convention default_convention;

extern string var2string(var); // replaces each '?' with "::"

extern string typ2string(typ);
extern string primop2string(primop);
extern string add_prefix(string, var);
extern bool   size_leq  (size_of,size_of);
extern typ    new_evar(var_class);

extern kind kind_of_typ(typ t);

// Returns the arity and operator associated with this variable if 
// the variable represents a special operator like cos.
// prefixes should already have been expanded!!
extern <*(int,primop)>Opt is_special(var);

}}}}
#endif
