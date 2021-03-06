#ifndef ABSYN_H
#define ABSYN_H

/******************************************************************************
 * abstract syntax: data types ("imported" from parse.pop)
 *****************************************************************************/

#include "list.h"

/* packets */

extern struct pop_pkt {
  <def> List::list program;
  string fn_to_exec;
  <value> List::list actuals;
  string sourceAddr;
  string evalAddr;
  int rb;
}
  
/* PLAN programs */

extern union def {
  fundef Fundef;
  string Exndef;
  valdef Valdef;
}

extern struct fundef {
  string    var;
  <*(string, tipe)> List::list params;
  tipe      result_type;
  exp       body;
}

extern struct valdef {
  string var;
  tipe   tipe;
  exp    exp;
}

extern union tipe {
  <tipe> List::list Tuple;
  tipe     List;
  void     Int;
  void     Char;
  void     String;
  void     Bool;
  void     Host;
  void     Port;
  void     Key;
  void     Blob;
  void     Exn;
  void     Dev;
  void     Chunk;
  tyvar    Var;
}

extern struct tyvar {
  int     id;
  tipeopt topt;
}

extern ?struct tipeopt {
  tipe t;
}

extern union unop {
  void NEG;
  void NOT;
  void HD;
  void TL;
  void FST;
  void SND;
  int NTH;
  void NOTI;
  void EXPLODE;
  void IMPLODE;
  void ORD;
  void CHR;
}

extern union binop {
  void DIV;
  void MOD;
  void TIMES;
  void PLUS;
  void MINUS;
  void AND;
  void OR;
  void LESS;
  void LESSEQ;
  void GREATER;
  void GREATEREQ;
  void EQ;
  void NOTEQ;
  void CONS;
  void CONCAT;
  void LSHIFT;
  void RSHIFT;
  void XORI;
  void ANDI;
  void ORI;
}

  /*
   * todo: Host, Blob, Port, Key, Exception
   */

extern union value {
  void                          Unit;
  int                           Int;
  char                          Char;
  string                        String;
  bool                          Bool;
  <value> List::list            VTuple;
  <value> List::list            VList;
  *(string, <value> List::list, <def> List::list) VChunk;
}

extern union exp {
  value                       Val;
  string                      Var;
  <exp> List::list            ETuple;
  <exp> List::list            ESequence;
  *(string, <exp> List::list) App;
  *(exp,exp,exp)              If;
  string                      Raise;
  *(exp,string,exp)           Try;
  *(<def> List::list,exp)     Let;
  *(unop,exp)                 Unop;
  *(string, <exp> List::list) EChunk;
  *(binop,exp,exp)            Binop;
  *(exp,exp,exp,exp)          OnRemote;
  *(exp,exp,exp,exp)          OnNeighbor;
  *(exp,exp,exp,exp,exp,exp)  RetransOnRemote;
  *(string,exp,exp)           Foldr;
  *(string,exp,exp)           Foldl;
}

#endif
