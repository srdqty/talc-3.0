#ifndef ABSYN_H
#define ABSYN_H

#include <list.h>
open List;


/***************************************************************************
 * data types
 **************************************************************************/

extern union tipe {
  <tipe> list Tuple;
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
}

extern union unop {
  void NEG;
  void NOT;
  void HD;
  void TL;
  void FST;
  void SND;
  int  NTH;
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


extern union value {
  void                          Unit;
  int                           Int;
  char                          Char;
  string                        String;
  bool                          Bool;
  <value> list                  VTuple;
  <value> list                  VList;
  *(string, <value> list, <def> list) VChunk;
  /* todo: Host, Blob, Port, Key, Exception, Dev */
}


extern union exp {
  value                      Val;
  string                     Var;       
  <exp> list                 ETuple;
  <exp> list                 ESequence;
  *(string, <exp> list)      App;
  *(exp,exp,exp)             If;
  string                     Raise;
  *(exp,string,exp)          Try;
  *(<def> list,exp)          Let;
  *(unop,exp)                Unop;
  *(string, <exp> list)      EChunk;
  *(binop,exp,exp)           Binop;
  *(exp,exp,exp,exp)         OnRemote;
  *(exp,exp,exp,exp)         OnNeighbor;
  *(exp,exp,exp,exp,exp,exp) RetransOnRemote;
  *(string,exp,exp)          Foldr;
  *(string,exp,exp)          Foldl;
}

extern struct valdef {
  string var;
  tipe   tipe;
  exp    exp;
}

extern struct formal
{
  string id;
  tipe t;
}

extern struct fundef {
  string        var;
  <formal> list params;
  tipe          result_type;
  exp           body;
}

extern union def {
  valdef Valdef;
  fundef Fundef;
  string Exndef;
}

extern struct pop_pkt {
  <def> list program;
  string fn_to_exec;
  <value> list actuals;
  string sourceAddr;
  string evalAddr;
  int rb;
}

/***************************************************************************
 * printer functions
 **************************************************************************/

extern string exp_to_str(exp);

#endif
