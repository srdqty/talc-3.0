#ifndef POPERR_H
#define POPERR_H

#include "id.h"
#include "core.h"

prefix Poperr {
open   Poperr {

extern union lexerError {
  string IllegalStringCharacter;
  void   RunawayComment;
  void   RunawayString;
  void   InvalidHexNum;
  void   InvalidChar;
  void   NonWhitespace;
}

extern union parseError {
  string Syntax;
  void   SwitchClausesDontMatch;
  void   ExternNoInit;
  void   ParamNotNamed;
  void   ParseUnimplemented;
}

extern union tcError {
  string Unimplemented;
  string Impossible;
  string TypeError;
}

extern union tcWarn {
  Id::id     WshadowVar;
}

extern union errorb {
  parseError Eparse;
  lexerError Elexer;
  tcError    Etypecheck;
  tcWarn     Wtypecheck;
}

extern union errorType {
  int  ETwarning;
  void ETerror;
  void ETpedantic;
  void ETcomment;
}

extern errorType error_type(errorb);

extern string error_message(errorb);

}}
#endif
