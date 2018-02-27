#ifndef TOC_H
#define TOC_H

#include "list.h"
#include "absyn.h"

prefix Toc {
  open Toc {
  open List {
  open Absyn {

// translate the declarations to C
extern <decl>list toc(<decl>list ds);
// set to false/true to turn off/on null checking in generated code
extern bool check_for_null;
}}}}
#endif
