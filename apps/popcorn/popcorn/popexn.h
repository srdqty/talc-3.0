#ifndef POPEXN_H
#define POPEXN_H

#include "list.h"
#include "popsyntax.h"
#include "poptypeenv.h"

prefix PopExn {
open PopExn;
open Popsyntax;
open List;

 extern <top_decl>list rewrite(Poptypeenv::global_env genv, <top_decl>list prog);

}

#endif
