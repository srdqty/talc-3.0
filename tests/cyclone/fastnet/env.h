#ifndef ENV_H
#define ENV_H
open List;

#include "absyn.h"

prefix Env {
  open Env;

  extern struct environ
  {
    string id;
    binding b;
  }
  
  extern union binding {
    value  Varbind;
    string Exnbind;
    *(<formal> list, tipe, exp, <environ> list) Funbind;
    string Svcbind;
  }
  
  extern <environ> list add(string, binding, <environ> list);
  extern <environ> list add_actuals(<formal> list,
                                    <value> list,
                                    <environ> list);

  extern binding
    lookup(string,
	   <environ> list,
	   <*(string, value (<value> list vl, pop_pkt ppkt))> list);
  
  extern void    print(<environ> list env);
  
}

#endif
