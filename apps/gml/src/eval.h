#ifndef __EVAL_H
#define __EVAL_H

#include "gmlsyntax.h"

prefix Eval {
open Eval;

// Closures

extern struct surf_prop {
  FP n;
  FP ks;
  FP kd;
  FP red;
  FP green;
  FP blue;
}

extern union closure {
  Gmlsyntax::value Unopt;
  Gmlsyntax::value Opt;
  surf_prop Cst;
}

 extern <Gmlsyntax::value>list eval(Gmlsyntax::value clos,
				   <Gmlsyntax::value>list stack);

extern surf_prop eval_surface_fun(closure f, FP u, FP v, int face);

extern closure optimize_surface_fun(closure f);

}

#endif
