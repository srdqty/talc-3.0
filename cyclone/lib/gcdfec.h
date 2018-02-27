
#ifndef GCDFEC_H
#define GCDFEC_H

#include "core.h"
#include "list.h"
#include "string.h"
#include "ref.h"
#include "lineno.h"

namespace Gcdfec {
using Core;
using List;
using String;

extern xenum exn { Exit };
extern void   reset_fe(string);
extern void   set_ctxt(string);
extern string get_ctxt();

//////// Location Tracking /////////
extern struct seg;
typedef struct seg @Seg;

extern Seg    dummy_seg;
extern Seg    seg_of_abs   (int,int);
extern Seg    seg_join     (Seg,Seg);
extern string string_of_loc(int);
extern string string_of_seg(Seg);

//////// Errors ////////////////////
extern enum error_kind {
  Lex, Parse, Elab
};
extern struct error {
  string     source;
  Seg        segment;
  enum error_kind kind;
  string     desc;
};
typedef struct error @Error;
extern Error mk_err_lex  (Seg,string);
extern Error mk_err_parse(Seg,string);
extern Error mk_err_elab (Seg,string);

//////// Error Reporting ///////////
extern xenum exn { Nocontext };
extern void post_error(Error);
extern bool error_p   ();
extern bool print_context;

}
#endif
