
#ifndef GCDFEC_H
#define GCDFEC_H

#include "core.h"
#include "list.h"
#include "string.h"
#include "arg.h"
#include "lineno.h"

prefix Gcdfec {
open   Gcdfec {
open   Core   {
open   List   {
open   String {

extern exception Exit;
extern void   reset_fe(string);
extern void   set_ctxt(string);
extern string get_ctxt();

//////// Location Tracking /////////
extern seg;
extern seg    dummy_seg;
extern seg    seg_of_abs   (int,int);
extern seg    seg_join     (seg,seg);
extern string string_of_loc(int);
extern string string_of_seg(seg);

//////// Errors ////////////////////
extern union error_kind { void Lex, Parse, Elab; }
extern struct error {
  string     source;
  seg        segment;
  error_kind kind;
  string     desc;
}
extern error mk_err_lex  (seg,string);
extern error mk_err_parse(seg,string);
extern error mk_err_elab (seg,string);

//////// Error Reporting ///////////
extern exception Nocontext;
extern void post_error(error);
extern bool error_p   ();
extern Arg::bool_ref print_context;

}}}}}
#endif
