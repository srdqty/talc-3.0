
////////////////////////////////////////////////////////////////////////////
// Popcorn library, file poploader.h                                      //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#ifndef POPLOADER_H
#define POPLOADER_H

prefix Poploader {
  open Poploader {

/* both raise Core::Failure on failure */
extern a pop_load<a>(string talbuf, string tobuf, <a>rep trep);
extern a pop_cast<a,b>(b v, <b>rep t1, <a>rep t2);

}}
extern void tal_print_typerep<a>(<a>rep t);
#endif
