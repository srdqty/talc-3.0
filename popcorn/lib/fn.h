#ifndef FN_H
#define FN_H
////////////////////////////////////////////////////////////////////////////
// Popcorn library, file list.pop                                         //
// Copyright Greg Morrisett, Dan Grossman                                 //
// January 1999, all rights reserved                                      //
//                                                                        //
// Defines "closures" and operations on them                              //
////////////////////////////////////////////////////////////////////////////
#include <list.h>

prefix Fn {
open   Fn {

// a closure is a function pointer and abstract environment
extern abstype <a,b>fn[c] = *(b f(c,a), c);

// make a closure out of a function pointer and environment
extern <a,b>fn make_fn<a,b,c>(b f(c,a), c);

// convert a function pointer to a closure
extern <a,b>fn fp2fn<a,b>(b f(a));

// apply closure f to argument x
extern b apply<a,b>(<a,b>fn f,a x);

// compose closures
extern <a,c>fn compose<a,b,c>(<a,b>fn g, <b,c>fn f);

// curry a closure that takes a pair
extern <a,<b,c>fn>fn curry<a,b,c>(<*(a,b),c>fn f);

// uncurry a closure
extern <*(a,b),c>fn uncurry<a,b,c>(<a,<b,c>fn>fn f);

// map a closure across a list
extern <b>List::list map_fn<a,b>(<a,b>fn f,<a>List::list x);

}}
#endif
