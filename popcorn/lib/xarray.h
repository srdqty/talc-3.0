#ifndef XARRAY_H
#define XARRAY_H

// Re-sizing arrays.  Ported with some modification by Dan from xarray.ml, 
// which was written by Steve

// Note these arrays never shrink in size.

#include "core.h"
open Core {
prefix Xarray {
open   Xarray {

extern xarray<a>;

extern int length<a>(<a>xarray);

extern a  get<a>(<a>xarray, int);

extern void set<a>(<a>xarray, int, a);

extern <a>xarray create<a>(int, a);

extern <a>xarray create_empty<a>();

extern <a>xarray singleton<a>(int, a);

extern void add<a>(<a>xarray, a);

extern int add_ind<a>(<a>xarray, a);

extern a to_array<a>(<a>xarray) [];

extern <a>xarray from_array<a>(a arr[]);

extern <a>xarray append<a>(<a>xarray, <a>xarray); // functional

extern void imp_append<a>(<a>xarray, <a>xarray); // imperative

extern void app<a,b>(b f(a), <a>xarray);

extern void app_c<a,b,c>(b f(c,a), c, <a>xarray);

extern void iter<a>(void f(a), <a>xarray);

extern void iter_c<a,c>(void f(c,a), c, <a>xarray);

extern <b>xarray map<a,b>(b f(a), <a>xarray);

extern <b>xarray map_c<a,b,c>(b f(c,a), c, <a>xarray);

extern void reuse<a>(<a>xarray xarr);

// Delete the last num elements.
extern void delete<a>(<a>xarray xarr, int num);

// Remove the element at position i.
// Slide all higher elements down one position.
extern void remove<a>(<a>xarray xarr, int i); 

}}}
#endif
