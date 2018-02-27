#ifndef SET_H
#define SET_H
////////////////////////////////////////////////////////////////////////////
// Popcorn library, file set.h                                            //
// Copyright Greg Morrisett, Dan Grossman                                 //
// January 1999, all rights reserved                                      //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "list.h"

prefix Set {
open   Set {
open List  {

// set.h:    defines polymorphic, functional, finite sets over elements
//           with a total order, following the conventions of the Ocaml set
//           library as much as possible.  

// sets are polymorphic and abstract
extern set<a>;

// The set creation functions take a functional argument that takes
// two elements and returns 0 when equal, -1 when the first is less
// than the second, and 1 when the second is less than the first.  

// create an empty set given a comparison function
extern <a>set empty<a>(int comp(a,a));

// create a singleton set given a comparison function and element
extern <a>set singleton<a>(int comp(a,a),a x);

// create a set out of a comparison function and a list of elements
extern <a>set from_list<a>(int comp(a,a),<a>list x);

// insert an element into a set
extern <a>set insert<a>(<a>set s,a elt);

// union of two sets
extern <a>set union_two<a>(<a>set s1,<a>set s2);

// intersection of two sets
extern <a>set intersect<a>(<a>set s1,<a>set s2);

// set difference -- remove from s1 all elements in s2
extern <a>set diff<a>(<a>set s1,<a>set s2);

// deletes an element if its present, otherwise returns same set
extern <a>set delete<a>(<a>set s,a elt);

// number of unique elements in the set
extern int cardinality<a>(<a>set s);

// true when the set is empty
extern bool is_empty<a>(<a>set s);

// true when elt is in the set
extern bool member<a>(<a>set s,a elt);

// true when s1 is a (not necessarily proper) subset of s2
extern bool subset<a>(<a>set s1,<a>set s2);

// true when s1 and s2 contain the same elements
extern int  compare<a>(<a>set s1,<a>set s2);
extern bool equals <a>(<a>set s1,<a>set s2);

// returns a list of the elements (in no guarateed order)
extern <a>list elements<a>(<a>set s);

// if s = {x1,x2,...,xn} then return f(x1,f(x2,f(...,f(xn,accum)...)))
extern b fold<a,b>(b f(a,b),<a>set s,b accum);
extern b fold_c<a,b,c>(b f(c,a,b),c env,<a>set s,b accum);

// apply the function f to each element of the set and discard the result
extern void app<a,b>(b f(a),<a>set s);
extern void iter<a>(void f(a),<a>set s);
extern void iter_c<a,c>(void f(c,a),c env,<a>set s);

extern exception Absent;
extern a choose<a>(<a>set s);

}}}
#endif
