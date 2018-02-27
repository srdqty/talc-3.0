#ifndef LIST_H
#define LIST_H
////////////////////////////////////////////////////////////////////////////
// Popcorn library, file list.h                                           //
// Copyright Greg Morrisett, Dan Grossman                                 //
// January 1999, all rights reserved                                      //
//                                                                        //
////////////////////////////////////////////////////////////////////////////
#include <core.h>

prefix List {
open   List {

// list.h:   defines generic lists and various operations over them,
//           following the conventions of the Ocaml list library as much
//           as possible.  Also, a reasonable attempt to make things as
//           space efficient as possible, while remaining "functional".

extern ?struct <a>list { a hd; <a>list tl; }

// Return the length of a list. */
extern int length<a>(<a>list x);

extern <a>list cons<a>(a hd,<a>list tl);

// raised when some function expects a non-empty list
extern exception List_empty;

// return the first element in a list, if any, raise List_empty otherwise. 
extern a hd<a>(<a>list x);

// return the tail of a list, if any, raise List_empty otherwise. 
extern <a>list tl<a>(<a>list x);

// return a fresh copy of the list (same as map of the identity)
extern <a>list copy<a>(<a>list src);

// Apply a function to each element in a list, returning a new list. 
extern <b>list map<a,b>(b f(a),<a>list x);

// Apply a "closure" to each element in a list, returning a new list.
extern <b>list map_c<a,b,c>(b f(c,a),c env,<a>list x);

// raised when two lists don't have the same size 
extern exception List_mismatch;

// Given two lists of the same length and a function, apply the function
// to each pair of elements (in order) and collect the results in a new list.
// Raises List_mismatch if the sizes of the lists aren't the same.
extern <c>list map2<a,b,c>(c f(a,b),<a>list x,<b>list y);

// Apply some function to each element of the list, but don't bother to
// save the result.  Similar to Ocaml's List.iter but doesn't require
// void type for the result of the function.
extern void app<a,b>(b f(a),<a>list x);
// Same but function gets an extra argument to simulate closures.
extern void app_c<a,b,c>(c f(a,b),a,<b>list x);

// Same as app, but generalized to a pair of lists.  Raises List_mismatch
// if the lengths of the list aren't the same.  
extern void app2<a,b,c>(c f(a,b),<a>list x,<b>list y);
extern void app2_c<a,b,c,d>(d f(a,b,c),a env,<b>list x,<c>list y);

// Similar to app but assumes function returns void type
extern void iter<a>(void f(a),<a>list x);
extern void iter_c<a,b>(void f(b,a),b env,<a>list x);
extern void iter2<a,b>(void f(a,b),<a>list x,<b>list y);
extern void iter2_c<a,b,c>(void f(a,b,c),a env,<b>list x,<c>list y);

// Given a list [x1,x2,...,xn-1,xn], a function f, and an accumulator a,
// return f(f(...(f(x2,f(x1,a))),xn-1),xn).  Notice that the function is
// first applied to the left-most element of the list.
extern a fold_left<a,b>(a f(a,b),a accum,<b>list x);
extern a fold_left_c<a,b,c>(a f(c,a,b),c,a accum,<b>list x);

// Given a list [x1,x2,....,xn-1,xn], a function f, and an accumulator a,
// return f(x1,f(x2,...,f(xn-1,f(xn,a))...)).  Notice that the function is
// first applied to the right-most element of the list. 
extern b fold_right<a,b>(b f(a,b),<a>list x,b accum);
extern b fold_right_c<a,b,c>(b f(c,a,b),c,<a>list x,b accum);

// Given [x1,...,xn] and [y1,...,ym], return [xn,...,x1,y1,...,ym].
// That is, the first list reversed and appended to the second list.
extern <a>list revappend<a>(<a>list x,<a>list y);

// Return the reverse of a list. 
extern <a>list rev<a>(<a>list x);
// Imperatively reverse the list
extern <a>list imp_rev<a>(<a>list x);

// Return a new list that has elements of x followed by elements of y
extern <a>list append<a>(<a>list x,<a>list y);
// Modify x so that y is appended to it destructively -- if x is empty return y
extern <a>list imp_append<a>(<a>list x,<a>list y);

// flatten a list of lists into a single list
extern <a>list flatten<a>(<<a>list>list x);

// Given a partial order less_eq on 'a elements and a list, return
// the list sorted by less_eq.  Uses a merge sort.  The less_eq
// function should return 0 if the elements are equal, i < 0 if
// the first is less than the second, and i > 0 otherwise.
extern <a>list merge_sort<a>(int less_eq(a,a), <a>list x);
extern <a>list merge_sort_c<a,b>(int less_eq(b,a,a), b env, <a>list x);

// Merge two (sorted) lists using the less_eq operation.
extern <a>list merge<a>(int less_eq(a,a),<a>list a,<a>list b);
extern <a>list merge_c<a,b>(int less_eq(b,a,a),b env,<a>list a,<a>list b);

// raised when list_nth doesn't have enough elements in the list. 
extern exception Nth;

// Given [x0,x1,...,xn], return the ith element of the list (0 <= i <= n).
// Raise Nth if the list doesn't have enough elements.  Notice that the
// indexing is zero-based.
extern a nth<a>(<a>list x,int i);

// Given a predicate on 'a values, determine whether every element in a list
// satisfies the predicate.
extern bool forall<a>(bool pred(a),<a>list x);
extern bool forall_c<a,b>(bool pred(a,b),a env,<b>list x);

// Given a predicate on 'a values, determine whether there exists an element
// in the list that satisfies the predicate.
extern bool exists<a>(bool pred(a),<a>list x);

// Given [x1,...,xn] and [y1,...,yn], return [(x1,y1),...,(xn,yn)].  
// Raises List_mismatch if the lengths are not the same.
extern <*(a,b)>list zip<a,b>(<a>list x,<b>list y);

// Given [(x1,y1),...,(xn,yn)], return ([x1,...,xn],[y1,...,yn]) */
extern *(<a>list,<b>list) split<a,b>(<*(a,b)>list x);
extern *(<a>list,<b>list,<c>list) split3<a,b,c>(<*(a,b,c)>list x);
  
// Given a list [x1,...,xn] and x, determine if x is in the list.  Uses
// physical equality for comparison.
extern bool memq<a>(<a>list l,a x);
extern bool mem<a>(int compare(a,a), <a>list l, a x);

// same as memq, but removes the element from the list, if found,
// and returns back the new list -- not functional
extern <a>list rmq<a>(<a>list l,a x);
extern <a>list rm<a>(<a>list l,a x,int compare(a,a));

// Raised by list_assoc and list_mem_assoc (now defined in Core)
//extern exception Not_found;

// Given an association list [(k1,d1),...,(kn,dn)] and a key x,
// return the first di such that ki = x.  Uses physical equality.
// Raises Not_found if no such ki exists.
extern b assoc<a,b>(<*(a,b)>list l,a x);
extern b assoc_cmp<a,b>(int compare(a,a), <*(a,b)>list l,a x);

// Given an association list [(k1,d1),...,(kn,dn)] and a key x,
// returns true iff there exists a ki = x.
extern bool mem_assoc<a,b>(<*(a,b)>list l,a x);

// checks that a list of elements is unique -- assumes they're sorted
// returns an element that is duplicated if one exists, otherwise returns
// null.
extern <c>Core::Opt check_unique<c>(int cmp(c,c),<c>list x);

// makes a new array with index i being the ith element of the list
extern a to_array<a>(<a>list x) [];

// makes a new list with ith element arr[i]
extern <a>list from_array<a>(a arr[]);

// filters out all elements for which pred is true, returning a new list
// in the same order as the original elements.
extern <a>list filter<a>(bool pred(a), <a>list x);
extern <a>list filter_c<a,b>(bool pred(b,a), b env, <a>list x);

// splits a list into those elements satisfying a predicate and those that dont
extern *(<a>list,<a>list) split_filter<a>(bool pred(a), <a>list x);
extern *(<a>list,<a>list) split_filter_c<a,b>(bool pred(b,a),b env,<a>list x);

// generate a list of length i by calling f with the values [0..i-1]
extern <a>list tabulate<a>(int i, a f(int));
extern <a>list tabulate_c<a,b>(int i, a f(b,int), b env);

}}
#endif
