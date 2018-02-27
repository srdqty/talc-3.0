#ifndef DICT_H
#define DICT_H
////////////////////////////////////////////////////////////////////////////
// Popcorn library, file dict.h                                           //
// Copyright Greg Morrisett, Dan Grossman                                 //
// January 1999, all rights reserved                                      //
//                                                                         //
////////////////////////////////////////////////////////////////////////////
#include <core.h>
#include <list.h>

prefix Dict {
open   Dict {
open   List {

// dict.h:   defines polymorphic, functional, finite maps from types 'a to 
//           'b, where the domain must have a total order.  Follows the
//           conventions of the Ocaml dict library as much as possible.

extern dict<a,b>;

// Raised when a key is present but not expected (e.g., insert_new) 
extern exception Present;
// Raised when a key is not present but expected (e.g., lookup) 
extern exception Absent;

// Given a comparison function, return an empty dict. 
extern <a,b>dict empty<a,b>(int comp(a,a));

// Determine whether a dict is empty 
extern bool is_empty<a,b>(<a,b>dict d);

// Return true if entry indexed by key is present, false otherwise 
extern bool member<a,b>(<a,b>dict d,a key);

// Inserts a key/data pair into a dictionary, replacing any existing
// pair with the same key. 
extern <a,b>dict insert<a,b>(<a,b>dict d,a key,b data);

// Inserts a key/data pair into a dictionary, raising Present if
// there is any existing pair with the same key.
extern <a,b>dict insert_new<a,b>(<a,b>dict d,a key,b data);

// Insert a list of key/data pairs into a dictionary, replacing
// duplicate keys.
extern <a,b>dict inserts<a,b>(<a,b>dict d,<*(a,b)>list kds);

// Return a dictionary containing exactly one key/data pair. 
extern <a,b>dict singleton<a,b>(int compare(a,a),a key,b data);

// Lookup a key in the dictionary, returning its associated data. If the key
// is not present, raise Absent.
extern b lookup<a,b>(<a,b>dict d,a key);

// Same as lookup but doesnt raise an exception -- rather, returns an
// option.
extern <b>Core::Opt lookup_opt<a,b>(<a,b>dict d,a key);

// Delete a key/pair from the dict if present. 
extern <a,b>dict delete<a,b>(<a,b>dict d,a key);

// Delete a key/pair from the dict.  Raise Absent if key doesn't exist 
extern <a,b>dict delete_present<a,b>(<a,b>dict d,a key);

// Fold a function f across the dictionary yielding an accumulator. 
extern c fold<a,b,c>(c f(a,b,c),<a,b>dict d,c accum);
// Same but fold an unboxed closure across the dictionary
extern c fold_c<a,b,c,d>(c f(d,a,b,c),d env,
			      <a,b>dict dict,c accum);

// Apply function f to every element in the dictionary.  Ignore result. 
extern void app<a,b,c>(c f(a,b),<a,b>dict d);
// Same but apply an unboxed closure across the dictionary
extern void app_c<a,b,c,d>(c f(d,a,b),d env,<a,b>dict d);
// void versions of the above
extern void iter<a,b>(void f(a,b),<a,b>dict d);
extern void iter_c<a,b,c>(void f(c,a,b),c env,<a,b>dict d);

// Given a function that maps 'b values to 'c values, convert an
// <'a,'b>dict to a <'a,'c>dict by applying the function to each
// data item.
extern <a,c>dict map<a,b,c>(c f(b),<a,b>dict d);
// Same but map an unboxed closure across the dictionary
extern <a,c>dict map_c<a,b,c,d>(c f(d,b),d env,<a,b>dict d);

// Return a key/data pair (in this case -- the first one in the dict).
// If the dict is empty, raise Absent.
extern *(a,b) choose<a,b>(<a,b>dict d);

// Return an association list containing all the elements
extern <*(a,b)>list to_list<a,b>(<a,b>dict d);

}}}

#endif
