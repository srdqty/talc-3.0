#ifndef ID_H
#define ID_H
////////////////////////////////////////////////////////////////////////////
// Popcorn library, file id.h                                             //
// Copyright Greg Morrisett, Dan Grossman                                 //
// January 1999, all rights reserved                                      //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "core.h"
#include "string.h"

prefix Id {
open   Id {
open   Core {
open String {

// id.h:    defines an identifiers used in compilers, type-checkers, etc.

// identifiers are abstract (though there's not much too them)
extern id;

// make an id that is equal to other id's with the same string and int
extern id id_make(string,int);

// make an id with input string, but not equal to any other id
extern id id_new(string);

// Generate a new unique id from an existing one.
 extern id id_renew(id);

// make an id with input string, equal to other ids with this string and int -1
extern id id_of_string(string);

// same as id_new
extern id id_unique(string);

// return string and int portions concatenated with a '$' in-between
extern string id_to_string(id);

// print an identifier to FILE
extern void id_prn(FILE,id);

// return string portion of an id
extern string id_to_source(id);

// defines a total ordering on ids, return 0 if equal, -1 if first arg is first,
//                                                     +1 if second arg is first
extern int id_compare(id,id);

}}}}

#endif
