#ifndef DLLIST_H
#define DLLIST_H
////////////////////////////////////////////////////////////////////////////
// Popcorn library, file dllist.pop                                       //
// Copyright Michael Hicks                                                //
// November 2000, all rights reserved                                     //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

prefix DLList {
open   DLList {

/* dllist.pop: defines doubly-linked lists.  An empty list actually
 *             contains a dummy head element so that an element can
 *             be removed without knowing the head of the list.
 */
?struct <a>dllist { 
  a hd; 		// the value at this list element
  <a>dllist tl; 	// the next element
  *(<a>dllist) prev; 	// pointer to the previous element's next pointer
}

/* given the current head of the list and a value, add a new
   value to the front */
extern void cons<a>(a val, *(<a>dllist) hd);

/* raised when some function expects a non-empty list */
extern exception DLList_empty;

/* given a pointer to some element of a dllist, remove that element
   from whatever list it's on */
extern void excise<a>(<a>dllist x);

/* removes the element with the given value; equality is tested
   with the given comparison operator */
extern void rm<a>(<a>dllist l,a x,int compare(a,a));

/* same as app but with void functions */
extern void iter<a>(void f(a),<a>dllist x);

/* Return the length of a dllist. */
extern int length<a>(<a>dllist x);

}}

#endif
