#ifndef QUEUE_H
#define QUEUE_H
////////////////////////////////////////////////////////////////////////////
// Popcorn library, file queue.h                                          //
// Copyright Greg Morrisett, Dan Grossman                                 //
// January 1999, all rights reserved                                      //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

prefix Queue {
open   Queue {

// queue.h:  defines generic imperative queues and various operations
//           following the conventions of the Ocaml queue library as much
//           as possible.  

extern queue<a>;

// true when the queue is empty
extern bool is_empty<a>(<a>queue);

// raised by queue_take and queue_peek
extern exception Queue_Empty;

// create a new queue
extern <a>queue create<a>();

// insert an element into the rear of the queue (side effect)
extern void add<a>(<a>queue,a x);

// get and remove an element from the front of the queue (side effect)
extern a take<a>(<a>queue);

// return the first element in the queue without removing it
extern a peek<a>(<a>queue);

// clear out the entire queue (side effect)
extern void queue_clear<a>(<a>queue);

// return the number of lements in the queue
extern int queue_length<a>(<a>queue);

// apply f to each element in the queue from the front to the back
extern void iter<a,b>(b f(a), <a>queue);

}}

#endif

