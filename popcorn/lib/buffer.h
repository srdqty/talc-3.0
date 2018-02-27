#ifndef BUFFER_H
#define BUFFER_H

// Ported from the standard library of 
//(***********************************************************************)
//(*                                                                     *)
//(*                           Objective Caml                            *)
//(*                                                                     *)
//(*  Pierre Weis and Xavier Leroy, projet Cristal, INRIA Rocquencourt   *)
//(*                                                                     *)
//(*  Copyright 1999 Institut National de Recherche en Informatique et   *)
//(*  en Automatique.  Distributed only by permission.                   *)
//(*                                                                     *)
//(***********************************************************************)

//(* Module [Buffer]: extensible string buffers *)

//(* This module implements string buffers that automatically expand
//   as necessary.  It provides accumulative concatenation of strings
//   in quasi-linear time (instead of quadratic time when strings are
//   concatenated pairwise). *) 

prefix Buffer {
open   Buffer {

extern t;
//     (* The abstract type of buffers. *)

extern t create(int);
//     (* [create n] returns a fresh buffer, initially empty.
//         The [n] parameter is the initial size of the internal string
//         that holds the buffer contents.  That string is automatically
//         reallocated when more than [n] characters are stored in the buffer,
//         but shrinks back to [n] characters when [reset] is called.
//         For best performance, [n] should be of the same order of magnitude
//         as the number of characters that are expected to be stored in
//         the buffer (for instance, 80 for a buffer that holds one output
//         line).  Nothing bad will happen if the buffer grows beyond that
//         limit, however.  In doubt, take [n = 16] for instance.
//         If [n] is not between 1 and [Sys.max_string_length], it will
//         be clipped to that interval. *)
extern string contents(t);
//     (* Return a copy of the current contents of the buffer.
//        The buffer itself is unchanged. *)
extern void copy_contents(t,string,int);
//     (* Copy the contents of the buffer into the given string at the
//        given offset.  If the string is larger than the contents, add a
//        null terminator.  Throw exception Invalid_arg if the buffer is
//        too small.  The buffer itself is unchanged. *)
extern int length (t);
//     (* Return the number of characters currently contained in the buffer. *)
extern void clear (t);
//     (* Empty the buffer. *)
extern void reset (t);
//     (* Empty the buffer and deallocate the internal string holding the
//        buffer contents, replacing it with the initial internal string
//        of length [n] that was allocated by [create n].
//        For long-lived buffers that may have grown a lot, [reset] allows
//        faster reclaimation of the space used by the buffer. *)
extern void add_char (t, char);
//     (* [add_char b c] appends the character [c] at the end of
//        the buffer [b]. *)
extern void add_string (t,string);
//     (* [add_string b s] appends the string [s] at the end of
//        the buffer [b]. *)
extern void add_substring ( t, string, int, int);
//     (* [add_substring b s ofs len] takes [len] characters from offset
//        [ofs] in string [s] and appends them at the end of the buffer [b]. *)
extern void add_buffer (t, t);
//     (* [add_buffer b1 b2] appends the current contents of buffer [b2]
//        at the end of buffer [b1].  [b2] is not modified. *)

}}
#endif
