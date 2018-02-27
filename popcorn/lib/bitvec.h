#ifndef BITVEC_H
#define BITVEC_H

#include "core.h"
#include "list.h"
#include "dict.h"
#include "set.h"

#define BITVEC <int>array

prefix Bitvec {
open   Bitvec {
extern bool get  (BITVEC bvec, int pos);
extern void set  (BITVEC bvec, int pos);
extern void clear(BITVEC bvec, int pos);
extern bool get_and_set(BITVEC bvec, int pos);

extern void union_two    (BITVEC dest, BITVEC src1, BITVEC src2);
extern void intersect_two(BITVEC dest, BITVEC src1, BITVEC src2);
extern void diff_two     (BITVEC dest, BITVEC src1, BITVEC src2);
extern bool compare_two  (BITVEC src1, BITVEC src2);
extern bool all_set      (BITVEC bvec, int sz);

extern BITVEC new_empty(int sz);
extern BITVEC new_full (int sz);
extern BITVEC new_copy (BITVEC old);
extern void   clear_all(BITVEC old);
extern void   set_all  (BITVEC old);

extern BITVEC resize(BITVEC old,int new_sz); 

extern void clear_above(BITVEC old, int i);
extern void clear_below(BITVEC old, int i);

// Return the next set bit at a position >= pos. Return -1 if no such bit.
extern int next(BITVEC bvec, int pos);
// Return the previous set bit at a position <= pos. Return -1 if no such bit.
extern int prev(BITVEC bvec, int pos);

extern BITVEC from_list<a,b>(<a,b>Dict::dict d,int f(b),int sz,<a>List::list l);
extern <int>List::list to_sorted_list(BITVEC bvec,int sz);

extern void print_bvec(BITVEC);

}}

#endif BITVEC_H
