
#ifndef __WORKLIST_H
#define __WORKLIST_H

prefix Worklist {
open Worklist;

extern worklist;

extern worklist create(int sz);

/* Add does nothing if the element is already present. */
extern void add(worklist w,int elt);

/* Take the next element off the worklist. Raise Failure if the
   worklist is empty. */
extern int remove(worklist w);

extern bool is_empty(worklist w);

}

#endif
