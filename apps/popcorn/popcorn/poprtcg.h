// Some useful RTCG functions

#ifndef POPRTCG_H
#define POPRTCG_H

#include "popil.h"

prefix Poprtcg {
open Poprtcg;
open Popil;

// Removes a block only in the sense that it cleans up the generator of this
// function and the template containing this block, but does not remove
// the block from the function.
extern void remove_block(cf_block b);

extern void replace_block(cf_block b_old, cf_block b_new);

extern void merge_block(cf_block b, cf_block m);

}

#endif
