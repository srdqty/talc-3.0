
#ifndef POPCONFIG_H
#define POPCONFIG_H

// please wrap each define in an ifndef so it can be set from the command-line
// (or Makefile)

// true means the tapp on calls uses stack slices and tails to instantiate
// the stack
// false should mean don't use the hack even for callee-save registers,
// but currently the IL->TAL doesn't track the contents of callee-save
//  registers, so that's not a possibility
#ifndef USE_DANS_HACK
#define USE_DANS_HACK true
#endif

// if leaving the type off a label would require a block to be verified
// this many times, then don't leave the type off
// zero means types on all labels.
// with jump trees, we for simplicity leave types off nodes
//   of the trees regardless of the threshold
#ifndef LABEL_OFF_THRESHOLD
#define LABEL_OFF_THRESHOLD 4
#endif

// true means callee save EBX, ESI, EDI and this is (of course) reflected
// in function types
#ifndef USE_CALLEE_SAVE
#define USE_CALLEE_SAVE true
#endif

// when a switch jump tree node has this many nodes or fewer, do a linear
// case switch.  Value must be no less than 2
#ifndef JUMP_TREE_THRESHOLD
#define JUMP_TREE_THRESHOLD 10
#endif

#endif
