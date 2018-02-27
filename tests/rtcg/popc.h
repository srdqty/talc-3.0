#ifndef __POPC_H
#define __POPC_H

/* Routines used to make C code use Popcorn's data representations
   and check array bounds. */

extern void *GC_malloc(int);

// WARNING: POP_ARRAY only works for T being a single identifier or keyword.
#define POP_ARRAY_DEF(T) typedef struct { int size; T *elts; } pop_##T##_array_s, *pop_##T##_array;
#define POP_ARRAY(T) pop_##T##_array
#define POP_ARRAY_S(T) pop_##T##_array_s

POP_ARRAY_DEF(int)
POP_ARRAY_DEF(float)
POP_ARRAY_DEF(double)

int array_bounds_error() {
  printf("array bounds error.\n");
  exit(1);
  return 1;
}

/* Make sure no argument to these routines has side effect!  The arguments
   are often duplicated!! */
#ifdef POP
#   define PONLY(X) X
#   define CONLY(X)
#   define ARRAY_BOUNDS(X,Y) (((X)->size <= (Y)) ? array_bounds_error () : (Y))
#   define ARR_TYP(T,X) POP_ARRAY(T) X
#   define ARR_DECL(T,X,L,D) \
  T *_i0_##X = (T *)GC_malloc(sizeof(T) * L); \
  POP_ARRAY(T) _i1_##X = (POP_ARRAY(T))GC_malloc(sizeof(POP_ARRAY_S(T))); \
  POP_ARRAY(T) X = (_i1_##X->size = L, _i1_##X->elts=_i0_##X, _i1_##X)
#   define ARR_SET_OP(X,Y,OP,W) (((X)->elts)[ARRAY_BOUNDS(X,Y)] OP W)
#   define ARR_SET(X,O,R) ARR_SET_OP(X,O,=,R)
#   define ARR_GET(X,Y) ((X)->elts[ARRAY_BOUNDS(X,Y)])
#   define ARR_SIZE(X) X->size
#   define ARR_BODY(X) X->elts
#   define ARR_SIZE_DECL(T,X,L) T X##__1[L]; POP_ARRAY_S(T) X##__2 = {L, X##__1}; POP_ARRAY(T) X = &X##__2
#else
#   define PONLY(X)
#   define CONLY(X) X 
#   define ARR_TYP(T,X) T *X
#   define ARR_DECL(T,X,L,D) T *X = (T *)malloc(sizeof(T) * L)
#   define ARR_SET_OP(X,Y,OP,W) X[Y] OP W
#   define ARR_SET(X,O,R) X[O] = R
#   define ARR_GET(X,O) X[O]
#   define ARR_SIZE(X) UNIMPLEMENTED
#   define ARR_SIZE_DECL(T,X,L) T X[L]
#endif

#endif
