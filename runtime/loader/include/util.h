#ifndef MODUTILS_UTIL_H
#define MODUTILS_UTIL_H 1

#include "gc.h"

void error(const char *, ...)
#ifdef __GNUC__
  __attribute__((format(printf, 1, 2)))
#endif
  ;

#define xmalloc GC_malloc
#define xrealloc GC_realloc
#define xfree GC_free

#endif /* util.h */
