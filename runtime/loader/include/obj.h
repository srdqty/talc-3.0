/* Object file loading and relocation routines. */

#ifndef __OBJ_H__
#define __OBJ_H__ 1

#ifndef WINDOWS
#include "elf_obj.h"
#else
#include "coff_obj.h"
#endif

#endif /* obj.h */
