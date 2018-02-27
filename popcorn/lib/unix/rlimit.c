#include "unixsupport.h"
#include <sys/resource.h>

/* All functions herein assume that the caller will make sure this
   stuff is valid (i.e., in the Popcorn library) */

static int rlimit_resource_table[] = {
       RLIMIT_CPU,     /* CPU time in seconds */
       RLIMIT_FSIZE,   /* Maximum filesize */
       RLIMIT_DATA,    /* max data size */
       RLIMIT_STACK,   /* max stack size */
       RLIMIT_CORE,    /* max core file size */
       RLIMIT_RSS,     /* max resident set size */
       RLIMIT_NPROC,   /* max number of processes */
       RLIMIT_NOFILE,  /* max number of open files */
       RLIMIT_MEMLOCK  /* max locked-in-memory address space*/
};

void unix_getrlimit(int rlimit_resource, struct rlimit *res) {
  int retcode;
  retcode = getrlimit(rlimit_resource_table[rlimit_resource-1],res);
  if (retcode == -1)
    unix_error(__FILE__,__LINE__,"getrlimit");
}

void unix_setrlimit(int rlimit_resource, struct rlimit *res) {
  int retcode;
  retcode = setrlimit(rlimit_resource_table[rlimit_resource-1],res);
  if (retcode == -1)
    unix_error(__FILE__,__LINE__,"setrlimit");
}
