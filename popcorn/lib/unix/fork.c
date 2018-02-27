#include "unixsupport.h"

int unix_fork(void) {
  int retcode;

  retcode = fork();
  if (retcode == -1)
    unix_error(__FILE__,__LINE__,"fork");
  return retcode;
}
