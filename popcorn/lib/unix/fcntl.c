#include "unixsupport.h"
#include <fcntl.h>

#ifndef O_NONBLOCK
#define O_NONBLOCK O_NDELAY
#endif

void unix_set_nonblock(int fd)
{
  int retcode;
  retcode = fcntl(fd, F_GETFL, 0);
  if (retcode == -1 ||
      fcntl(fd, F_SETFL, retcode | O_NONBLOCK) == -1)
    unix_error(__FILE__,__LINE__,"set_nonblock");
}

void unix_clear_nonblock(int fd)
{
  int retcode;
  retcode = fcntl(fd, F_GETFL, 0);
  if (retcode == -1 ||
      fcntl(fd, F_SETFL, retcode & ~O_NONBLOCK) == -1)
    unix_error(__FILE__,__LINE__,"clear_nonblock");
}
