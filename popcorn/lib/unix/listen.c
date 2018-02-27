#include "unixsupport.h"
#include <sys/socket.h>

void unix_listen(int fd, int backlog) {
  int retcode = listen(fd, backlog);
  if (retcode < 0)
    unix_error(__FILE__,__LINE__,"listen");
}
