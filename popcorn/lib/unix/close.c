#include "unixsupport.h"

void unix_close(int fd) {
  int ret = close(fd);
  if (ret == -1) 
    unix_error(__FILE__,__LINE__,"close");
}
