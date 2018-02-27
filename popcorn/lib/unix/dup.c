#include "unixsupport.h"

void unix_dup2(int orig_fd, int new_fd) {
  int ret_code;
  ret_code = dup2(orig_fd, new_fd);
  if (ret_code == -1)
    unix_error(__FILE__,__LINE__,"dup2");
}

int unix_dup(int orig_fd) {
  int new_fd;
  new_fd = dup(orig_fd);
  if (new_fd == -1)
    unix_error(__FILE__,__LINE__,"dup2");
  return new_fd;
}
