#include "unixsupport.h"

void unix_chroot(string path) {
  int ret;
  assert(path->chars[path->size] == '\0');
  ret = chroot(path->chars);
  if (ret == -1) 
    unix_error(__FILE__,__LINE__,"chroot");
}
