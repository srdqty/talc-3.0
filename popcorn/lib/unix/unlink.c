#include "unixsupport.h"

void unix_unlink(string path)
{
  int retcode;
  char *p = convert_pop_string(path);

  retcode = unlink(p);
  if (retcode == -1)
    unix_error(__FILE__,__LINE__,"unlink");
}

