#include "unixsupport.h"

string unix_readlink(string path)
{
  char buf[5000];
  char *p = convert_pop_string(path);
  int retcode;
  retcode = readlink(p, buf, sizeof(buf));
  if (retcode == -1)
    unix_error(__FILE__,__LINE__,"getlink");
  buf[retcode] = '\0';
  return Cstring_to_string(buf);
}
