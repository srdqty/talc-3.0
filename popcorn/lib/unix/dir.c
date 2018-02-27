#include "unixsupport.h"
#include <sys/param.h>

#ifndef PATH_MAX
#ifdef MAXPATHLEN
#define PATH_MAX MAXPATHLEN
#else
#define PATH_MAX 512
#endif
#endif

string unix_getcwd()
{
  char buff[PATH_MAX];
  if (getcwd(buff, sizeof(buff)) == 0) 
    unix_error(__FILE__,__LINE__,"getcwd");
  return Cstring_to_string(buff);
}

void unix_chdir(string path) {
  char *Cpath = convert_pop_string(path);
  int ret = chdir(Cpath);
  if (ret == -1) 
    unix_error(__FILE__,__LINE__,"chdir");
  return;
}
