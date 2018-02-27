#include "unixsupport.h"

void unix_setuid(int uid)
{
  if (setuid(uid) == -1) unix_error(__FILE__,__LINE__,"setuid");
}
