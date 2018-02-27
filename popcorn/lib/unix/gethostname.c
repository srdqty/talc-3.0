#include "unixsupport.h"

#ifndef MAXHOSTNAMELEN
#define MAXHOSTNAMELEN 256
#endif

/* Return the name of the local host */
string unix_gethostname(void)
{
  char name[MAXHOSTNAMELEN];
  
  if (gethostname(name, MAXHOSTNAMELEN) == -1)
    unix_error(__FILE__,__LINE__,"gethostname");

  return(Cstring_to_string(name));
}

