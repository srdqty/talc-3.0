#include "unixsupport.h"
#include <errno.h>

#define MAX_ARGS 100

void unix_execv(string path, array args, int numargs) 
{
  char *Cargs[MAX_ARGS];
  char *Cpath;
  int i, ret_code;
  
  if (numargs > args->size ||
      numargs > (MAX_ARGS-1))
    raise_pop_exception(make_unix_error(__FILE__,__LINE__,
					EINVAL, "execv"));
  Cpath = convert_pop_string(path);
  for (i=0; i<numargs; i++)
    Cargs[i] = convert_pop_string(((string *)(args->elts))[i]);
  Cargs[i] = NULL;

  ret_code = execv(Cpath, Cargs);
  /* if we got here there is definitely an error */
  if (ret_code == -1)
    unix_error(__FILE__,__LINE__,"execv");
}
