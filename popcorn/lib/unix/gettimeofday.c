#include "unixsupport.h"
#include <sys/time.h>

void unix_gettimeofday(struct timeval *tvp) {
  if (gettimeofday(tvp, NULL) == -1) 
    unix_error(__FILE__,__LINE__,"gettimeofday");
}

int unix_time() {
  time_t t;
  if ((t = time(NULL)) == -1) 
    unix_error(__FILE__,__LINE__,"time");
  return (int)t;
}
