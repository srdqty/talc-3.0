#include "unixsupport.h"
#include <time.h>
#include <errno.h>

struct pop_tm {
  int sec;
  int min;
  int hour;
  int day;
  int mon;
  int year;
  int wday;
  int yday;
  int isdst;
};

static struct pop_tm *alloc_tm(struct tm *tm)
{
  struct pop_tm *res;
  res = xalloc_atomic(sizeof(struct pop_tm));
  /* make sure memory alignment is correct */
  assert(((int)&res->isdst - (int)res) == 
	 ((int)&tm->tm_isdst - (int)tm));
  memcpy(res,tm,sizeof(struct pop_tm));
  return res;
}

struct pop_tm *unix_gmtime(int clock)
{
  struct tm * tm;
  tm = gmtime((time_t *)&clock);
  if (tm == NULL) 
    raise_pop_exception(make_unix_error(__FILE__,__LINE__,EINVAL, "gmtime"));
  return alloc_tm(tm);
}
