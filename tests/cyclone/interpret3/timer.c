#ifdef __linux__

#include <sys/time.h>
#include <sys/resource.h>

/*
 *  tv_sec and tv_usec are of type time_t <timebits.h>
 */
int unix_time()
{
  struct rusage self;

  getrusage(RUSAGE_SELF, &self);
  return (self.ru_utime.tv_sec * 1000) + (self.ru_utime.tv_usec / 1000);
}
#else

int unix_time()
{
  return 0;
}

#endif
