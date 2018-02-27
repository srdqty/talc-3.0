#include "socketaddr.h"
#include "unixsupport.h"
#include <sys/select.h>

int unix_fd_setsize = FD_SETSIZE;

fd_set *unix_empty_set(void) {
  fd_set *set = (fd_set *)xalloc(sizeof(fd_set));
  return set;
}

void unix_fd_clr(int fd, fd_set *set) {
  if (set == NULL)
    nullpointer_exn(__FILE__,__LINE__);
  FD_CLR(fd,set);
}

int unix_fd_isset(int fd, fd_set *set) {
  if (set == NULL)
    nullpointer_exn(__FILE__,__LINE__);
  return FD_ISSET(fd,set);
}

void unix_fd_set(int fd, fd_set *set) {
  if (set == NULL)
    nullpointer_exn(__FILE__,__LINE__);
  FD_SET(fd,set);
}

void unix_fd_zero(fd_set *set) {
  if (set == NULL)
    nullpointer_exn(__FILE__,__LINE__);
  FD_ZERO(set);
}

void unix_copy_fd_set(fd_set *dst, fd_set *src) {
  if (dst == NULL || src == NULL)
    nullpointer_exn(__FILE__,__LINE__);
  memcpy(dst, src, sizeof(*src));
}

int unix_select(int maxfd, fd_set *readfds, fd_set *writefds, 
		fd_set *exceptfds, struct timeval *timeout)
{
  int retcode;

  if (maxfd < 0 || maxfd > FD_SETSIZE) maxfd = FD_SETSIZE;
  retcode = select(maxfd, readfds, writefds, exceptfds, timeout);
  if (retcode == -1) unix_error(__FILE__,__LINE__,"select");
  return retcode;
}
