#include "unixsupport.h"
#include "socketaddr.h"
#include <netinet/tcp.h>

#ifndef SO_BROADCAST
#define SO_BROADCAST (-1)
#endif
#ifndef SO_OOBINLINE
#define SO_OOBINLINE (-1)
#endif

static int sockopt[] = {
  SO_DEBUG, SO_BROADCAST, SO_REUSEADDR, SO_KEEPALIVE,
  SO_DONTROUTE, SO_OOBINLINE, SO_SNDBUF, TCP_NODELAY /* , SO_LINGER */ };

int unix_getsockopt(int fd, int flag)
{
  int optval;
  int optsize = sizeof(optval);
  int option = sockopt[flag-1];
  int proto = SOL_SOCKET;

  if (option == TCP_NODELAY)
    proto = IPPROTO_TCP;

  if (getsockopt(fd, proto, option,
                 (void *) &optval, &optsize) == -1)
    unix_error(__FILE__,__LINE__,"getsockopt");
  return optval;
}

void unix_setsockopt(int fd, int flag, int optval)
{
  int proto = SOL_SOCKET;
  int option = sockopt[flag-1];

  if (option == TCP_NODELAY)
    proto = IPPROTO_TCP;

  if (setsockopt(fd, proto, option, 
                 (void *) &optval, sizeof(optval)) == -1)
    unix_error(__FILE__,__LINE__,"setsockopt");
}
