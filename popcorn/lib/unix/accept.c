#include "socketaddr.h"
#include "unixsupport.h"

/*
 *  file_descr -> (file_descr * sockaddr)
 *
 *    Accept connections on the given socket. The returned descriptor
 *    is a socket connected to the client; the returned address is the
 *    address of the connecting client.
 */
void *unix_accept(int fd)
{
  int retcode;
  union sock_addr_union sa;
  int sa_len;
  void **result;
  
  sa_len = sizeof(sa);
  retcode = accept(fd, &sa.s_gen, &sa_len);
  if (retcode < 0)
    unix_error(__FILE__,__LINE__,"accept");
    
  result = xalloc(8);
  result[0] = (void *) retcode;
  result[1] = alloc_sockaddr(&sa);

  return result;
}
