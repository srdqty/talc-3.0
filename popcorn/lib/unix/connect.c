#include "unixsupport.h"
#include "socketaddr.h"

/*
 *  file_descr * inet_addr -> void
 *
 *  Connect a socket to an address
 */
void unix_connect(int sockfd, void *pop_saddr) {
  int retcode;
  union sock_addr_union sa;
  int addr_len;

  get_sockaddr(pop_saddr,&sa,&addr_len);
  retcode = connect(sockfd, &sa.s_gen, addr_len);
  if (retcode == -1)
    unix_error(__FILE__,__LINE__,"connect");
  return;
}
