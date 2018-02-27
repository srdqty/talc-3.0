#include "socketaddr.h"
#include "unixsupport.h"

/*
 * file_descriptor * sockaddr -> void
 *
 * Binds a socket to an address.
 */
void unix_bind(int sockfd, void *pop_saddr) {
  int retcode;
  union sock_addr_union sa;
  int addr_len;

  get_sockaddr(pop_saddr,&sa,&addr_len);
  retcode = bind(sockfd, &sa.s_gen, addr_len);
  if (retcode == -1)
    unix_error(__FILE__,__LINE__,"bind");

  return;
}
