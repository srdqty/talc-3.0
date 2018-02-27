#include "socketaddr.h"
#include "unixsupport.h"
#include <errno.h>

/* Assumes the following Popcorn definitions:

extern union socket_domain {
  void PF_UNIX;
  void PF_INET;
}

extern union socket_type {
  void SOCK_STREAM;
  void SOCK_DGRAM;
  void SOCK_RAW;
  void SOCK_SEQPACKET;
}

*/
int socket_domain_table[] = { PF_UNIX, PF_INET };
int socket_type_table[] = { SOCK_STREAM, SOCK_DGRAM, 
			    SOCK_RAW, SOCK_SEQPACKET };

int unix_socket(int sock_dom, int sock_type, int protocol)
{
  int sd = socket(socket_domain_table[sock_dom-1],
		  socket_type_table[sock_type-1], protocol);
  if (sd < 0)
    unix_error(__FILE__,__LINE__,"socket");
  return sd;
}

void unix_socketpair(int sock_dom, int sock_type, int protocol, array ret)
{
  int sv[2];
  int ret_code;

  /* make sure the return value is big enough */
  if (ret->size < 2)
    raise_pop_exception(make_unix_error(__FILE__,__LINE__,
					EINVAL, "socketpair"));

  ret_code = socketpair(socket_domain_table[sock_dom-1],
			socket_type_table[sock_type-1], 
			protocol, sv);
  if (ret_code == -1)
    unix_error(__FILE__,__LINE__,"socket");
  ((int *)(ret->elts))[0] = sv[0];
  ((int *)(ret->elts))[1] = sv[1];
  return;
}
