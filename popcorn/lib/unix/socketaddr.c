#include "unixsupport.h"
#include "socketaddr.h"

/* Converts a UNIX sockaddr struct to a Popcorn one.  We assume that 
   the Popcorn struct declaration for sockaddr is:

   extern union sockaddr {
     string ADDR_UNIX;
     *(int, int) ADDR_INET;
   }

   and that ADDR_UNIX gets compiled to a 1, and ADDR_INET is a 2.
*/

void *alloc_sockaddr(union sock_addr_union *sa) {
  void **result;

  switch(sa->s_gen.sa_family) {
#ifdef __linux__
  case PF_UNIX:
    result = xalloc(8);
    ADDR_TYPE(result) = UNIX_ADDR;
    ADDR_UNIX(result) = (void *)Cstring_to_string(sa->s_unix.sun_path);
    break;
#endif
  case PF_INET: {
    void **inet_addr;
    result = xalloc(8);
    inet_addr = xalloc_atomic(8);
    inet_addr[0] = (void *) sa->s_inet.sin_addr.s_addr;
    inet_addr[1] = (void *) (int) ntohs(sa->s_inet.sin_port);
    ADDR_TYPE(result) = INET_ADDR;
    ADDR_INET(result) = (void *)inet_addr;
    break;
  }
  default:
    result = NULL;
    unix_error(__FILE__,__LINE__,"bad family type");
  }
  return result;
}

/* Converts a Popcorn sockaddr to a UNIX one */

void get_sockaddr(void *pop_saddr, 
		  union sock_addr_union *sa /* out */,
		  int *addr_len /* out */) {
  switch (ADDR_TYPE(pop_saddr)) {
#ifdef __linux__
  case UNIX_ADDR: {
    int len;
    len = ADDR_UNIX(pop_saddr)->size;
    if (len >= sizeof(((struct sockaddr_un *)sa)->sun_path)) {
      unix_error(__FILE__,__LINE__,"invalid path");
    }
    sa->s_gen.sa_family = PF_UNIX;
    memcpy(sa->s_unix.sun_path, ADDR_UNIX(pop_saddr)->chars, (int)len);
    sa->s_unix.sun_path[len] = '\0';
    *addr_len =
      ((char *)&(sa->s_unix.sun_path) - (char *)&(sa->s_unix))
      + len;
    break;
  }
#endif
  case INET_ADDR:
    sa->s_gen.sa_family = PF_INET;
    sa->s_inet.sin_addr.s_addr = ADDR_INET_IP(pop_saddr);
    sa->s_inet.sin_port = htons(ADDR_INET_PORT(pop_saddr));
    *addr_len = sizeof(struct sockaddr_in);
    break;
  default:
    unix_error(__FILE__,__LINE__,"bad family type");
  }
  return;
}
