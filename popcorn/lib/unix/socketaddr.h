#ifndef SOCKETADDR_H
#define SOCKETADDR_H

#include <sys/types.h>
#include <sys/socket.h>		/* AF_UNIX, SOCK_STREAM */
#include <sys/un.h>		/* sockaddr_un */
#include <netinet/in.h>		/* sockaddr_in, INADDR_ANY */
#include <arpa/inet.h>

union sock_addr_union {
  struct sockaddr s_gen;
  struct sockaddr_un s_unix;
  struct sockaddr_in s_inet;
};

/* for accessing the relevant fields of a Popcorn socket addr.  These
   obviously depend on the correct sockaddr Popcorn definition and
   compiler strategy. */
/* assumes 
   union sockaddr {
     string ADDR_UNIX;
     *(int, int) ADDR_INET;
   }
*/
#define UNIX_ADDR 1
#define INET_ADDR 2
#define ADDR_TYPE(sa)      (((int *)sa)[0])
#define ADDR_UNIX(sa)      (((string *)sa)[1])
#define ADDR_INET(sa)      (((void **)sa)[1])
#define ADDR_INET_IP(sa)   (((unsigned int **)sa)[1][0])
#define ADDR_INET_PORT(sa) (((unsigned int **)sa)[1][1])

/* create a Popcorn sockaddr value for the given socket address */
extern void *alloc_sockaddr(union sock_addr_union *sa);

/* return a socket address derived from a Popcorn sockaddr value */
extern void get_sockaddr(void *pop_saddr, union sock_addr_union *sa,
			 int *addr_len);

#endif
