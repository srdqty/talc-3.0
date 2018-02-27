#ifndef TAL_SOCKETS_H
#define TAL_SOCKETS_H

/* #include "list.h" */
/* extern ?struct <a>mylist { a hd; <a>mylist tl; } */

/* Internet addresses (is this how abstract data types work???) */
/* is this needed??? */
/*
extern inet_addr;
extern inet_addr INET_ADDR_ANY;
*/

#define	TAL_INADDR_ANY	(0)

extern union socket_domain {
  void PF_UNIX;			/* internal value 1 */
  void PF_INET;			/* internal value 2 */
}

extern union socket_type {
  void SOCK_STREAM;
  void SOCK_DGRAM;
  void SOCK_RAW;
  void SOCK_SEQPACKET;
}

extern union sockaddr {
  string ADDR_UNIX;
  *(int, int) ADDR_INET;	/* (addr, port) */
  void ADDR_NULL;
}

extern FILE?;

extern int               tal_send(FILE, string);
extern int               tal_recv(FILE, string, int, int);
extern int               tal_sendto(FILE, string, sockaddr);
extern *(int, sockaddr)  tal_recvfrom(FILE, string, sockaddr);

extern FILE              tal_socket(socket_domain, socket_type, int);
extern void              tal_bind(FILE, sockaddr);
extern void              tal_listen(FILE, int);
extern *(FILE, sockaddr) tal_accept(FILE);
extern void              tal_connect(FILE, sockaddr);
extern void              tal_close(FILE);
extern void              tal_unlink(string s);

extern FILE tal_select(FILE [], int, int, int) [];

extern string tal_gethostname();
extern void   print_sockaddr(sockaddr);
extern string tal_tmpnam();
extern int    tal_gethostbyname(string);
extern string tal_inaddr_to_dot(int);
extern void   tal_signal(int, void f());
extern int    fd2int(FILE);

/*
 *  sockets.pop
 */

extern string sock_read(FILE);
extern void sock_send(FILE, string);

#endif
