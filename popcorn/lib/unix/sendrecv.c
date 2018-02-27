#include "unixsupport.h"
#include "socketaddr.h"

/* All functions herein assume that the caller will make sure this
   stuff is valid (i.e., in the Popcorn library) */

static int msg_flag_table[] = {
  MSG_OOB, MSG_DONTROUTE, MSG_PEEK
};

int unix_recv(int sockfd, string buf, int ofs, int len, array flags) {
  int retcode;
  retcode = recv(sockfd, (buf->chars)+ofs, len,
		 convert_flags(flags,msg_flag_table));
  if (retcode == -1)
    unix_error(__FILE__,__LINE__,"recv");
  return retcode;
}

void *unix_recvfrom(int sockfd, string buf, int ofs, int len, array flags) {
  int retcode;
  void **result;
  union sock_addr_union sa;
  int addr_len = sizeof(sa);

  retcode = recvfrom(sockfd, (buf->chars) + ofs, len, 
		     convert_flags(flags, msg_flag_table),
		     &sa.s_gen, &addr_len);

  if (retcode == -1) 
    unix_error(__FILE__,__LINE__,"recvfrom");
  
  result = xalloc(8);
  result[0] = (void *) retcode;
  result[1] = alloc_sockaddr(&sa);

  return result;
}

int unix_send(int sockfd, string buf, int ofs, int len, array flags) {
  int retcode;

  retcode = send(sockfd, buf->chars + ofs, len, 
             convert_flags(flags, msg_flag_table));
  if (retcode == -1) unix_error(__FILE__,__LINE__,"send");
  return retcode;
}

int unix_sendto(int sockfd, string buf, int ofs, int len, array flags,
		void *dest)
{
  int retcode;
  union sock_addr_union sa;
  int addr_len;

  get_sockaddr(dest, &sa, &addr_len);
  retcode = sendto(sockfd, buf->chars + ofs, len,
		   convert_flags(flags, msg_flag_table),
		   &sa.s_gen, addr_len);
  if (retcode == -1) unix_error(__FILE__,__LINE__,"sendto");
  return retcode;
}
