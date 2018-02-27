#include <sys/socket.h>		/* AF_UNIX, SOCK_STREAM */
#include <sys/un.h>		/* sockaddr_un */
#include <netinet/in.h>		/* sockaddr_in, INADDR_ANY */
#include <netdb.h>		/* hostinfo */
#include <stdio.h>

#define SA struct sockaddr

/* DATAGRAM */

void dgram_echo(int sockfd, SA *serv_addr, int serv_len)
{
  char buff[10];

  /* sendto */
  printf("sendto: %d\n", 
	 sendto(sockfd, "thisthat", 8, 0, serv_addr, serv_len));

  /* recvfrom */
  printf("recvfrom: %d\n", recvfrom(sockfd, buff, 9, 0, NULL, NULL));
  printf("client [%.9s]\n", buff);
}

void unix_dgram()
{
  int cli_sock;
  struct sockaddr_un serv_addr, cli_addr;
  char buff[10];

  /* socket */
  cli_sock = socket(AF_UNIX, SOCK_DGRAM, 0);
  printf("AF_UNIX, SOCK_DGRAM: %d\n", cli_sock);

  /* *** NOTE *** bind needed for UNIX dgram (not for INET dgram) */
  bzero(&cli_addr, sizeof(cli_addr));
  cli_addr.sun_family = AF_UNIX;
  strcpy(cli_addr.sun_path, tmpnam(NULL));

  bind(cli_sock, &cli_addr, sizeof(cli_addr));

  /* sockaddr */
  bzero(&serv_addr, sizeof(serv_addr));
  serv_addr.sun_family = AF_UNIX;
  strcpy(serv_addr.sun_path, "/tmp/sockpath");

  dgram_echo(cli_sock, (SA *) &serv_addr, sizeof(serv_addr));
}

inet_dgram()
{
  int cli_sock;
  struct sockaddr_in serv_addr;
  char buff[10];
  struct hostent *host_info, *tmp;
  struct in_addr *host_ptr;
  
  /* socket */
  cli_sock = socket(AF_INET, SOCK_DGRAM, 0);
  printf("AF_INET, SOCK_DGRAM: %d\n", cli_sock);

  /* sockaddr */
  bzero(&serv_addr, sizeof(serv_addr));
  host_info = gethostbyname("cyclone.cis.upenn.edu");

  host_ptr = (struct in_addr *) host_info->h_addr_list[0];
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_port = htons(4444);
  serv_addr.sin_addr.s_addr = host_ptr->s_addr;

  tmp = gethostbyname("cyclone.cis.upenn.edu");
  printf("<%x>\n", ((struct in_addr *) (tmp->h_addr_list[0]))->s_addr);
  printf("<%s>\n", tmp->h_name);
  fflush(stdout);
  tmp = gethostbyname("cyclone");
  printf("[%x]\n", ((struct in_addr *) (tmp->h_addr_list[0]))->s_addr);
  printf("[%s]\n", tmp->h_name);
  fflush(stdout);
  tmp = gethostbyname("158.130.6.111");
  printf("<%x>\n", ((struct in_addr *) (tmp->h_addr_list[0]))->s_addr);
  printf("<%s>\n", tmp->h_name);
  fflush(stdout);
  
  dgram_echo(cli_sock, (SA *) &serv_addr, sizeof(serv_addr));
}


/* STREAM */

stream_echo(int sockfd, SA *serv_addr, int serv_len)
{
  char buff[10];

  /* connect */
  printf("connect: %d\n", connect(sockfd, serv_addr, serv_len));

  /* send */
  printf("send: %d\n", send(sockfd, "thisthat", 8, 0));

  /* receive */
  printf("recv: %d\n", recv(sockfd, buff, 9, 0));
  printf("client [%.9s]\n", buff);
}

unix_stream()
{
  int cli_sock;
  struct sockaddr_un serv_addr;

  /* socket */
  cli_sock = socket(AF_UNIX, SOCK_STREAM, 0);
  printf("AF_UNIX, SOCK_STREAM: %d\n", cli_sock);

  /* sockaddr */
  bzero(&serv_addr, sizeof(serv_addr));
  serv_addr.sun_family = AF_UNIX;
  strcpy(serv_addr.sun_path, "/tmp/sockpath");

  stream_echo(cli_sock, (SA *) &serv_addr, sizeof(serv_addr));
}

inet_stream()
{
  int cli_sock;
  struct sockaddr_in serv_addr;
  struct hostent *host_info;
  struct in_addr *host_ptr;
  
  /* socket */
  cli_sock = socket(AF_INET, SOCK_STREAM, 0);
  printf("AF_INET, SOCK_STREAM: %d\n", cli_sock);
  
  /* sockaddr */
  bzero(&serv_addr, sizeof(serv_addr));
  host_info = gethostbyname("cyclone.cis.upenn.edu");
  host_ptr = (struct in_addr *) host_info->h_addr_list[0];
  serv_addr.sin_family=AF_INET;
  serv_addr.sin_port = htons(4444);
  serv_addr.sin_addr.s_addr = host_ptr->s_addr;
  
  stream_echo(cli_sock, (SA *) &serv_addr, sizeof(serv_addr));
}

/* MAIN */

int main( int argc, char *argv[] )
{
  if (argc != 2)
    printf("Must provide exactly one argument\n"), exit(255);

  if (argv[1][0] == '1')
    unix_stream();
  else if (argv[1][0] == '2')
    inet_stream();
  else if (argv[1][0] == '3')
    unix_dgram();
  else if (argv[1][0] == '4')
    inet_dgram();
  else
    printf("Argument must be 1, 2, 3, or 4\n"), exit(255);
}
