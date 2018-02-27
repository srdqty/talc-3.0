#include <sys/socket.h>		/* AF_UNIX, SOCK_STREAM */
#include <sys/un.h>		/* sockaddr_un */
#include <netinet/in.h>		/* sockaddr_in, INADDR_ANY */

#define SA struct sockaddr

/* DATAGRAM */
void dgram_echo(int sockfd, SA *cli_addr, int cli_len)
{
  char buff[10];

  /* recvfrom */
  printf("recvfrom: %d\n", recvfrom(sockfd, buff, 8, 0, cli_addr, &cli_len));
  printf("[%.8s (%d)]\n", buff, cli_len);
  
  /* sendto */
  printf("sendto: %d\n", 
	 sendto(sockfd, "backtoyou", 9, 0, cli_addr, 110));
}

void unix_dgram()
{
  int serv_sock;
  struct sockaddr_un serv_addr, cli_addr;
  char buff[10];

  unlink("/tmp/sockpath"); 
  
  /* socket */
  serv_sock = socket(AF_UNIX, SOCK_DGRAM, 0);
  printf("AF_UNIX, SOCK_DGRAM: %d\n", serv_sock);
  
  /* sockaddr */
  bzero(&serv_addr, sizeof(serv_addr));
  serv_addr.sun_family = AF_UNIX;
  strcpy(serv_addr.sun_path, "/tmp/sockpath");
  
  /* bind */
  printf("bind: %d\n", bind(serv_sock, &serv_addr, sizeof(serv_addr)));

  dgram_echo(serv_sock, (SA *) &cli_addr, sizeof(cli_addr));
}

void inet_dgram()
{
  int serv_sock;
  struct sockaddr_in serv_addr, cli_addr;

  /* socket */
  serv_sock = socket(AF_INET, SOCK_DGRAM, 0);
  printf("AF_INET, SOCK_DGRAM: %d\n", serv_sock);

  /* sockaddr */
  bzero(&serv_addr, sizeof(serv_addr));
  serv_addr.sin_family = AF_INET;
  serv_addr.sin_addr.s_addr = INADDR_ANY;
  serv_addr.sin_port = htons(4444);
  
  /* bind */
  printf("bind: %d\n", bind(serv_sock, &serv_addr, sizeof(serv_addr)));

  dgram_echo(serv_sock, (SA *) &cli_addr, sizeof(cli_addr));
}

/* STREAM */
stream_echo(int sockfd, SA *serv_addr, int serv_len, 
   	                SA *cli_addr,  int cli_len)
{
  int accept_sock;
  char buff[10];

  /* bind */
  printf("bind: %d\n", bind(sockfd, serv_addr, serv_len));

  /* listen */
  printf("listen: %d\n", listen(sockfd, 15));

  /* accept */
  accept_sock = accept(sockfd, cli_addr, &cli_len);
  printf("accept: %d\n", accept_sock); 

  /* receive */
  printf("recv: %d\n", recv(accept_sock, buff, 8, 0));
  printf("[%.8s]\n", buff);

  /* send */
  send(accept_sock, "backtoyou", 9, 0);
}

void unix_stream()
{
  int listen_sock;
  struct sockaddr_un serv_addr, cli_addr;

  unlink("/tmp/sockpath"); 
  
  /* socket */
  listen_sock = socket(AF_UNIX, SOCK_STREAM, 0);
  printf("AF_UNIX, SOCK_STREAM: %d\n", listen_sock);

  /* sockaddr */
  bzero(&serv_addr, sizeof(serv_addr));
  serv_addr.sun_family = AF_UNIX;
  strcpy(serv_addr.sun_path, "/tmp/sockpath");
  
  stream_echo(listen_sock, (SA *) &serv_addr, sizeof(serv_addr),
	                (SA *) &cli_addr,  sizeof(cli_addr));
}

void inet_stream()
{
  int listen_sock;
  struct sockaddr_in serv_addr, cli_addr;
  char buff[10];

  /* socket */
  listen_sock = socket(AF_INET, SOCK_STREAM, 0);
  printf("AF_INET, SOCK_STREAM: %d\n", listen_sock);
  
  /* sockaddr */
  bzero(&serv_addr, sizeof(serv_addr));
  serv_addr.sin_port = htons(4444);
  serv_addr.sin_addr.s_addr = INADDR_ANY;
  serv_addr.sin_family = AF_INET;

  stream_echo(listen_sock, (SA *) &serv_addr, sizeof(serv_addr),
	                (SA *) &cli_addr,  sizeof(cli_addr));
}

/* MAIN */

int main( int argc, char *argv[] )
{
  if (argc != 2)
    printf("Must provide exactly 1 argument\n"), exit(255);

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
