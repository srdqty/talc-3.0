#include <errno.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>


int main() {
  char buf[5] = { 'a', 'a', 'a', 'a', 'a' };

  printf("sizeof(dev_t)=%d\n",sizeof(__dev_t));
  printf("sizeof(gid_t)=%d\n",sizeof(__gid_t));
  printf("sizeof(ino_t)=%d\n",sizeof(__ino_t));
  printf("sizeof(mode_t)=%d\n",sizeof(__mode_t));
  printf("sizeof(nlink_t)=%d\n",sizeof(__nlink_t));
  printf("sizeof(uid_t)=%d\n",sizeof(__uid_t));
  printf("sizeof(off_t)=%d\n",sizeof(__off_t));
  printf("sizeof(unsigned long)=%d\n",sizeof(unsigned long));
  printf("sizeof(time_t)=%d\n",sizeof(__time_t));
  printf("EACCES=%d\n",EACCES);
  printf("ENOTDIR=%d\n",ENOTDIR);
  printf("S_IFDIR=%d\n",S_IFDIR);
  printf("S_IXOTH=%d\n",S_IXOTH);
  printf("EAGAIN=%d\n",EAGAIN);

  strncpy(buf,"hoop",4);
  printf("strncpy does %s add a null terminator\n",
	 buf[4] == 'a' ? "not" : "");

  printf("\\r = %d\n",'\r');
  return 1;
}
