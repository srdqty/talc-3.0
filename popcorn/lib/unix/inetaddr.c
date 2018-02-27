#include "unixsupport.h"
#include <stdio.h>
#include <netinet/in.h>

static void sprintf_addr(char *buf, unsigned int addr) {
  unsigned int addr_h = ntohl(addr);
  sprintf(buf,"%d.%d.%d.%d", (addr_h) >> 24,
          (addr_h & (0xff << 16)) >> 16,
          (addr_h & (0xff << 8)) >> 8,
          (addr_h & 0xff));
}

string unix_inet_ntoa(int inet_addr) {
  static char buf[16];
  static struct str_internal s = { 16, buf };
  
  sprintf_addr(buf,inet_addr);
  return &s;
}
