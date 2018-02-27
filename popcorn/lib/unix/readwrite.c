#include "unixsupport.h"
#include <errno.h>

int unix_read(int fd, string buf, int ofs, int len)
{
  int ret;

  ret = read(fd, (buf->chars) + ofs, len);
  if (ret == -1) 
    unix_error(__FILE__,__LINE__,"read");
  return ret;
}

int unix_write(int fd, string buf, int ofs, int len)
{
  int written;
  int ret;

  written = 0;
  while (len > 0) {
    ret = write(fd, buf->chars+ofs, len);
    if (ret == -1) {
      if ((errno == EAGAIN || errno == EWOULDBLOCK) && written > 0) break;
      unix_error(__FILE__,__LINE__,"write");
    }    
    written += ret;
    ofs += ret;
    len -= ret;
  }
  return written;
}
