#include "unixsupport.h"
#include <errno.h>
#include <sys/uio.h>

struct pop_iovec {
  string buf;
  int ofs;
  int len;
};

#define MAX_BUFS 100

int unix_writev(int fd, array pop_iovecs, int num_bufs)
{
  int i, ret;
  static struct iovec bufs[MAX_BUFS];
  static struct iovec *iovecs = bufs;
  static int num_iovecs = MAX_BUFS;

  /* make sure the user-specified number of bufs < size(array) */
  if (num_bufs > pop_iovecs->size)
    raise_pop_exception(make_unix_error(__FILE__,__LINE__,
					  EINVAL,"writev"));

  /* copy the Poporn buf information into the iovecs */
  if (num_bufs > num_iovecs) {
    iovecs = xalloc(sizeof(struct iovec) * num_iovecs * 2);
    num_iovecs *= 2;
  }
  for (i=0; i<num_bufs; i++) {
    struct pop_iovec *p = ((struct pop_iovec **)pop_iovecs->elts)[i];
    if (p->ofs >= 0 && p->len >= 0 && p->ofs + p->len <= p->buf->size) {
      iovecs[i].iov_base = (void *)(p->buf->chars + p->ofs);
      iovecs[i].iov_len = p->len;
    }
    else
      raise_pop_exception(make_unix_error(__FILE__,__LINE__,
					  EINVAL,"writev"));
  }

  /* call writev */
  ret = writev(fd, iovecs, num_bufs);
  if (ret == -1)
    unix_error(__FILE__,__LINE__,"writev");
  return ret;
}
