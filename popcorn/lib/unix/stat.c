#include "unixsupport.h"
#include <sys/stat.h>

struct pop_stat {
  int st_mode;     /* protection */
  int st_uid;      /* user ID of owner */
  int st_gid;      /* group ID of owner */
  int st_size;     /* total size, in bytes */
  int st_atime;    /* time of last access */
  int st_mtime;    /* time of last modification */
  int st_ctime;    /* time of last change */
};

void unix_stat(string path, struct pop_stat *psb) {
  int retcode;
  struct stat sb;
  char *p = convert_pop_string(path);

  retcode = stat(p,&sb);
  if (retcode == -1)
    unix_error(__FILE__,__LINE__,"stat");

  psb->st_mode = sb.st_mode;
  psb->st_uid = sb.st_uid;
  psb->st_gid = sb.st_gid;
  psb->st_size = sb.st_size;
  psb->st_atime = sb.st_atime;
  psb->st_mtime = sb.st_mtime;
  psb->st_ctime = sb.st_ctime;
}
