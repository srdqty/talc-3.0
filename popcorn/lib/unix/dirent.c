#include "unixsupport.h"
#include <dirent.h>

DIR *unix_opendir(string name) {
  char *Cname = convert_pop_string(name);
  DIR *ret = opendir(Cname);
  if (ret == NULL) {
    unix_error(__FILE__,__LINE__,"opendir");
  }
  return ret;
}

void unix_closedir(DIR *dirp) {
  if (dirp == NULL)
    nullpointer_exn(__FILE__,__LINE__);
  if (closedir(dirp) != 0)
    unix_error(__FILE__,__LINE__,"closedir");
  return;
}

struct pop_dirent {
  int d_ino;
  int d_off;
  short d_reclen;
  string d_name;
};

int unix_readdir(DIR *dirp,struct pop_dirent *de) {
  struct dirent *ret;
  if (dirp == NULL || de == NULL)
    nullpointer_exn(__FILE__,__LINE__);
  ret = readdir(dirp);
  if (ret != NULL) {
    de->d_ino = ret->d_ino;
    de->d_off = ret->d_off;
    de->d_reclen = ret->d_reclen;
    de->d_name = Cstring_to_string(ret->d_name);
    return(1);
  } else {
    return(0);
  }
}

void unix_rewinddir(DIR *dirp) {
  if (dirp == NULL)
    nullpointer_exn(__FILE__,__LINE__);
  rewinddir(dirp);
}

void unix_seekdir(DIR *dirp, int pos) {
  if (dirp == NULL)
    nullpointer_exn(__FILE__,__LINE__);
  seekdir(dirp,pos);
}

int unix_telldir(DIR *dirp) {
  if (dirp == NULL)
    nullpointer_exn(__FILE__,__LINE__);
  return(telldir(dirp));
}

int unix_dirfd(DIR *dirp) {
  if (dirp == NULL)
    nullpointer_exn(__FILE__,__LINE__);
  return(dirfd(dirp));
}

