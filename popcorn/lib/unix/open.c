#include "unixsupport.h"
#include <sys/stat.h>
#include <fcntl.h>

static int open_flag_table[] = {
  O_RDONLY, O_WRONLY, O_RDWR, O_CREAT, O_EXCL, O_APPEND, O_TRUNC
};

static int open_mode_flag_table[] = {
  S_IRWXU, S_IREAD, S_IWRITE, S_IEXEC, S_IRUSR, S_IWUSR, S_IXUSR,
  S_IRWXG, S_IRGRP, S_IWGRP, S_IXGRP, S_IRWXO, S_IROTH, S_IWOTH, S_IXOTH
};

int unix_open(string filename, array flags, array mode_flags) {
  int retcode;
  char *p = convert_pop_string(filename);

  if (mode_flags->size == 0)
    retcode = open(p, convert_flags(flags, open_flag_table));
  else
    retcode = open(p, convert_flags(flags, open_flag_table),
		      convert_flags(mode_flags, open_mode_flag_table));
  if (retcode == -1)
    unix_error(__FILE__,__LINE__,"open");
  return retcode;
}
