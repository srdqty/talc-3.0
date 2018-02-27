#include "unixsupport.h"
#include <sys/wait.h>

/* assumes

  union process_status {
    int WEXITED;
    int WSIGNALED;
    int WSTOPPED;
  }

  with numbering starting at 1.
*/

#define TAG_WEXITED 1
#define TAG_WSIGNALED 2
#define TAG_WSTOPPED 3

struct process_status {
  int tag;
  int val;
};

struct result {
  int pid;
  struct process_status *val;
};

static struct result *alloc_process_status(int pid, int status)
{
  struct process_status *st;
  struct result *res;

  if (WIFEXITED(status)) {
    st = xalloc(sizeof(struct process_status));
    st->tag = TAG_WEXITED;
    st->val = WEXITSTATUS(status);
  }
  else if (WIFSTOPPED(status)) {
    st = xalloc(sizeof(struct process_status));
    st->tag = TAG_WEXITED;
    st->val = WSTOPSIG(status);
  }
  else {
    st = xalloc(sizeof(struct process_status));
    st->tag = TAG_WEXITED;
    st->val = WTERMSIG(status);
  }
  res = xalloc(sizeof(struct result));
  res->pid = pid;
  res->val = st;
  return res;
}

static int wait_flag_table[] = {
  WNOHANG, WUNTRACED
};

struct result *unix_waitpid(array flags, int pid_req)
{
  int pid, status;
  
  pid = waitpid(pid_req, &status, 
                convert_flags(flags, wait_flag_table));
  if (pid == -1) 
    unix_error(__FILE__,__LINE__,"waitpid");
  return alloc_process_status(pid, status);
}

