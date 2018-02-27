#include "unixsupport.h"
#include "pwd.h"

struct pop_passwd {
  string pw_name;
  string pw_passwd;
  int pw_uid;
  int pw_gid;
  string pw_gecos;
  string pw_dir;
  string pw_shell;
};

static struct pop_passwd *convert_passwd(struct passwd *pwd) {
  struct pop_passwd *ret;
  if (pwd == NULL)
    return NULL;
  ret = xalloc(sizeof(struct pop_passwd));
  ret->pw_name = Cstring_to_string(pwd->pw_name);
  ret->pw_passwd = Cstring_to_string(pwd->pw_passwd);
  ret->pw_uid = pwd->pw_uid;
  ret->pw_gid = pwd->pw_gid;
  ret->pw_gecos = Cstring_to_string(pwd->pw_gecos);
  ret->pw_dir = Cstring_to_string(pwd->pw_dir);
  ret->pw_shell = Cstring_to_string(pwd->pw_shell);
  return ret;
}

struct pop_passwd *unix_getpwnam(string name) {
  char *Cname = convert_pop_string(name);
  return (convert_passwd(getpwnam(Cname)));
}

struct pop_passwd *unix_getpwuid(int uid) {
  return (convert_passwd(getpwuid(uid)));
}

int unix_getuid(void) {
  return getuid();
}

void unix_setuid(int uid) {
  int ret = setuid(uid);
  if (ret == -1)
    unix_error(__FILE__,__LINE__,"setuid");
  return;
}

void unix_setgid(int gid) {
  int ret = setgid(gid);
  if (ret == -1)
    unix_error(__FILE__,__LINE__,"setgid");
  return;
}

int unix_geteuid(void) {
  return geteuid();
}
