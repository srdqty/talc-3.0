/* Popcorn header file for the UNIX library */

#ifndef UNIX_H
#define UNIX_H

/* Error reporting */

extern exception Unix_error(*(int errno, string unix_fn));
extern string unix_error_string(int errno);

#define ENOENT   2
#define EINTR    4
#define ECHILD  10
#define EAGAIN  11
#define EACCES  13
#define ENOTDIR 20
#define EINVAL  22
#define EPIPE   32

/* File descriptors */

#define STDIN  0
#define STDOUT 1
#define STDERR 2

extern union open_flag {
  void O_RDONLY;
  void O_WRONLY;
  void O_RDWR;
  void O_CREAT;
  void O_EXCL;
  void O_APPEND;
  void O_TRUNC;
}

extern union open_mode_flag {
  void S_IRWXU;
  void S_IREAD;
  void S_IWRITE;
  void S_IEXEC;
  void S_IRUSR;
  void S_IWUSR;
  void S_IXUSR;
  void S_IRWXG;
  void S_IRGRP;
  void S_IWGRP;
  void S_IXGRP;
  void S_IRWXO;
  void S_IROTH;
  void S_IWOTH;
  void S_IXOTH;
}

extern int unix_open(string filename, open_flag flags[],
		     open_mode_flag mode_flags[]); 
extern void unix_close(int fd);
extern void unix_set_nonblock(int fd);
extern void unix_clear_nonblock(int fd);
extern int unix_read(int fd, string buf, int ofs, int len);
extern int unix_write(int fd, string buf, int ofs, int len);
extern int unix_fd_setsize;

/* #define FD_SETSIZE unix_fd_setsize */

extern struct iovec {
  string buf;
  int ofs;
  int len;
}

extern int unix_writev(int fd, iovec buffers[], int numBufs);
extern void unix_dup2(int orig_fd, int copy_fd);
extern int unix_dup(int orig_fd);

/* Filesystem */

extern void unix_unlink(string path);
extern string unix_getcwd();
extern string unix_readlink(string path);
extern string unix_mmap(int fd, int file_ofs, int length);
extern void unix_munmap(string mapped_str);

/* This is a subset of what UNIX stat gives you---dumps stuff about
   the device and inodes */
extern struct stat {
  int st_mode;     /* protection */
  int st_uid;      /* user ID of owner */
  int st_gid;      /* group ID of owner */
  int st_size;     /* total size, in bytes */
  int st_atime;    /* time of last access */
  int st_mtime;    /* time of last modification */
  int st_ctime;    /* time of last change */
}

/* mode flags and macros */
/* XXX change clients to use open_mode_flag type above? */
#define S_IXOTH 1
#define S_IROTH 4
#define S_IFDIR 16384

extern void unix_stat(string filename, stat sb_ret);

extern void unix_chdir(string path);

/* Users */

extern ?struct passwd {
  string pw_name;
  string pw_passwd;
  int pw_uid;
  int pw_gid;
  string pw_gecos;
  string pw_dir;
  string pw_shell;
}

extern passwd unix_getpwnam(string name);
extern passwd unix_getpwuid(int uid);

extern int unix_getuid();
extern int unix_geteuid();
extern void unix_setuid(int uid);
extern void unix_setgid(int gid);

/* Timing */

extern struct timeval {
  int tv_sec;
  int tv_usec;
}
extern void unix_gettimeofday(timeval td);

extern int unix_time();

extern struct tm {
  int tm_sec;
  int tm_min;
  int tm_hour;
  int tm_mday;
  int tm_mon;
  int tm_year;
  int tm_wday;
  int tm_yday;
  bool tm_isdst;
}

extern tm unix_gmtime(int clockval);
extern tm unix_localtime(int clockval);
extern int unix_strftime(string s, int maxlen, string format, tm tm);

/* Polling */

extern fd_set?;

extern fd_set unix_empty_set();
extern void unix_fd_clr(int fd, fd_set set);
extern bool unix_fd_isset(int fd, fd_set set);
extern void unix_fd_set(int fd, fd_set set);
extern void unix_fd_zero(fd_set set);
extern void unix_copy_fd_set(fd_set dst, fd_set src);

extern int unix_select(int max_fd, fd_set read_fds, fd_set write_fds, 
		       fd_set except_fds, timeval timeout);

/* Process control */

extern void unix_chroot(string path);
extern int unix_fork();
extern void unix_execv(string path, string args[], int numargs);
extern void exit(int code);

extern union process_status {
  int WEXITED;
  int WSIGNALED;
  int WSTOPPED;
}

extern union wait_flag {
  void WNOHANG;
  void WUNTRACED;
}

extern *(int, process_status) unix_waitpid(wait_flag flags[], int pid);

/* Signals */

extern union signal_number { /* XXX flesh out */
  void SIGINT;
  void SIGPIPE;
  void SIGTERM;
  void SIGCHLD;
}

extern union signal_behavior {
  void Signal_default;
  void Signal_ignore;
  void Signal_handle(signal_number);
}

extern void unix_signal(signal_behavior what, signal_number which);
extern string unix_signal_string(signal_number which);

/* Sockets */

extern union sockaddr {
  string ADDR_UNIX;
  *(int, int) ADDR_INET;	/* (addr, port) */
}

#define INADDR_ANY 0

extern *(int,sockaddr) unix_accept(int fd);
extern void unix_bind(int fd, sockaddr saddr);
extern void unix_connect(int fd, sockaddr saddr);
extern void unix_listen(int fd, int backlog);

extern union msg_flag {
  void MSG_OOB;
  void MSG_DONTROUTE;
  void MSG_PEEK;
}

extern int unix_recv(int fd, string buf, int ofs, int len, 
		     msg_flag flags[]);
extern *(int,sockaddr) unix_recvfrom(int fd, string buf, int ofs,
				     int len,  msg_flag flags[]);
extern int unix_send(int fd, string buf, int ofs, int len, 
		     msg_flag flags[]);
extern int unix_sendto(int fd, string buf, int ofs, int len,
		       msg_flag flags[], sockaddr dest);

extern union socket_domain {
  void PF_UNIX;               /* Unix Domain */
  void PF_INET;               /* IP */
}
  
extern union socket_type {
  void SOCK_STREAM;           /* reliable delivery (i.e. TCP or IPC) */
  void SOCK_DGRAM;            /* datagram (i.e UDP) */
  void SOCK_RAW;              /* raw socket (i.e. IP) */
  void SOCK_SEQPACKET;        /* ??? */
}

extern int unix_socket(socket_domain dom, socket_type typ, int proto);
extern void unix_socketpair(socket_domain dom, socket_type typ, 
			    int proto, int sv[]);

extern union socket_option {
  void SO_DEBUG;               /* Record debugging information */
  void SO_BROADCAST;           /* Permit sending of broadcast messages */
  void SO_REUSEADDR;           /* Allow reuse of local addresses for bind */
  void SO_KEEPALIVE;           /* Keep connection active */
  void SO_DONTROUTE;           /* Bypass the standard routing algorithms */
  void SO_OOBINLINE;           /* Leave out-of-band data in line */
  void SO_SNDBUF;              /* Size of kernel socket send buffer */
  void SO_TCPNODELAY;          /* TCP NoDelay flag (Nagle's algorithm) */
  /*  void SO_LINGER; */       /* Block on close if data still to be sent */
}

extern int unix_getsockopt(int fd, socket_option flag);
extern void unix_setsockopt(int fd, socket_option flag, int value);

/* Internet addresses */

extern struct host_entry {
  string h_name;
  string h_aliases[];
  socket_domain h_addrtype;
  int h_addr_list[];
#define h_addr h_addr_list[0]
}

extern host_entry unix_gethostbyname(string hostname);
extern host_entry unix_gethostbyaddr(int addr);

extern string unix_inet_ntoa(int addr); /* returns static buf */

/* Random numbers */

extern int random(); 
extern int srandom(int seed);

/* System resources */

extern int getpagesize();

extern union rlimit_resource {
  void RLIMIT_CPU;     /* CPU time in seconds */
  void RLIMIT_FSIZE;   /* Maximum filesize */
  void RLIMIT_DATA;    /* max data size */
  void RLIMIT_STACK;   /* max stack size */
  void RLIMIT_CORE;    /* max core file size */
  void RLIMIT_RSS;     /* max resident set size */
  void RLIMIT_NPROC;   /* max number of processes */
  void RLIMIT_NOFILE;  /* max number of open files */
  void RLIMIT_MEMLOCK; /* max locked-in-memory address space*/
}

#define RLIM_INFINITY 0x7fffffff

extern void unix_getrlimit(rlimit_resource res, *(int,int) ret_curmax);
extern void unix_setrlimit(rlimit_resource res, *(int,int) ret_curmax);

extern struct dirent {
  int d_ino;
  int d_off;
  int d_reclen;
  string d_name;
}

extern DIR?;

#define MAXPATHLEN 4095

extern DIR unix_opendir(string name);
extern void unix_closedir(DIR dirp);
extern bool unix_readdir(DIR dirp,dirent de);
extern void unix_rewinddir(DIR dirp);
extern void unix_seekdir(DIR dirp, int pos);
extern int unix_telldir(DIR dirp);
extern int unix_dirfd(DIR dirp);

#endif
