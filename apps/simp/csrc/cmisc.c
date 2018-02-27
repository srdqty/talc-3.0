/* Miscellaneous functions I need from C */

#include <stdio.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

#include <fcntl.h>
typedef struct {
  int len;
  unsigned int *elts;
} pop_array_s, *pop_array;

#ifdef __linux__
#define int64 long long
#define FSTAT fstat
#define STAT stat
#else
#define int64 __int64
#define FSTAT _fstat
#define STAT _stat
#endif

#define NUM_REGS 32

extern void *GC_malloc(int n);
extern void *GC_malloc_atomic(int n);

pop_array new_pop_array(int len, unsigned int *elts) {
  pop_array x = (pop_array)GC_malloc(sizeof(pop_array_s));

  if(x==NULL) {
    fprintf(stderr,"Out of memory.");
    exit(1);
  }

  x->len = len;
  x->elts = elts;

  return x;
}

unsigned int **cnew_register_file_indirect() {

  unsigned int *x = (int *)GC_malloc_atomic(NUM_REGS * sizeof(int));
  unsigned int **y = (unsigned int **)GC_malloc(NUM_REGS * sizeof(int *));
  int i;

  for(i=0; i < NUM_REGS; i++) {
    y[i] = &x[i];
  }
  
  return y;
}

#define BLOCK_SIZE 0x4000

unsigned int *cnew_block() {
  return GC_malloc_atomic(BLOCK_SIZE * sizeof(unsigned int));
}

unsigned int *cnew_register_file() {
  return GC_malloc_atomic(NUM_REGS * sizeof(unsigned int));
}

/******************************************************************************/

float bits2float(unsigned int w) {
  return *((float *)&w);
}

unsigned int float2bits(float f) {
  return *((unsigned int *)&f);
}

double bits2double(unsigned int lo, unsigned int hi) {
  int64 z[1] = { ((int64)hi << 32) | ((int64)lo) };
  /* printf("z = %llx \n",z); */
  return *((double *)z);
}

void double2bits(pop_array_s *p, unsigned int offset, double d) {
  int64 f = *((int64 *)&d);

  offset = offset & ~1;

  p->elts[offset+1] = f >> 32;
  p->elts[offset] = (unsigned int)(f & 0xffffffff);
  
}

void double2bits_indirect(unsigned int **p, unsigned int offset, double d) {
  int64 f = *((int64 *)&d);

  offset = offset & ~1;

  *(p[offset+1]) = f >> 32;
  *(p[offset]) = (unsigned int)(f & (0xffffffff));
  
}

void double2bits_direct(unsigned int *p, unsigned int offset, double d) {
  int64 f = *((int64 *)&d);

  offset = offset & ~1;

  p[offset+1] = f >> 32;
  p[offset] = (unsigned int)(f & 0xffffffff);
  
}

/******************************************************************************/
/* System calls. */

extern int tal_errno;

struct  ss_statbuf
{
        short           ss_st_dev;
        unsigned int   ss_st_ino;
        unsigned short  ss_st_mode;
        short         	ss_st_nlink;
        short           ss_st_uid;
        short           ss_st_gid;
        short           ss_st_rdev;
        int           	ss_st_size;
        int          	ss_st_atime;
        int             ss_st_spare1;
        int          	ss_st_mtime;
        int             ss_st_spare2;
        int          	ss_st_ctime;
        int             ss_st_spare3;
        int            ss_st_blksize;
        int            ss_st_blocks;
        unsigned int   ss_st_gennum;
        int            ss_st_spare4;
};

void translate_stat_buf(struct ss_statbuf *pop_buf, struct STAT *sbuf) {
  pop_buf->ss_st_dev     = sbuf->st_dev;
  pop_buf->ss_st_ino     = sbuf->st_ino;
  pop_buf->ss_st_mode    = sbuf->st_mode;
  pop_buf->ss_st_nlink   = sbuf->st_nlink;
  pop_buf->ss_st_uid     = sbuf->st_uid;
  pop_buf->ss_st_gid     = sbuf->st_gid;
  pop_buf->ss_st_rdev    = sbuf->st_rdev;
  pop_buf->ss_st_size    = sbuf->st_size;
  pop_buf->ss_st_atime   = sbuf->st_atime;
  pop_buf->ss_st_mtime   = sbuf->st_mtime;
  pop_buf->ss_st_ctime   = sbuf->st_ctime;
  pop_buf->ss_st_blksize = sbuf->st_blksize;
  pop_buf->ss_st_blocks  = sbuf->st_blocks;
}

/* We take in a pointer to buf so that we can allocate a new array safely. */
int pop_fstat(unsigned int fd, pop_array *buf) {
  struct STAT sbuf;
  int sz_statbuf;
  int result;
  struct ss_statbuf *pop_buf;
  result = FSTAT(fd,&sbuf);

  sz_statbuf = sizeof(struct ss_statbuf);

  if(sz_statbuf % 4 != 0) {
    fprintf(stderr,"Size of statbuf is not a multiple of 4.\n");
    exit(1);
  }
  
  pop_buf =(struct ss_statbuf *)GC_malloc_atomic(sz_statbuf);

  if(pop_buf == NULL) {
    fprintf(stderr,"Out of memory.\n");
    exit(1);
  }

  translate_stat_buf(pop_buf,&sbuf);

  *buf = new_pop_array(sizeof(struct ss_statbuf)/4,(unsigned int *)pop_buf);

  tal_errno = -1; /* XXX - should be errno but can't get it to link? */
  return result;
}

/* We take in a pointer to buf so that we can allocate a new array safely. */
int pop_stat(pop_array fname, pop_array *buf) {
  struct STAT sbuf;
  int sz_statbuf;
  int result;
  struct ss_statbuf *pop_buf;

  result = stat((char *)fname->elts,&sbuf);

  sz_statbuf = sizeof(struct ss_statbuf);

  if(sz_statbuf % 4 != 0) {
    fprintf(stderr,"Size of statbuf is not a multiple of 4.\n");
    exit(1);
  }
  
  pop_buf =(struct ss_statbuf *)GC_malloc_atomic(sz_statbuf);

  if(pop_buf == NULL) {
    fprintf(stderr,"Out of memory.\n");
    exit(1);
  }

  translate_stat_buf(pop_buf,&sbuf);

  *buf = new_pop_array(sizeof(struct ss_statbuf)/4,(unsigned int *)pop_buf);

  tal_errno = -1; /* XXX - should be errno but can't get it to link? */
  return result;
}

/* We take in a pointer to buf so that we can allocate a new array safely. */
int pop_lstat(pop_array fname, pop_array *buf) {
  struct STAT sbuf;
  int sz_statbuf;
  int result;
  struct ss_statbuf *pop_buf;

  result = lstat((char *)fname->elts,&sbuf);

  sz_statbuf = sizeof(struct ss_statbuf);

  if(sz_statbuf % 4 != 0) {
    fprintf(stderr,"Size of statbuf is not a multiple of 4.\n");
    exit(1);
  }
  
  pop_buf =(struct ss_statbuf *)GC_malloc_atomic(sz_statbuf);

  if(pop_buf == NULL) {
    fprintf(stderr,"Out of memory.\n");
    exit(1);
  }

  translate_stat_buf(pop_buf,&sbuf);

  *buf = new_pop_array(sizeof(struct ss_statbuf)/4,(unsigned int *)pop_buf);

  tal_errno = -1; /* XXX - should be errno but can't get it to link? */
  return result;
}


/* SStrix ioctl values */
#define SS_IOCTL_TIOCGETP	1074164744
#define SS_IOCTL_TIOCSETP	-2147060727
#define SS_IOCTL_TCGETP		1076130901
#define SS_IOCTL_TCGETA		1075082331
#define SS_IOCTL_TIOCGLTC	1074164852
#define SS_IOCTL_TIOCSLTC	-2147060619
#define SS_IOCTL_TIOCGWINSZ	1074295912
#define SS_IOCTL_TCSETAW	-2146143143
#define SS_IOCTL_TIOCGETC	1074164754
#define SS_IOCTL_TIOCSETC	-2147060719
#define SS_IOCTL_TIOCLBIC	0x8004747e
#define SS_IOCTL_TIOCLBIS	0x8004747f
#define SS_IOCTL_TIOCLGET	0x4004747c
#define SS_IOCTL_TIOCLSET	0x8004747d


int pop_ioctl(unsigned int ss_ioctl, int b, pop_array c) {
    unsigned int local_req;
    switch(ss_ioctl) {
#ifdef TCGETA
    case SS_IOCTL_TCGETA    : local_req = TCGETA; break;
#endif
#ifdef TIOGLTC
    case SS_IOCTL_TIOGLTC   : local_req = TIOGLTC; break; 
#endif
#ifdef TIOSLTC
    case SS_IOCTL_TIOSLTC   : local_req = TIOSLTC; break; 
#endif
    case SS_IOCTL_TIOCGWINSZ: local_req = TIOCGWINSZ; break; 
#ifdef TCSETAW
    case SS_IOCTL_TCSETAW   : local_req = TCSETAW; break; 
#endif
#ifdef TIOGETC
    case SS_IOCTL_TIOGETC   : local_req = TIOGETC; break;
#endif
#ifdef TIOSETC
    case SS_IOCTL_TIOSETC   : local_req = TIOSETC; break; 
#endif
#ifdef TIOCLBIC
    case SS_IOCTL_TIOCLBIC  : local_req = TIOCLBIC; break;
#endif
#ifdef TIOCLBIS
    case SS_IOCTL_TIOCLBIS  : local_req = TIOCLBIS; break;
#endif
#ifdef TIOCLGET
    case SS_IOCTL_TIOCLGET  : local_req = TIOCLGET; break;
#endif
#ifdef TIOCLSET
    case SS_IOCTL_TIOCLSET  : local_req = TIOCLSET; break;
#endif
    default:
      fprintf(stderr,"syscall: ioctl: ioctl code not supported. d = %d, req = %d\n",b,ss_ioctl);
      return 0;
    }

  return ioctl(b,local_req,(char *)c->elts);
}


int pop_read(int fd, pop_array buf, unsigned int nbytes) {
  return read(fd,(char *)buf->elts,nbytes);
}


int pop_write(int fd, pop_array buf, unsigned int nbytes) {
  return write(fd,(char *)buf->elts,nbytes);
}

/* open(2) flags for SimpleScalar target, syscall.c automagically maps
   between these codes to/from host open(2) flags */
#define SS_O_RDONLY		0
#define SS_O_WRONLY		1
#define SS_O_RDWR		2
#define SS_O_APPEND		0x0008
#define SS_O_CREAT		0x0200
#define SS_O_TRUNC		0x0400
#define SS_O_EXCL		0x0800
#define SS_O_NONBLOCK		0x4000
#define SS_O_NOCTTY		0x8000
#define SS_O_SYNC		0x2000

static int convert_flags(int ss_flags) {

  int result = 0;

  if(ss_flags == SS_O_RDONLY) return O_RDONLY;

#define CHECK_FLAG(SS,O)  \
  if(ss_flags & SS) { result |= O; ss_flags &= ~SS; }

  CHECK_FLAG(SS_O_WRONLY,O_WRONLY);
  CHECK_FLAG(SS_O_RDWR,O_RDWR);
  CHECK_FLAG(SS_O_APPEND,O_APPEND);
  CHECK_FLAG(SS_O_CREAT,O_CREAT);
  CHECK_FLAG(SS_O_TRUNC,O_TRUNC);
  CHECK_FLAG(SS_O_EXCL,O_EXCL);
  CHECK_FLAG(SS_O_NONBLOCK,O_NONBLOCK);
  CHECK_FLAG(SS_O_NOCTTY,O_NOCTTY);
#ifdef O_SYNC
  CHECK_FLAG(SS_O_SYNC,O_SYNC);
#endif

  if(ss_flags != 0) printf("Could not decode io flag %x, ignoring,\n",ss_flags);

  return result;
}

int pop_open(pop_array fname, int ss_flags, int mode) {
  
  int local_flags = convert_flags(ss_flags);
 
  return open((char *)fname->elts, local_flags, mode);
}

int pop_close(int fd) { return close(fd); }

int pop_creat(pop_array fname, int mode) {
  return creat((char *)fname->elts,mode);
}

int pop_unlink(pop_array fname) { 
  return unlink((char *)fname->elts);
}

int pop_chdir(pop_array fname) {
  return chdir((char *)fname->elts);
}

int pop_chmod(pop_array fname, int mode) {
  return chmod((char *)fname->elts,mode);
}

int pop_chown(pop_array fname, int owner, int grp) {
  return chown((char *)fname->elts,owner,grp);
}

int pop_lseek(int fd, int off, int dir) {
  return lseek(fd,off,dir);
}

int pop_getpid()  { return getpid();  }
int pop_getuid()  { return getuid();  }
int pop_geteuid() { return geteuid(); }
int pop_getgid()  { return getgid();  }
int pop_getegid() { return getegid(); }

int pop_access(pop_array fname, int mode) {
  return access((char *)fname->elts,mode);
}

int pop_dup(int fd) { return dup(fd); }

int pop_pipe(pop_array fd) {
  return pipe((int *)fd->elts);
}

int pop_dup2(int fd1, int fd2) {
  return dup2(fd1,fd2);
}

int pop_fcntl(int fd, int cmd, int arg) {
  return fcntl(fd,cmd,arg);
}


struct ss_timezone
{
  int ss_tz_minuteswest; /* minutes west of Greenwich */
  int ss_tz_dsttime;     /* type of dst correction */
};

struct ss_timeval
{
  int ss_tv_sec;         /* seconds */
  int ss_tv_usec;        /* microseconds */
};

int pop_gettimeofday(pop_array ss_tv, pop_array ss_tz) {
  struct timeval tv, *tvp;
  struct timezone tz, *tzp;
  struct ss_timeval *ss_tvp = (struct ss_timeval *)ss_tv->elts;
  struct ss_timezone *ss_tzp = (struct ss_timezone *)ss_tz->elts;

  int result;

  if(ss_tv->len == 0) tvp = NULL;
  else {
    tvp = &tv;
    tv.tv_sec = ss_tvp->ss_tv_sec;
    tv.tv_usec = ss_tvp->ss_tv_usec;
  }

  if(ss_tz->len == 0) tzp = NULL;
  else {
    tzp = &tz;
    tz.tz_minuteswest = ss_tzp->ss_tz_minuteswest;
    tz.tz_dsttime = ss_tzp->ss_tz_dsttime;
  }

  result = gettimeofday(tvp,tzp);

  if(ss_tv->len != 0) {
    ss_tvp->ss_tv_sec  = tv.tv_sec;
    ss_tvp->ss_tv_usec = tv.tv_usec;
  }
  if(ss_tz->len != 0) {
    ss_tzp->ss_tz_minuteswest = tz.tz_minuteswest;
    ss_tzp->ss_tz_dsttime     = tz.tz_dsttime;
  }

  return result;  
}


/* target getrusage() buffer definition, the host stat buffer format is
   automagically mapped to/from this format in syscall.c */
struct ss_rusage
{
    struct ss_timeval ss_ru_utime;
    struct ss_timeval ss_ru_stime;
    int ss_ru_maxrss;
    int ss_ru_ixrss;
    int ss_ru_idrss;
    int ss_ru_isrss;
    int ss_ru_minflt;
    int ss_ru_majflt;
    int ss_ru_nswap;
    int ss_ru_inblock;
    int ss_ru_oublock;
    int ss_ru_msgsnd;
    int ss_ru_msgrcv;
    int ss_ru_nsignals;
    int ss_ru_nvcsw;
    int ss_ru_nivcsw;
};

int pop_getrusage(unsigned int who, pop_array usage) {

  struct ss_rusage *ss_ru = (struct ss_rusage *)usage->elts;
  struct rusage local_rusage;
  int result;

  if(usage->len * 4 != sizeof(struct ss_rusage)) {
    printf("pop_getrusage: array len = %d, usage struct has size %d.\n",
	   usage->len, sizeof(struct ss_rusage));
    exit(1);
  }

  result = getrusage(who,&local_rusage);

  /* convert from host rusage structure to target format */
  ss_ru->ss_ru_utime.ss_tv_sec  = local_rusage.ru_utime.tv_sec;
  ss_ru->ss_ru_utime.ss_tv_usec = local_rusage.ru_utime.tv_usec;
  ss_ru->ss_ru_utime.ss_tv_sec  = local_rusage.ru_utime.tv_sec;
  ss_ru->ss_ru_utime.ss_tv_usec = local_rusage.ru_utime.tv_usec;
  ss_ru->ss_ru_stime.ss_tv_sec  = local_rusage.ru_stime.tv_sec;
  ss_ru->ss_ru_stime.ss_tv_usec = local_rusage.ru_stime.tv_usec;
  ss_ru->ss_ru_stime.ss_tv_sec  = local_rusage.ru_stime.tv_sec;
  ss_ru->ss_ru_stime.ss_tv_usec = local_rusage.ru_stime.tv_usec;
  ss_ru->ss_ru_maxrss           = local_rusage.ru_maxrss;
  ss_ru->ss_ru_ixrss            = local_rusage.ru_ixrss;
  ss_ru->ss_ru_idrss            = local_rusage.ru_idrss;
  ss_ru->ss_ru_isrss            = local_rusage.ru_isrss;
  ss_ru->ss_ru_minflt           = local_rusage.ru_minflt;
  ss_ru->ss_ru_majflt           = local_rusage.ru_majflt;
  ss_ru->ss_ru_nswap            = local_rusage.ru_nswap;
  ss_ru->ss_ru_inblock          = local_rusage.ru_inblock;
  ss_ru->ss_ru_oublock          = local_rusage.ru_oublock;
  ss_ru->ss_ru_msgsnd           = local_rusage.ru_msgsnd;
  ss_ru->ss_ru_msgrcv           = local_rusage.ru_msgrcv;
  ss_ru->ss_ru_nsignals         = local_rusage.ru_nsignals;
  ss_ru->ss_ru_nvcsw            = local_rusage.ru_nvcsw;
  ss_ru->ss_ru_nivcsw           = local_rusage.ru_nivcsw;
   
  return result;
}

int pop_utimes(pop_array pop_fname, pop_array pop_tvals) {
  char *fname = (char *)pop_fname->elts;
  struct ss_timeval *ss_tval = (struct ss_timeval *)pop_tvals->elts;
  struct timeval tval[2];

  tval[0].tv_sec  = ss_tval[0].ss_tv_sec;
  tval[0].tv_usec = ss_tval[0].ss_tv_usec;
  tval[1].tv_sec  = ss_tval[1].ss_tv_sec;
  tval[1].tv_usec = ss_tval[1].ss_tv_usec;
  
  return utimes(fname,tval);
}

struct ss_rlimit {
  int ss_rlim_cur;
  int ss_rlim_max;
};

int pop_getrlimit(int rs, pop_array pop_bf) {

  struct ss_rlimit *ss_buf = (struct ss_rlimit *)pop_bf->elts;
  struct rlimit buf;
  int result;

  buf.rlim_cur = ss_buf->ss_rlim_cur;
  buf.rlim_max = ss_buf->ss_rlim_max;

  result = getrlimit(rs,&buf);
  
  ss_buf->ss_rlim_cur = buf.rlim_cur;
  ss_buf->ss_rlim_max = buf.rlim_max;

  return result;  
}

int pop_setrlimit(int resource, pop_array pop_buf) {
  struct ss_rlimit *ss_buf = (struct ss_rlimit *)pop_buf->elts;
  struct rlimit buf;
  int result;

  buf.rlim_cur = ss_buf->ss_rlim_cur;
  buf.rlim_max = ss_buf->ss_rlim_max;

  result = setrlimit(resource,&buf);
  
  ss_buf->ss_rlim_cur = buf.rlim_cur;
  ss_buf->ss_rlim_max = buf.rlim_max;

  return result;  
}

/* For testing purposes only!! */
/*
void main() {
  unsigned int w;
  float f;
  double d;
  unsigned int x[] = {0,0,0,0};
  pop_array_s p = { 4, x };

  f = 1.0;
  w = float2bits(f);
  printf(" f = 1.0, w = %x \n",w);
  f = bits2float(w);
  printf(" f = %2f, w = %x \n",f,w);

  d = 2.0;
  double2bits(&p,0,d);

  printf(" d = %g, x[0] = %x, x[1] = %x\n",d,x[0],x[1]);

  d = bits2double(x[0],x[1]);
  printf(" d = %g, x[0] = %x, x[1] = %x\n",d,x[0],x[1]);


  d = 2.0;
  double2bits(&p,2,d);

  printf(" d = %g, x[2] = %x, x[3] = %x\n",d,x[2],x[3]);

  d = bits2double(x[2],x[3]);
  printf(" d = %g, x[2] = %x, x[3] = %x\n",d,x[2],x[3]);
}
*/
