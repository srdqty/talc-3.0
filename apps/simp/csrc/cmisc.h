#ifndef __CMISC_H
#define __CMISC_H

#define BLOCK_SIZE 0x4000
#define NUM_REGS 32

extern unsigned int cnew_block()[|BLOCK_SIZE|];
extern unsigned int cnew_register_file()[|NUM_REGS|];
extern *(unsigned int) cnew_register_file_indirect()[|NUM_REGS|];

// Expensive identity functions.
extern float bits2float(unsigned int x);
extern unsigned int float2bits(float f);
extern double bits2double(unsigned int hi, unsigned int lo);
extern void double2bits(unsigned int x[], int offset, double d);
extern void double2bits_indirect(*(unsigned int) x[|NUM_REGS|], 
				 int offset, double d);
extern void double2bits_direct(unsigned int x[|NUM_REGS|],int offset, double d);

extern int pop_fstat(int fd, *(<unsigned int>array) buf);
extern int pop_stat(string fname, *(<unsigned int>array) buf);
extern int pop_lstat(string fname, *(<unsigned int>array) buf);
extern int pop_ioctl(unsigned int ss_ioctl, int b, <unsigned int>array buf);
extern int pop_read(int fd,  string buf, unsigned int nbytes);
extern int pop_write(int fd, string buf, unsigned int nbytes);
extern int pop_open(string fname, int ss_flags, int mode);
extern int pop_close(int fd);
extern int pop_creat(string fname, int mode);
extern int pop_unlink(string fname);

extern int pop_chdir(string fname);
extern int pop_chmod(string fname,int mode);
extern int pop_chown(string fname, int owner, int grp);

extern int pop_lseek(int fd, int off, int dir);

extern int pop_getpid();
extern int pop_getuid();
extern int pop_geteuid();
extern int pop_getgid();
extern int pop_getegid();

extern int pop_access(string fname, int mode);

extern int pop_dup(int fd);

extern int pop_pipe(<unsigned int>array fd);

extern int pop_dup2(int fd1, int fd2);

extern int pop_fcntl(int fd, int cmd, int arg);

extern int pop_gettimeofday(unsigned int tv[], unsigned int tz[]);

extern int pop_getrusage(unsigned int who, unsigned int usage[]);

extern int pop_utimes(string fname, unsigned int tvals[]);

extern int pop_getrlimit(int resource, unsigned int buf[]);

extern int pop_setrlimit(int resource, unsigned int buf[]);

#endif
