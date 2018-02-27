DYN_UPDATE=
NO_STACK_TRACE=
include ../Makefile.inc

SOURCES=accept.c bind.c chroot.c close.c connect.c dir.c dirent.c dup.c exec.c 	\
  fcntl.c fork.c gethost.c gethostname.c gettimeofday.c inetaddr.c	\
  listen.c mmap.c open.c readlink.c readwrite.c rlimit.c select.c	\
  sendrecv.c signals.c socket.c socketaddr.c sockopt.c stat.c	\
  time.c unixsupport.c unlink.c user.c wait.c writev.c dyninit.c

OBJECTS=$(SOURCES:.c=.$(O)) popraise.$(O) unix_prelude.$(O)

all: unixlib.tali unixlib.$(A)

unixlib.$(A): $(OBJECTS)
	ar r $@ $+

dyninit.c: dyninit.tmpl
	cp dyninit.tmpl dyninit.c
	genCinit.exe --no-string-def --init-code-preamble "extern int dyninit_unix_prelude(void *unused,void *unused2, void (*f)(void *closure,string sym,rep r,void *p), void *f_closure, int no_init);" --init-code-postamble "dyninit_unix_prelude(look_sym,look_closure,upd_sym,upd_closure,no_init);" unixlib.tali >> dyninit.c
clean:
	rm -f *.$(O) *.$(A) *.*tal* *~ *.to dyninit.c

