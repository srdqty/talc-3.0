#USE_COFF=y
WHERE=.
INCLUDES=-Iinclude
DEFINES=
CFLAGS=-c $(INCLUDES) $(DEFINES)
CC=gcc
LDFLAGS=

ifdef WINDIR

# Assume Windows NT/98/95/3.x
O=obj
A=lib
CC=cl /nologo
DEFINES += -DWINDOWS
OBJPREFIX=coff_
CFLAGS += /MT

else

# Assume some kind of Unix
O=o
A=a
CC=gcc -Wall -O2 -ggdb
ifdef USE_COFF
DEFINES += -DWINDOWS
OBJPREFIX=coff_
else
OBJPREFIX=elf_
endif

endif


SRCS= \
  obj_install.c \
  $(OBJPREFIX)obj_common.c \
  $(OBJPREFIX)obj_i386.c \
  $(OBJPREFIX)obj_load.c \
  $(OBJPREFIX)obj_reloc.c

OBJECTS=$(SRCS:.c=.$(O))

install: loader.$(O) loader.tali objlib.$(A)
	cp $+ $(WHERE)

clean:
	rm -f *.$(O) *~ objlib.$(A) loader.c

objlib.$(A): $(OBJECTS)
	ar rs $@ $+

%.$(O) : %.c
	$(CC) $(CFLAGS) $< -o $@
