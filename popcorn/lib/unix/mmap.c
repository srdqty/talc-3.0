#include "unixsupport.h"
#include <sys/mman.h>
#include <stdio.h>
#include <errno.h>

/* This version of mmap elides the protection flags portions of mmap.
   We'll just implement read-only, shared pages for now.  Note that
   this will cause a memory protection fault if the returned memory
   is written to.  Really, we should be able to use the TAL type
   system to enable read-only strings and prevent this from
   happening.

   Not bothering with flags other than MAP_SHARED, at least for now.
*/

/* Associate a finalizer with the allocated string.
   Will unmap (if not already unmapped by the user), if it becomes
   unreachable. */
typedef void (*GC_finalization_proc)
         (void *obj, void *client_data)  ;

extern  void GC_register_finalizer
         (void *obj, GC_finalization_proc fn, void *cd,
                  GC_finalization_proc *ofn, void **ocd)  ;

/* #define DEBUG */

static int do_munmap(string s) {
  int retcode = munmap(s->chars,s->size);
  if (retcode == -1) return -1;

  /* have to null out the string so we don't segfault in trying
     to read it later */
  s->chars = "";
  s->size = 0;
#ifdef DEBUG
    fprintf (stderr,"unmap: %#x (data %#x)\n",
	     (unsigned int)s, (unsigned int)s->chars);
#endif
  return 0;
}

static void mmap_finalizer(void *o, void *unused) {
  string s = (string)o;
  if (s->size != 0) { /* make sure not already unmapped */
    int retc = do_munmap(s);
#ifdef DEBUG
    fprintf (stderr,"  during finalization\n");
#endif
    if (retc == -1)
      perror("munmap");
  }
}

string unix_mmap(int fd, int offset, int length) {
  void *addr;
  string retval;

  addr = mmap(0, length, PROT_READ, MAP_SHARED, fd, offset);
  if ((int)addr == -1)
    unix_error(__FILE__,__LINE__,"mmap");

  /* create a string with the mapped address as its contents;
     make this string 'atomic' so that the pointer argument
     to chars is not scanned. */
  retval = xalloc_atomic(sizeof(struct str_internal));
  retval->size = length;
  retval->chars = (char *)addr;

  /* note finalizer */
  GC_register_finalizer(retval,mmap_finalizer,NULL,NULL,NULL);
  
#ifdef DEBUG
    fprintf (stderr,"  map: %#x (data %#x, size=%d bytes)\n",
	     (unsigned int)retval, (unsigned int)retval->chars,
	     retval->size);
#endif
  return retval;
}

void unix_munmap(string s) {
  int retcode;

  if (s == NULL)
    nullpointer_exn(__FILE__,__LINE__);

  retcode = do_munmap(s);
  if (retcode == -1)
    unix_error(__FILE__,__LINE__,"munmap");
}
