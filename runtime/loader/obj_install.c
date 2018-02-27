/* Insert a module into a running kernel.
   Copyright 1996, 1997 Linux International.

   New implementation contributed by Richard Henderson <rth@tamu.edu>
   Based on original work by Bjorn Eckwall <bj0rn@blox.se>

   This file is part of the Linux modutils.

   This program is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by the
   Free Software Foundation; either version 2 of the License, or (at your
   option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software Foundation,
   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.  */


#include <stdarg.h>
#include <assert.h>

#include "obj.h"
#include "util.h"

void error(const char *fmt, ...) {
  va_list ap;
  va_start(ap,fmt);
  vfprintf(stderr,fmt,ap);
  va_end(ap);
  fprintf(stderr,"\n");
  fflush(stderr);
}

/*======================================================================*/

struct registered_symbol
{
  unsigned long value;
  unsigned long name;
  struct registered_symbol *next;
};

/* Conditionally add the symbols from the given symbol set to the
   new module.  */

static int
add_symbols_from(struct obj_file *f, int idx,
		 struct registered_symbol *syms)
{
  struct registered_symbol *s;
  int used = 0;

  for (s = syms; s != NULL; s = s->next)
    {
      /* Only add symbols that are already marked external.  If we
	 override locals we may cause problems for argument initialization.
	 We will also create a false dependency on the module.  */

      struct obj_symbol *sym;
      sym = obj_find_symbol(f, (char *)s->name);
#ifndef WINDOWS
      if (sym && ELFW(ST_BIND)(sym->info) != STB_LOCAL)
	{
	  sym = obj_add_symbol(f, (char *)s->name, -1,
			       ELFW(ST_INFO)(STB_GLOBAL, STT_NOTYPE),
			       idx, s->value, 0);
#else
      if (sym && sym->class != CLASS_STATIC)
	{
	  sym = obj_add_symbol(f, (char *)s->name, -1,
			       TYPE_NULL, CLASS_EXTERNAL,
			       idx, s->value);
#endif
	  /* Did our symbol just get installed?  If so, mark the
	     module as "used".  */
	  if (sym->secidx == idx)
	    used = 1;
	}
    }

  return used;
}

static struct registered_symbol *registered_syms = NULL;

void
register_symbol (unsigned long value, unsigned long name) 
{
  struct registered_symbol *s = 
    (struct registered_symbol *)xmalloc(sizeof(struct registered_symbol));
  s->name = name;
  s->value = value;
  s->next = registered_syms;
  registered_syms = s;
#ifdef VERBOSE
  printf("registered symbol |%s| = %#x\n",(char *)name, value);
  fflush(stdout);
#endif
}
  
static void
resolve_externals(struct obj_file *f)
{
  add_symbols_from(f, SHN_HIRESERVE+1, registered_syms);
}

struct obj_file *
load(char *buf, int buflen)
{
  struct obj_file *f;
  unsigned long m_size;
  char *m_addr;

  /* Load it into memory */

  if ((f = obj_load(buf,buflen)) == NULL) {
    error("Failed to load object file");
    goto out;
  }

  /* Fill in allowed external symbols; we need this for special
     macro symbols.  In general, externals will be resolved
     by the init function */

  resolve_externals(f);
  if (!obj_check_undefineds(f)) {
    error("Undefined symbols in object file");
    goto out;
  }

#ifndef WINDOWS
  /* These are defensive checks at this point, to make sure
     that the loaded file doesn't have any common symbols or
     relocations that use a GOT */
  obj_allocate_commons(f);
#endif

  /* XXX hide special init symbol? */
/*
  hide_special_symbols(f);
*/

  /* Module has now finished growing; find its size and install it.  */

  m_size = obj_load_size(f);
#ifdef COMMENT_OUT
  {
    int pgsize;
    if ((pgsize = sysconf(_SC_PAGESIZE)) < 0) {
      error("Error getting the pagesize");
      goto out;
    }
    else {
      int npages;
      npages = (m_size + sizeof (long) + pgsize - 1) / pgsize;
      m_addr = xmalloc(npages * pgsize);
      /* is it aligned? XXX if not, bump it forward by the diff */ 
      /* assert(m_addr & (~pgsize) == 0); */
    }
  }
#endif
  m_addr = xmalloc(m_size);

  if (!obj_relocate(f, (Addr)m_addr))
    {
      error("Failed relocation");
      goto out;
    }

  obj_create_image(f, m_addr);

#ifdef DEBUG
  print_load_map(f);
#endif

out:
  fflush(stderr);
  return f;
}

Addr
lookup_symbol(struct obj_file *f, const char *symname)
{
  Addr init = obj_symbol_final_value(f, obj_find_symbol(f, symname));
  if (init == (Addr)0) {
    error("could not find symbol %s", symname);
  }
  return init;
}
