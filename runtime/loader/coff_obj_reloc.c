/* Elf relocation routines.
   Copyright 1996, 1997 Linux International.

   Contributed by Richard Henderson <rth@tamu.edu>

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

#ident "$Id: coff_obj_reloc.c,v 1.1 1999/10/19 18:25:00 mwh Exp $"

#include <string.h>
#include <assert.h>

#include <obj.h>
#include <util.h>

/*======================================================================*/

int
obj_check_undefineds(struct obj_file *f)
{
  unsigned long i;
  int ret = 1;

  for (i = 0; i < HASH_BUCKETS; ++i)
    {
      struct obj_symbol *sym;
      for (sym = f->symtab[i]; sym ; sym = sym->next)
	if (sym->secidx == SECTION_UNDEF)
	  {
	    if (sym->class == CLASS_WEAKEXT)
	      {
		error("Don't know how to deal with weak symbol %s", sym->name);
		ret = 0;
		/*
		sym->secidx = SHN_ABS;
		sym->value = 0;
		*/
	      }
	    else
	      {
		error("unresolved symbol %s", sym->name);
		ret = 0;
	      }
	  }
    }

  return ret;
}

unsigned long
obj_load_size (struct obj_file *f)
{
  unsigned long dot = 0;
  struct obj_section *sec;

  /* Finalize the positions of the sections relative to one another.  */

  for (sec = f->load_order; sec ; sec = sec->load_next)
    {
      unsigned int align;

      align = ALIGN(sec->header->s_flags);
      if (align && (dot & (align - 1)))
	dot = (dot | (align - 1)) + 1;

      sec->header->s_vaddr = dot;
      dot += sec->header->s_size;
    }

  return dot;
}

int
obj_relocate (struct obj_file *f, unsigned int base)
{
  int i, n = f->header->f_nscns;
  int ret = 1;
  struct external_syment *symtab = f->symbol_table;
  const char *strtab = f->string_table;

  /* Finalize the addresses of the sections.  */

  f->baseaddr = base;
  for (i = 0; i < n; ++i)
    f->sections[i]->header->s_vaddr += base;

  /* And iterate over all of the relocations.  */

  for (i = 0; i < n; ++i)
    {
      struct obj_section *targsec;
      struct external_reloc *rel, *relend;

      targsec = f->sections[i];
      if (targsec->relocation_table == NULL)
	continue;
#ifdef VERBOSE
      printf("-- Relocations from section %s --\n",f->sections[i]->name);
#endif

      rel = targsec->relocation_table;
      relend = rel + targsec->header->s_nreloc;

      for (; rel < relend; ++rel)
	{
	  unsigned int value = 0;
	  struct obj_symbol *intsym = NULL;
	  unsigned long symndx;
	  struct external_syment *extsym = 0;
	  const char *errmsg;

	  /* Attempt to find a value to use for this relocation.  */

	  symndx = rel->r_symndx;
	  if (symndx)
	    {
	      /* Note we've already checked for undefined symbols.  */

	      extsym = &symtab[symndx];
	      if (extsym->e_sclass == CLASS_STATIC)
		{
		  /* Local symbols we look up in the local table to be sure
		     we get the one that is really intended.  */
		  intsym = f->local_symtab[symndx];
		}
	      else
		{
		  /* Others we look up in the hash table.  */
	          const char *name;
		  name = get_coff_string(extsym->e.e_name,strtab);

	          intsym = obj_find_symbol(f, name);
		}

	      value = obj_symbol_final_value(f, intsym);
	    }

	  /* Do it! */
	  switch (arch_apply_relocation(f,targsec,rel,value))
	    {
	    case obj_reloc_ok:
	      break;

	    case obj_reloc_overflow:
	      errmsg = "Relocation overflow";
	      goto bad_reloc;
	    case obj_reloc_dangerous:
	      errmsg = "Dangerous relocation";
	      goto bad_reloc;
	    case obj_reloc_unhandled:
	      errmsg = "Unhandled relocation";
	    bad_reloc:
	      if (extsym)
		{
		  error("%s of type %#x for %s", errmsg,
			rel->r_type,
			intsym->name);
		}
	      else
		{
		  error("%s of type %#x", errmsg,
			rel->r_type);
		}
	      ret = 0;
	      break;
	    }
	}
    }

  return ret;
}

int
obj_create_image (struct obj_file *f, char *image)
{
  struct obj_section *sec;
  unsigned int base = f->baseaddr;

  for (sec = f->load_order; sec ; sec = sec->load_next)
    {
      char *secimg;

      if (sec->header->s_size == 0)
	continue;

      secimg = image + (sec->header->s_vaddr - base);

      /* Note that we allocated data for NOBITS sections earlier.  */
      memcpy(secimg, sec->contents, sec->header->s_size);
    }

  return 1;
}
