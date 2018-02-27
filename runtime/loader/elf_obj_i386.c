/* i386 specific support for Elf loading and relocation.
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

#ident "$Id: elf_obj_i386.c,v 1.1 1999/10/19 18:25:00 mwh Exp $"

#include <string.h>
#include <assert.h>

#include <obj.h>
#include <util.h>


/*======================================================================*/

struct obj_file *
arch_new_file (void)
{
  return (struct obj_file *)xmalloc(sizeof(struct obj_file));
}

struct obj_section *
arch_new_section (void)
{
  return (struct obj_section *)xmalloc(sizeof(struct obj_section));
}

struct obj_symbol *
arch_new_symbol (void)
{
  return (struct obj_symbol *)xmalloc(sizeof(struct obj_symbol));
}

enum obj_reloc
arch_apply_relocation (struct obj_file *f,
		       struct obj_section *targsec,
		       struct obj_section *symsec,
		       struct obj_symbol *sym,
		       Elf32_Rel *rel,
		       Elf32_Addr v)
{
  Elf32_Addr *loc = (Elf32_Addr *)(targsec->contents + rel->r_offset);
  Elf32_Addr dot = targsec->header->sh_addr + rel->r_offset;

  enum obj_reloc ret = obj_reloc_ok;

  switch (ELF32_R_TYPE(rel->r_info))
    {
    case R_386_32:
#ifdef VERBOSE
      printf("abs reloc v = %x to @%x (a @%x), oldval = %x, ",
	     v, loc, dot, *loc);
#endif
      *loc += v;
#ifdef VERBOSE
      printf("newval = %x\n",*loc);
#endif
      break;

    case R_386_PC32:
#ifdef VERBOSE
      printf("pc-rel reloc v-dot = %x to @%x, oldval = %x, ", 
	     v - dot, loc, *loc);
#endif
      *loc += v - dot;
#ifdef VERBOSE
      printf("newval = %x\n",*loc);
#endif
      break;

    default:
      ret = obj_reloc_unhandled;
      break;
    }

  return ret;
}
