/* Routines that print out loaded elf object files for purposes of
   debugging */

#include "obj.h"
#include <alloca.h>
#include <stdlib.h>
#include <ctype.h>

static char *e_types[ET_NUM] = 
{"none", "relocatable", "executable", "shared object", "core"};

void
print_elf_header(FILE *fp, char *indent, ElfW(Ehdr) *header) 
{
  fprintf(fp, "%sElf Header {\n",indent);
  fprintf(fp, "%s  ident\n",indent);
  fprintf(fp, "%s  e_type = %s (%d)\n",indent,
	  (header->e_type > 0 && header->e_type < ET_NUM) ?
	  e_types[header->e_type] : "????", 
	  header->e_type);
  fprintf(fp, "%s  e_machine = %s (%d)\n",indent,
	  (header->e_machine == EM_386 ? "i386" : "????"),
	  header->e_machine);
  fprintf(fp, "%s  e_version = %s (%d)\n",indent,
	  (header->e_version == 1 ? "current" : "????"),
	  header->e_version);
  fprintf(fp, "%s  e_entry = %d\n",indent,header->e_entry);
  fprintf(fp, "%s  e_phoff = %d\n",indent,header->e_phoff);
  fprintf(fp, "%s  e_shoff = %d\n",indent,header->e_shoff);
  fprintf(fp, "%s  e_flags = %d\n",indent,header->e_flags);
  fprintf(fp, "%s  e_ehsize = %d\n",indent,header->e_ehsize);
  fprintf(fp, "%s  e_phentsize = %d\n",indent,header->e_phentsize);
  fprintf(fp, "%s  e_phnum = %d\n",indent,header->e_phnum);
  fprintf(fp, "%s  e_shentsize = %d\n",indent,header->e_shentsize);
  fprintf(fp, "%s  e_shnum = %d\n",indent,header->e_shnum);
  fprintf(fp, "%s  e_shstrndx = %d\n",indent,header->e_shstrndx);
  fprintf(fp, "%s}\n",indent);
}

static char *sh_types[SHT_NUM] =
{"null", "progbits", "symtab", "strtab", "rela", "hash", "dynamic",
 "note", "nobits", "rel", "shlib", "dynsym"};

static char *sh_flags[8] =
{"none", "write", "alloc", "write, alloc", "exec", "write, exec",
 "alloc, exec", "alloc, exec, write"};

void 
print_elf_section_header(FILE *fp, char *indent, ElfW(Shdr) *header,
			 char *strtab)
{
  fprintf(fp, "%sElf Section Header {\n",indent);
  if (strtab == NULL) {
    fprintf(fp, "%s  sh_name = %d\n",indent,header->sh_name);
  } else {
    fprintf(fp, "%s  sh_name = %s\n",indent,&strtab[header->sh_name]);
  }
  fprintf(fp, "%s  sh_type = %s (%d)\n",indent,
	  header->sh_type < SHT_NUM && header->sh_type >= 0 ?
	  sh_types[header->sh_type] : "????",
	  header->sh_type);
  fprintf(fp, "%s  sh_flags = %s (%d)\n",indent,
	  header->sh_flags < 8 && header->sh_flags >= 0 ?
	  sh_flags[header->sh_flags] : "????",
	  header->sh_flags);
  fprintf(fp, "%s  sh_addr = %d\n",indent,header->sh_addr);
  fprintf(fp, "%s  sh_offset = %d\n",indent,header->sh_offset);
  fprintf(fp, "%s  sh_size = %d\n",indent,header->sh_size);
  fprintf(fp, "%s  sh_link = %d\n",indent,header->sh_link);
  fprintf(fp, "%s  sh_info = %d\n",indent,header->sh_info);
  fprintf(fp, "%s  sh_addralign = %d\n",indent,header->sh_addralign);
  fprintf(fp, "%s  sh_entsize = %d\n",indent,header->sh_entsize);
  fprintf(fp, "%s}\n",indent);
}


void
print_load_map(struct obj_file *f)
{
  int
  load_map_cmp(const void *a, const void *b)
  {
    struct obj_symbol **as = (struct obj_symbol **)a;
    struct obj_symbol **bs = (struct obj_symbol **)b;
    unsigned long aa = obj_symbol_final_value(f, *as);
    unsigned long ba = obj_symbol_final_value(f, *bs);
    return aa < ba ? -1 : aa > ba ? 1 : 0;
  }

  int i, nsyms, *loaded;
  struct obj_symbol *sym;
  struct obj_symbol **all, **p;
  struct obj_section *sec;

  /* Report on the section layout.  */

  printf("Sections:       Size      %-*s  Align\n",
	  (int)(2*sizeof(void*)), "Address");
  for (sec = f->load_order; sec ; sec = sec->load_next)
    {
      int a;
      unsigned long tmp;
      for (a = -1, tmp = sec->header.sh_addralign; tmp ; ++a)
	tmp >>= 1;
      if (a == -1)
	a = 0;

      printf("%-16s%08lx  %0*lx  2**%d\n", sec->name, sec->header.sh_size,
	      (int)(2*sizeof(void*)), sec->header.sh_addr, a);
    }

  /* Quick reference which section indicies are loaded.  */

  loaded = alloca(sizeof(int) * (i = f->header.e_shnum));
  while (--i >= 0)
    loaded[i] = (f->sections[i]->header.sh_flags & SHF_ALLOC) != 0;

  /* Collect the symbols we'll be listing.  */

  for (nsyms = i = 0; i < HASH_BUCKETS; ++i)
    for (sym = f->symtab[i]; sym; sym = sym->next)
      if (sym->secidx <= SHN_HIRESERVE
	  && (sym->secidx >= SHN_LORESERVE || loaded[sym->secidx]))
	++nsyms;

  all = alloca(nsyms * sizeof(struct obj_symbol *));

  for (i = 0, p = all; i < HASH_BUCKETS; ++i)
    for (sym = f->symtab[i]; sym; sym = sym->next)
      if (sym->secidx <= SHN_HIRESERVE
	  && (sym->secidx >= SHN_LORESERVE || loaded[sym->secidx]))
	*p++ = sym;

  /* Sort them by final value.  */

  qsort(all, nsyms, sizeof(struct obj_file *), load_map_cmp);

  /* And list them.  */

  printf("\nSymbols:\n");
  for (p = all; p < all+nsyms; ++p)
    {
      char type = '?';
      unsigned long value;

      sym = *p;
      if (sym->secidx == SHN_ABS)
	{
	  type = 'A';
	  value = sym->value;
	}
      else if (sym->secidx == SHN_UNDEF)
	{
	  type = 'U';
	  value = 0;
	}
      else
	{
	  struct obj_section *sec = f->sections[sym->secidx];

	  if (sec->header.sh_type == SHT_NOBITS)
	    type = 'B';
	  else if (sec->header.sh_flags & SHF_ALLOC)
	    {
	      if (sec->header.sh_flags & SHF_EXECINSTR)
		type = 'T';
	      else if (sec->header.sh_flags & SHF_WRITE)
		type = 'D';
	      else
		type = 'R';
	    }

	  value = sym->value + sec->header.sh_addr;
	}

      if (ELFW(ST_BIND)(sym->info) == STB_LOCAL)
	type = tolower(type);

      printf("%0*lx %c %s\n", (int)(2*sizeof(void*)), value,
	      type, sym->name);
    }
}

