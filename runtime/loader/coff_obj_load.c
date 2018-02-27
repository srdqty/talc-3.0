/* COFF file reader. */

#include <string.h>
#include <assert.h>

#include "obj.h"
#include "util.h"

/*======================================================================*/

const char *
get_coff_string (char raw_input[], const char *string_table)
{
  /* Pointer to the string table */
  if (raw_input[0] == '\0') {
    int idx = *(unsigned int *)(&raw_input[4]);
    return &string_table[idx];
  }

  /* String is defined in-line */
  else {
    int i;
    char *new_string;
    for (i = 0; i<8; i++) {
      if (raw_input[i] == '\0')
	return raw_input;
    }
    new_string = xmalloc(9);
    strncpy(new_string,raw_input,8);
    new_string[8] = '\0';
    return new_string;
  }
}

struct obj_file *
obj_load (char *buf, int buflen)
{
  struct obj_file *f;
  struct external_scnhdr *section_headers;
  int shnum, i;
  char *strtab;
  int strtab_len;
  int bufidx = 0, saveidx;

  assert(sizeof(unsigned char) == 1);
  assert(sizeof(unsigned short) == 2);
  assert(sizeof(unsigned int) == 4);
  assert(sizeof(struct external_filehdr) == FILHSZ);
  assert(sizeof(struct external_scnhdr) == SCNHSZ);

  /* Read the file header.  */
  f = arch_new_file();
  memset(f, 0, sizeof(*f));
  f->symbol_cmp = strcmp;
  f->symbol_hash = obj_elf_hash;
  f->load_order_search_start = &f->load_order;
  
  f->header = (struct external_filehdr *)buf;
  if (buflen < sizeof(struct external_filehdr))
    {
      error("error reading COFF header");
      return NULL;
    }
  else
    bufidx += sizeof(struct external_filehdr);
  
  if (f->header->f_magic != I386MAGIC)
    {
      error("not an COFF i386 file");
      return NULL;
    }
  
  if (CHECK_FLAG(*f->header,F_RELFLG) ||
      CHECK_FLAG(*f->header,F_EXEC) ||
      CHECK_FLAG(*f->header,F_LSYMS))
    {
      error("COFF file not a relocatable object");
      return NULL;
    }
  
  /* Read the optional header */

  if (f->header->f_opthdr != 0) {
    f->opt_header = &buf[bufidx];
    if (bufidx + sizeof(AOUTHDR) <= buflen)
      bufidx += f->header->f_opthdr;
    else
      {
	error("error reading optional header");
	return NULL;
      }  
  }
  else
    f->opt_header = NULL;
  
  /* Read the string table */
  
  saveidx = bufidx;
  bufidx = f->header->f_symptr + SYMESZ * f->header->f_nsyms;
  if ((bufidx + 4) <= buflen) {
    strtab_len = *(unsigned int *)(&buf[bufidx]);
    strtab = &buf[bufidx];
    *(unsigned int *)(&strtab[0]) = 0; /* null out length */
    if ((strtab_len + bufidx) > buflen)
      {
	error("error reading string table");
	return NULL;
      }
  }
  else
    {
      error("error reading string table length");
      return NULL;
    }
  bufidx = saveidx;
  f->string_table = strtab;

  /* Read the section headers.  */

  shnum = f->header->f_nscns;
  f->sections = (struct obj_section **)
    xmalloc(sizeof(struct obj_section *) * shnum);
  memset(f->sections, 0, sizeof(struct obj_section *) * shnum);

  section_headers = (struct external_scnhdr *)&buf[bufidx];
  if ((bufidx + sizeof(struct external_scnhdr) * shnum) > buflen)
    {
      error("error reading COFF section headers");
      return NULL;
    }

  /* Read the section data.  */

  for (i = 0; i < shnum; ++i)
    {
      struct obj_section *sec;

      f->sections[i] = sec = arch_new_section();
      memset(sec, 0, sizeof(*sec));

      sec->header = &section_headers[i];
      sec->idx = i;

      if (sec->header->s_size > 0)
	{
	  /* Read the section contents */

	  sec->contents = &buf[sec->header->s_scnptr];
	  if ((sec->header->s_scnptr + sec->header->s_size) > buflen)
	    {
	      error("error reading COFF section data");
	      return NULL;
	    }

	  /* Read relocation table */	  
	  { 
	    unsigned short num_relocs = sec->header->s_nreloc;
	    int table_size = sizeof(struct external_reloc) * num_relocs;
	    char *tmp, *raw_table;
	    int j;

	    if (table_size) {
	      tmp = (char *)sec->relocation_table =
		(struct external_reloc *)xmalloc(table_size);
	      memset(tmp,0,table_size);

	      raw_table = (char *)&buf[sec->header->s_relptr];
	      if ((sec->header->s_relptr + RELSZ * num_relocs) > buflen)
		{
		  error("error reading COFF relocation table");
		  return NULL;
		}

	      /* fix up alignment */
	      for (j = 0; j < num_relocs; ++j) {
		memcpy(&tmp[j*(sizeof(struct external_reloc))],
		       &raw_table[j*RELSZ], RELSZ);
	      }
	    }
	    else
	      sec->relocation_table = NULL;
	  }
	}
      else
	sec->contents = NULL;
    }

  /* Read in the symbol table */
  { 
    unsigned long j, numaux;
    struct external_syment *sym;
    char *raw_symtab, *tmp;

    unsigned int nsym = f->header->f_nsyms;

    tmp = (char *)f->symbol_table = (struct external_syment *)
      xmalloc(nsym * sizeof(struct external_syment));
    memset(tmp,0,nsym * sizeof(struct external_syment));

    raw_symtab = (char *)&buf[f->header->f_symptr];
    if ((f->header->f_symptr + SYMESZ * nsym) > buflen)
      {
	error("error reading COFF symbol table");
	return NULL;
      }
  
    /* fix up alignment */
    for (j = 0; j < nsym; ++j) {
      memcpy(&tmp[j*(sizeof(struct external_syment))],
	     &raw_symtab[j*SYMESZ],
	     SYMESZ);
    }
    sym = f->symbol_table;

    /* Allocate space for a table of local symbols.  */
    j = f->local_symtab_size = nsym; /* XXX conservative; should calc */
    f->local_symtab = (struct obj_symbol **)
      xmalloc(j *= sizeof(struct obj_symbol *));
    memset(f->local_symtab, 0, j);

    /* Insert all symbols into the hash table.  */
    for (j = 0, numaux = 0; j < nsym; ++j, ++sym)
      {
	const char * name;

	if (numaux == 0) {
	  name = get_coff_string(sym->e.e_name,strtab);
	  obj_add_symbol(f, name, j, sym->e_type, sym->e_sclass,
			 sym->e_scnum, sym->e_value);
	  numaux = sym->e_numaux;
	}
	else
	  numaux--;
      }
  }

  /* Fill in the section names and set up the load order */

  for (i = 0; i < shnum; ++i)
    {
      struct obj_section *sec = f->sections[i];
      sec->name = get_coff_string(sec->header->s_name,strtab);
    }

  for (i = 0; i < shnum; ++i)
    {
      struct obj_section *sec = f->sections[i];
      if (!(sec->header->s_flags & SFLAG_LNK_REMOVE))
	obj_insert_section_load_order(f, sec);
    }

  return f;
}
