/* COFF object file loading and relocation routines. */

#ifndef __COFF_OBJ_H__
#define __COFF_OBJ_H__ 1

#include <stdio.h>
#include "coff_i386.h"

struct obj_section
{
  struct external_scnhdr *header;
  const char *name;
  char *contents;
  struct obj_section *load_next;
  struct external_reloc *relocation_table;
  int idx;
};

struct obj_symbol
{
  struct obj_symbol *next;	/* hash table link */
  const char *name;
  unsigned long value;
  int secidx;			/* the defining section index/module */
  unsigned short type;
  unsigned char class;
};

/* Borrow these from ELF definition for signalling special symbols */

#define SHN_LORESERVE   0xff00          /* Start of reserved indices */
#define SHN_HIRESERVE   0xffff          /* End of reserved indices */


/* Hardcode the hash table size.  We shouldn't be needing so many
   symbols that we begin to degrade performance, and we get a big win
   by giving the compiler a constant divisor.  */

#define HASH_BUCKETS  521

struct obj_file
{
  struct external_filehdr *header;
  char *opt_header; /* ignored */
  unsigned int baseaddr;
  char *string_table;
  struct external_syment *symbol_table;
  struct obj_section **sections;
  struct obj_section *load_order;
  struct obj_section **load_order_search_start;
  int (*symbol_cmp)(const char *, const char *);
  unsigned long (*symbol_hash)(const char *);
  unsigned long local_symtab_size;
  struct obj_symbol **local_symtab;
  struct obj_symbol *symtab[HASH_BUCKETS];
};

enum obj_reloc
{
  obj_reloc_ok,
  obj_reloc_overflow,
  obj_reloc_dangerous,
  obj_reloc_unhandled
};

#define Addr unsigned int

/* Generic object manipulation routines.  */

unsigned long obj_elf_hash(const char *);

unsigned long obj_elf_hash_n(const char *, unsigned long len);

struct obj_symbol *obj_add_symbol (struct obj_file *f, const char *name, 
				   unsigned long symidx, unsigned short type, 
				   unsigned char class, int secidx, 
				   unsigned int value);

struct obj_symbol *obj_find_symbol (struct obj_file *f,
					 const char *name);

unsigned int obj_symbol_final_value(struct obj_file *f,
				    struct obj_symbol *sym);

void obj_set_symbol_compare(struct obj_file *f,
			    int (*cmp)(const char *, const char *),
			    unsigned long (*hash)(const char *));

void obj_insert_section_load_order (struct obj_file *f,
				    struct obj_section *sec);

int obj_check_undefineds(struct obj_file *f);

unsigned long obj_load_size (struct obj_file *f);

int obj_relocate (struct obj_file *f, unsigned int base);

struct obj_file *obj_load(char *buf, int buflen);

const char *get_coff_string (char raw_input[], 
			     const char *string_table);

int obj_create_image (struct obj_file *f, char *image);

/* Architecture specific manipulation routines.  */

struct obj_file *arch_new_file (void);

struct obj_section *arch_new_section (void);

struct obj_symbol *arch_new_symbol (void);
enum obj_reloc
arch_apply_relocation (struct obj_file *f,
		       struct obj_section *targsec,
		       struct external_reloc *rel,
		       unsigned int v);
#endif /* coff_obj.h */
