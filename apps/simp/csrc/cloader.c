/*
 * loader.c - program loader routines
 *
 * This file is a part of the SimpleScalar tool suite written by
 * Todd M. Austin as a part of the Multiscalar Research Project.
 *  
 * The tool suite is currently maintained by Doug Burger and Todd M. Austin.
 * 
 * Copyright (C) 1994, 1995, 1996, 1997 by Todd M. Austin
 *
 * This source file is distributed "as is" in the hope that it will be
 * useful.  The tool set comes with no warranty, and no author or
 * distributor accepts any responsibility for the consequences of its
 * use. 
 * 
 * Everyone is granted permission to copy, modify and redistribute
 * this tool set under the following conditions:
 * 
 *    This source code is distributed for non-commercial use only. 
 *    Please contact the maintainer for restrictions applying to 
 *    commercial use.
 *
 *    Permission is granted to anyone to make or distribute copies
 *    of this source code, either as received or modified, in any
 *    medium, provided that all copyright notices, permission and
 *    nonwarranty notices are preserved, and that the distributor
 *    grants the recipient permission for further redistribution as
 *    permitted by this document.
 *
 *    Permission is granted to distribute this file in compiled
 *    or executable form under the same conditions that apply for
 *    source code, provided that either:
 *
 *    A. it is accompanied by the corresponding machine-readable
 *       source code,
 *    B. it is accompanied by a written offer, with no time limit,
 *       to give anyone a machine-readable copy of the corresponding
 *       source code in return for reimbursement of the cost of
 *       distribution.  This written offer must permit verbatim
 *       duplication by anyone, or
 *    C. it is distributed by someone who received only the
 *       executable form, and is accompanied by a copy of the
 *       written offer of source code that they received concurrently.
 *
 * In other words, you are welcome to use, share and improve this
 * source file.  You are forbidden to forbid anyone else to use, share
 * and improve what you give them.
 *
 */

/* FMS: Modified to interface with Popcorn. */

#include <stdio.h>
#include <stdlib.h>
#include "ecoff.h"

extern void *GC_malloc_atomic(int n);
extern void *GC_malloc(int n);

typedef struct { int len; unsigned int *elts; } pop_seg_s, *pop_seg;
typedef struct { unsigned int address; unsigned int kind; 
  pop_seg seg; } pop_seg_desc_s,*pop_seg_desc;
typedef struct { int len; pop_seg_desc *elts; } pop_seg_array_s, *pop_seg_array;

typedef struct { 
  unsigned int entry; 
  unsigned int data_size;
  pop_seg_array segments;
} pop_exec_s,*pop_exec;

#define fatal0(S) { fprintf(stderr,S); exit(1); }
#define fatal1(S,X) { fprintf(stderr,S,X); exit(2); }

#define debug0(S) { fprintf(stderr,S); }
#define debug1(S,X) { fprintf(stderr,S,X); }
#define debug2(S,X1,X2) { fprintf(stderr,S,X1,X2); }

pop_seg new_pop_seg(int words, unsigned int *elts) {
  pop_seg ps = (pop_seg) GC_malloc(sizeof(pop_seg_s));

  if(ps==NULL) fatal0("Out of memory.\n");
  ps->len = words;
  ps->elts = elts;

  return(ps);
}

pop_seg_desc new_pop_seg_desc(int address, int kind, pop_seg seg) {
  pop_seg_desc psd = (pop_seg_desc) GC_malloc(sizeof(pop_seg_desc_s));
  
  if(psd == NULL) fatal0("Out of memory.\n");
  psd->address = address;
  psd->kind = kind;
  psd->seg = seg;

  return psd;
}

pop_seg_array new_pop_seg_array(int len, pop_seg_desc *elts) {
  pop_seg_array ps = (pop_seg_array) GC_malloc(sizeof(pop_seg_array_s));

  if(ps==NULL) fatal0("Out of memory.\n");
  ps->len = len;
  ps->elts = elts;

  return(ps);
}

pop_exec new_pop_exec(unsigned int entry,
		      unsigned int data_size,
		      pop_seg_array segments) {
  pop_exec pe = (pop_exec) GC_malloc(sizeof(pop_exec_s));
  
  if(pe == NULL) fatal0("Out of memory.\n");
  pe->entry = entry;
  pe->data_size = data_size;
  pe->segments = segments;

  return pe;
}

/* Extract the segments from an executable and load them into memory. */
pop_exec load_exec(char *fname) {
  FILE *fobj;
  long floc;
  struct ecoff_filehdr fhdr;
  struct ecoff_aouthdr ahdr;
  struct ecoff_scnhdr shdr;
  pop_seg_desc *segs;
  int i;

  /* load the program into memory, try both endians */
  fobj = fopen(fname, "rb");
  if (!fobj)
    fatal1("cannot open executable `%s'", fname);

  if (fread(&fhdr, sizeof(struct ecoff_filehdr), 1, fobj) < 1)
    fatal1("cannot read header from executable `%s'", fname);

  if (fhdr.f_magic == ECOFF_EB_MAGIC) {
    fatal0("Don't support big endian.\n");
  }
  else {
    if (fhdr.f_magic != ECOFF_EL_MAGIC)
      fatal1("bad magic number in executable `%s'", fname);
  }

  if (fread(&ahdr, sizeof(struct ecoff_aouthdr), 1, fobj) < 1)
    fatal1("cannot read AOUT header from executable `%s'", fname);

  /* seek to the beginning of the first section header, the file header comes
     first, followed by the optional header (this is the aouthdr), the size
     of the aouthdr is given in Fdhr.f_opthdr */
  fseek(fobj, sizeof(struct ecoff_filehdr) + fhdr.f_opthdr, 0);

  /* debug2("processing %d sections in `%s'...", fhdr.f_nscns, fname); */

  segs = (pop_seg_desc *)GC_malloc(sizeof(pop_seg_desc) * fhdr.f_nscns);
  
  { 
    pop_seg_desc empty_seg = new_pop_seg_desc(0,0,new_pop_seg(0,NULL));
    for (i=0; i < fhdr.f_nscns; i++) {
      segs[i] = empty_seg;
    }
  }

  /* loop through the section headers */
  floc = ftell(fobj);
  for (i = 0; i < fhdr.f_nscns; i++)
    {
      char *p;

      if (fseek(fobj, floc, 0) == -1)
	fatal0("could not reset location in executable");
      if (fread(&shdr, sizeof(struct ecoff_scnhdr), 1, fobj) < 1)
	fatal1("could not read section %d from executable", i);
      floc = ftell(fobj);

      switch (shdr.s_flags)
	{
	case ECOFF_STYP_TEXT : /* fall-through */
	case ECOFF_STYP_RDATA: /* fall-through */
	case ECOFF_STYP_DATA : /* fall-through */
	case ECOFF_STYP_SDATA: {
	  
	  unsigned int sz = shdr.s_size;
	  unsigned int  w_sz = sz/sizeof(unsigned int);

	  if(sz % sizeof(unsigned int) != 0) {
	    fatal1("Size %d is not a multiple of 4\n",sz);
	  }

	  p = (char *)GC_malloc_atomic(w_sz * sizeof(unsigned int));

	  if (!p) fatal0("out of virtual memory");

	  if (fseek(fobj, shdr.s_scnptr, 0) == -1)
	    fatal0("could not read `.text' from executable");
	  if (fread(p, shdr.s_size, 1, fobj) < 1)
	    fatal0("could not read text section from executable");

	  segs[i] = new_pop_seg_desc(shdr.s_vaddr,
				     shdr.s_flags,
				     new_pop_seg(w_sz,(unsigned int *)p));

	  break;
	}
	case ECOFF_STYP_BSS:
	  break;
	case ECOFF_STYP_SBSS:
	  break;
        }
    }

  /* done with the executable, close it */
  if (fclose(fobj))
    fatal1("could not close executable `%s'", fname);

  return new_pop_exec(ahdr.entry,
		      ahdr.dsize + ahdr.bsize,
		      new_pop_seg_array(fhdr.f_nscns,segs));
}


