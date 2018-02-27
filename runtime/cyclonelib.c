/**********************************************************************/
/* cyclonelib.c                                                       */
/*                                                                    */
/* C code used by the Cyclone macros                                  */
/*                                                                    */
/* XXX: eventually the C code should be eliminated, it is useful now  */
/* for debugging.                                                     */
/**********************************************************************/


#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
void *GC_malloc(int size);
void *GC_realloc(char *ptr, int size);

/*

Here's a picture of the data structures used in Cyclone code generation:

  cg_ref       cg_region        cons               cons

  +-----+     +---------+      +-----+
  |  *------->|    *---------->|  *-------> x1
  +-----+     +---------+      +-----+            +-----+
              | length  |      |  *-------------->|  *--------> x2
              +---------+      +-----+            +-----+
              | current |                         |  /  |
              +---------+                         +-----+
              |  code   |
              |    .    |
              |    .    |
              |    .    |
              |         |
              +---------+


The Cyclone macros manipulate a ref cell that points to a region; the
region holds the code that is being generated plus some bookkeeping
data.

The ref cell is used so that the region can be resized (and possibly
relocated) during code generation.

The region includes a pointer to a list of data values that have been
placed in holes during code generation.  This is needed so that the
filled values are not lost during garbage collection.


Informally, the Cyclone macros operate as follows.

CG_START returns a new ref cell pointing to a new region.

CG_DUMP takes a ref cell, dumps into the region (possibly resizing it),
and returns an offset into the region.  If a region is resized, old
offsets are still good.

CG_FILL takes a ref cell, offset, and fill value, plugs the hole,
and adds an element to the list of fill values for the garbage collector.

CG_FILLJMP and CG_FILLJCC are for inter-template jumps.

CG_END discards the ref cell and returns a pointer to the code in the
region. (It is important that the ref cell be marked as unusable by
the type system after CG_END.) Note, the value returned by CG_END is
an internal pointer, so it is important for the garbage collector to
recognize this (ALL_INTERIOR_POINTERS must be defined). I don't think
we can do away with this: heap-allocated code seems to require
interior pointers. For example, generate a function, call it, and lose
the pointer to it. Then the only way for the gc to reach it is via the
program counter, which is a pointer into the interior of the region.

CG_FORGET is a nop that is needed by the verifier.

The macros are defined in talx86/cyclone.ml, and some of them call
functions defined in this file.

*/


/**********************************************************************/
/* If DEBUG is defined, then the functions will print out             */
/* debugging information when they are invoked.                       */
/* If DEBUG2 is defined, templates will start out with some NOP's     */
/* to help the disassembler synchronize with the first real           */
/* instruction of the template.                                       */
/**********************************************************************/
/*
#define DEBUG2
#define DEBUG
*/

#ifdef DEBUG
#define PR(x) (fprintf(stderr,(x)), fflush(stderr))
#else
#define PR(x)
#endif

/**********************************************************************/
/* Uncomment this to turn off garbage collection for the              */
/* Cyclone datastructures                                             */
/**********************************************************************/
/*
#define GC_malloc(x) (malloc(x))
*/






/**********************************************************************/
/* Initial size of a code generation region; must be a multiple of 4  */
/**********************************************************************/
#define buffersize (512)

typedef struct {
  int length;
  unsigned char contents[0];
} template;

struct cons {
  void *head;
  struct cons *next;
};

typedef struct cg_region {
  struct cons *fills;        /* list of filled values, for the gc */
  int length;                /* length of the code region */
  int current;               /* offset to dump the next template */
  unsigned char contents[0]; /* the code under construction */
} cg_region;

/* Extra indirection in case we need to expand the region */
struct cg_ref {
  cg_region *region;
};

void external_addr() { }  /* used for dummy hole value in cyclone.ml */

struct cg_ref *CG_start(void)
{
  struct cg_ref *newref = GC_malloc(sizeof(struct cg_ref));
  /* Should we check to be sure we didn't run out of space??
     FMS: Yes! XXX
     Stdlib.c doesn't */

  /* Eventually make this an atomic malloc, except for the first cell */
  cg_region *newregion = GC_malloc(sizeof(cg_region)+buffersize);
  /* Should we check to be sure we didn't run out of space??
     FMS: Yes! XXX
     Stdlib.c doesn't */

  newref->region = newregion;
  newregion->fills = NULL;
  newregion->length = buffersize;
  newregion->current = 0;


#ifdef DEBUG
  fflush(stdout);
  fprintf(stderr,"\nCG_start\n");
  fprintf(stderr, "\tnew ref cell at 0x%08x\n",(int)newref);
  fprintf(stderr, "\tnew region at   0x%08x\n",(int)newregion);
  fprintf(stderr, "\tcode at         0x%08x\n",(int)(&newregion->contents));
  fflush(stderr);
#endif

#ifdef DEBUG2
  /** For debugging, it is convenient to make the first 8 instructions NOPs,
  *** so the disassembler can synchronize with the first real instruction
  *** of the template
  **/
  newregion->contents[0] =
    newregion->contents[1] =
    newregion->contents[2] =
    newregion->contents[3] =
    newregion->contents[4] =
    newregion->contents[5] =
    newregion->contents[6] =
    newregion->contents[7] = 0x90; /* NOP */
  newregion->current = 8;
#endif

#ifdef DEBUG
  fprintf(stderr,"CG_start finished\n");
  fflush(stderr);
#endif

  return newref;
}


int CG_dump(struct cg_ref *ref, template *tmpl)
{
  /* The type system ensures ref<>null<>tmpl */

  int i;
  int retval;
  cg_region *region = ref->region;

#ifdef DEBUG
  fprintf(stderr, "CG_dump\n");
  fprintf(stderr, "\tregion at              0x%p\n",(void *)region);
  fprintf(stderr, "\ttemplate at            0x%08x\n",(int)tmpl);
  fprintf(stderr, "\t&template->contents is 0x%08x\n",(int)(&tmpl->contents));
  fprintf(stderr, "\ttemplate length is 0x%x\n",tmpl->length);
  fprintf(stderr, "\ttemplate->contents[0-3]: 0x %02x %02x %02x %02x\n",
          tmpl->contents[0], tmpl->contents[1], tmpl->contents[2], tmpl->contents[3]);
  fflush(stderr);
#endif

  /* Expand the region if necessary */
  if (region->current + tmpl->length > region->length) {

    int new_length = region->length * 2;
    while (region->current + tmpl->length > new_length)
      new_length *= 2;

#ifdef DEBUG
    fprintf(stderr, "Increasing code region length:\n");
    fprintf(stderr, "\tcurrent ptr:       0x%p\n",(void *)region);
    fprintf(stderr, "\ttemplate length:   0x%x\n",tmpl->length);
    fprintf(stderr, "\told region length: 0x%x\n",region->length);
    fprintf(stderr, "\tnew region length: 0x%x\n",new_length);
    fflush(stderr);
#endif /* DEBUG */

    ref->region = region = GC_realloc((char *)region,
                                      sizeof(cg_region)+new_length);
    region->length = new_length;

#ifdef DEBUG
    fprintf(stderr, "\tnew region at:     0x%p\n",(void *)region);
    fflush(stderr);
#endif /* DEBUG */
  }


  PR("\tBeginning template copy.\n");
  for (i=0; i<tmpl->length; i++)
    region->contents[region->current+i] = tmpl->contents[i];
  PR("\tCompleted copy.\n");

  retval = region->current;
  region->current += tmpl->length;

#ifdef DEBUG
  fprintf(stderr, "CG_dump returning with 0x%08x\n",retval);
  fflush(stderr);
#endif

  return retval;
}

#ifdef DEBUG
/* For debugging */
void print_opcodes(cg_region *r) {
  int i;

  if (r == NULL) return;
  fprintf(stderr,"\t");
  for (i=0; i < r->current; i++) {
    fprintf(stderr, "%02.2x ",r->contents[i]);
    if ((i%8)==7) fprintf(stderr,"\n\t");
  }
  if ((i%8)!=0) fprintf(stderr,"\n");
  fflush(stderr);
}
#endif

void *CG_end(struct cg_ref *ref)
{
  /* Type system guarantees that ref is not null */
  void *retval;
  cg_region *region = ref->region;

#ifdef DEBUG
  fprintf(stderr, "CG_end\n");

  fprintf(stderr, "\tregion at 0x%08x\n",
          (int)region);
  fprintf(stderr, "\tregion->current is 0x%x\n",
          region->current);
  fprintf(stderr, "\tregion->length is 0x%x\n",
          region->length);
  fflush(stderr);
#endif

  retval = (void *)(&region->contents);

#ifdef DEBUG
  fprintf(stderr, "CG_end returning with 0x%08x, opcodes:\n",(int)retval);
  print_opcodes(region);
  fflush(stderr);
#endif

  return retval;


}

/* For the moment this has been changed to both fill and mark. */
void CG_mark(struct cg_ref *ref, void *x, int disp) {

  struct cons *cons;
  cg_region *region = ref->region;

#ifdef DEBUG
  fprintf(stderr, "CG_mark\n");

  fprintf(stderr, "\tregion at   0x%08x\n", (int)region);
  fprintf(stderr, "\thole offset 0x%08x\n", disp);
  fprintf(stderr, "\thole at     0x%08x\n", (int)(&region->contents[disp]));
  fprintf(stderr, "\thole value  0x%08x\n", (int)x);
  fprintf(stderr, "\tOPCODES BEFORE\n");
  print_opcodes(region);
  fflush(stderr);
#endif

  /* fill */
  /* XXX: 12 is a magic number to skip over fills,length,current */

  /*
  (*(void **)(region->contents[disp+12])) = x;
  */

  *(void**)(&region->contents[disp]) = x;

#ifdef DEBUG
  fprintf(stderr, "\tOPCODES AFTER\n");
  print_opcodes(region);
  fflush(stderr);
#endif

  /* mark */
  cons = GC_malloc(sizeof(struct cons));
  cons->head = x;
  cons->next = region->fills;
  region->fills = cons;

  PR("CG_mark finished\n");
}

#ifdef DEBUG
/* showmem and CG_fillitjc were used for debugging (Luke) */
void showmem(unsigned char *reg, int length)
{
  int i;

  for(i = 0; i < length; i += 4)
    fprintf(stderr, "mem[%x] 0x %02x %02x %02x %02x\n",
	    reg+i, reg[i], reg[i+1], reg[i+2], reg[i+3]);
}

void CG_fillitjc(template *holereg, template *holetmpllab, template *holelab,
		 template *targreg, template *targtmpllab, template *targlab)
{
  int fillval, *holeaddr;

  fprintf(stderr, "holereg: 0x %x  holetmplab: 0x %x  holelab: 0x %x\n",
	  holereg, holetmpllab, holelab);
  fprintf(stderr, "targreg: 0x %x  targtmplab: 0x %x  targlab: 0x %x\n",
	  targreg, targtmpllab, targlab);

  holeaddr = (int *) ((int)holereg + ((int)holelab - (int)holetmpllab) + 1);
  fillval = ((int)targreg - (int)holereg)
    + ((int)targlab - (int)targtmpllab)
    - ((int)holelab - (int)holetmpllab)
    - 9;

  showmem((unsigned char *)holereg, holetmpllab->length);
  fprintf(stderr, ">mem[%x] = %x \n", holeaddr, *holeaddr);
  *holeaddr = fillval;
  showmem((unsigned char *)holereg, holetmpllab->length);
  fprintf(stderr, ">mem[%x] = %x \n", holeaddr, *holeaddr);

  fflush(stderr);
}
#endif

/***** loader initialization *****/

#ifdef __linux__
#define SYMNAME(s) s
#else
#define SYMNAME(s) "_" ## s
#endif

void
cyclonelib_init_loader_syms(void (*register_sym)(long,long))
{
  register_sym((long)CG_dump,(long)SYMNAME("CG_dump"));
  register_sym((long)CG_end,(long)SYMNAME("CG_end"));
  register_sym((long)CG_mark,(long)SYMNAME("CG_mark"));
  register_sym((long)CG_start,(long)SYMNAME("CG_start"));
  register_sym((long)external_addr,(long)SYMNAME("external_addr"));
}

/* EOF: cyclonelib.c */
