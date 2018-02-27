#include "obj.h"
#include "util.h"

void
print_filehdr(struct external_filehdr *h)
{
  printf("COFF File Header:\n");
  printf("  f_magic = %d\n",h->f_magic);
  printf("  f_nscns = %d\n",h->f_nscns);
  printf("  f_timdat = %d\n",h->f_timdat);
  printf("  f_symptr = %d\n",h->f_symptr);
  printf("  f_nsyms = %d\n",h->f_nsyms);
  printf("  f_opthdr = %d\n",h->f_opthdr);
  printf("  f_flags = %d\n",h->f_flags);
  fflush(stdout);
}

void
print_external_scnhdr (struct external_scnhdr *h)
{
  printf("Section Header:\n");
  printf("  s_name = %s\n",h->s_name);
  printf("  s_paddr = %d\n",h->s_paddr);
  printf("  s_vaddr = %d\n",h->s_vaddr);
  printf("  s_size = %d\n",h->s_size);
  printf("  s_scnptr = %d\n",h->s_scnptr);
  printf("  s_relptr = %d\n",h->s_relptr);
  printf("  s_lnnoptr = %d\n",h->s_lnnoptr);
  printf("  s_nreloc = %d\n",h->s_nreloc);
  printf("  s_nlnno = %d\n",h->s_nlnno);
  printf("  s_flags = %d\n",h->s_flags);
  fflush(stdout);
}


