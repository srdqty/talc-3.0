/* Machine-specific elf macros for i386 et al.  */
#ident "$Id: elf_i386.h,v 1.2 2001/02/08 00:24:01 fms Exp $"

#define ELFCLASSM	ELFCLASS32
#define ELFDATAM	ELFDATA2LSB

#ifndef EM_486
#define MATCH_MACHINE(x)  (x == EM_386)
#else
#define MATCH_MACHINE(x)  (x == EM_386 || x == EM_486)
#endif

#define SHT_RELM	SHT_REL
#define Elf32_RelM	Elf32_Rel
