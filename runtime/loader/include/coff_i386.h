#ifndef __COFF_I386_H__
#define __COFF_I386_H__

/*** coff information for Intel 386/486.  */


/********************** FILE HEADER **********************/

/* Assume
   sizeof(unsigned short) == 2
   sizeof(unsigned int) = 4 */

struct external_filehdr {
	unsigned short f_magic;	/* magic number			*/
	unsigned short f_nscns;	/* number of sections		*/
	unsigned int f_timdat;	/* time & date stamp		*/
	unsigned int f_symptr;	/* file pointer to symtab	*/
	unsigned int f_nsyms;	/* number of symtab entries	*/
	unsigned short f_opthdr;	/* sizeof(optional hdr)		*/
	unsigned short f_flags;	/* flags			*/
};



/* Bits for f_flags:
 *	F_RELFLG	relocation info stripped from file
 *	F_EXEC		file is executable (no unresolved external references)
 *	F_LNNO		line numbers stripped from file
 *	F_LSYMS		local symbols stripped from file
 *	F_AR32WR	file has byte ordering of an AR32WR machine (e.g. vax)
 */

#define F_RELFLG	(0x0001)
#define F_EXEC		(0x0002)
#define F_LNNO		(0x0004)
#define F_LSYMS		(0x0008)

#define	I386MAGIC	0x14c
#define I386PTXMAGIC	0x154
#define I386AIXMAGIC	0x175

#define CHECK_FLAG(x,f) ((((x).f_flags) & (f)) == (f))

/* This is Lynx's all-platform magic number for executables. */

#define LYNXCOFFMAGIC	0415

#define I386BADMAG(x) (((x).f_magic != I386MAGIC) \
		       && (x).f_magic != I386AIXMAGIC \
		       && (x).f_magic != I386PTXMAGIC \
		       && (x).f_magic != LYNXCOFFMAGIC)

#define	FILHDR	struct external_filehdr
#define	FILHSZ	20


/********************** AOUT "OPTIONAL HEADER" **********************/


typedef struct 
{
  char 	magic[2];		/* type of file				*/
  char	vstamp[2];		/* version stamp			*/
  char	tsize[4];		/* text size in bytes, padded to FW bdry*/
  char	dsize[4];		/* initialized data "  "		*/
  char	bsize[4];		/* uninitialized data "   "		*/
  char	entry[4];		/* entry pt.				*/
  char 	text_start[4];		/* base of text used for this file */
  char 	data_start[4];		/* base of data used for this file */


}
AOUTHDR;


#define AOUTSZ 28
#define AOUTHDRSZ 28

#define OMAGIC          0404    /* object files, eg as output */
#define ZMAGIC          0413    /* demand load format, eg normal ld output */
#define STMAGIC		0401	/* target shlib */
#define SHMAGIC		0443	/* host   shlib */


/* define some NT default values */
/*  #define NT_IMAGE_BASE        0x400000 moved to internal.h */
#define NT_SECTION_ALIGNMENT 0x1000
#define NT_FILE_ALIGNMENT    0x200
#define NT_DEF_RESERVE       0x100000
#define NT_DEF_COMMIT        0x1000

/********************** SECTION HEADER **********************/


struct external_scnhdr {
	char		s_name[8];	/* section name			*/
	unsigned int 	s_paddr;	/* physical address, aliased s_nlib */
	unsigned int 	s_vaddr;	/* virtual address		*/
	unsigned int 	s_size;	/* section size			*/
	unsigned int 	s_scnptr;	/* file ptr to raw data for section */
	unsigned int 	s_relptr;	/* file ptr to relocation	*/
	unsigned int 	s_lnnoptr;	/* file ptr to line numbers	*/
	unsigned short 	s_nreloc;	/* number of relocation entries	*/
	unsigned short 	s_nlnno;	/* number of line number entries*/
	unsigned int 	s_flags;	/* flags			*/
};

#define	SCNHDR	struct external_scnhdr
#define	SCNHSZ	40

/*
 * names of "special" sections
 */
#define _TEXT	".text"
#define _DATA	".data"
#define _BSS	".bss"
#define _COMMENT ".comment"
#define _LIB ".lib"

/* flags */

#define ALIGN(f) (1 << ((((f) >> 20) & 0xf)-1))

#define SFLAG_EXECUTE    0x20000000
#define SFLAG_READ       0x40000000
#define SFLAG_WRITE      0x80000000
#define SFLAG_LNK_REMOVE 0x00000800

/********************** LINE NUMBERS **********************/

/* 1 line number entry for every "breakpointable" source line in a section.
 * Line numbers are grouped on a per function basis; first entry in a function
 * grouping will have l_lnno = 0 and in place of physical address will be the
 * symbol table index of the function name.
 */
struct external_lineno {
	union {
		char l_symndx[4];	/* function name symbol index, iff l_lnno == 0*/
		char l_paddr[4];	/* (physical) address of line number	*/
	} l_addr;
	char l_lnno[2];	/* line number		*/
};


#define	LINENO	struct external_lineno
#define	LINESZ	6


/********************** SYMBOLS **********************/

#define E_SYMNMLEN	8	/* # characters in a symbol name	*/
#define E_FILNMLEN	14	/* # characters in a file name		*/
#define E_DIMNUM	4	/* # array dimensions in auxiliary entry */

struct external_syment 
{
  union {
    char e_name[E_SYMNMLEN];
    struct {
      char e_zeroes[4];
      unsigned int e_offset;
    } e;
  } e;
  unsigned int e_value;
           short e_scnum;
  unsigned short e_type;
  unsigned char e_sclass;
  unsigned char e_numaux;
};

#define CLASS_STATIC    3
#define CLASS_EXTERNAL  2
#define CLASS_FILE      103
#define CLASS_WEAKEXT   108
#define TYPE_NULL       0
#define SECTION_UNDEF   0

#define N_BTMASK	(0xf)
#define N_TMASK		(0x30)
#define N_BTSHFT	(4)
#define N_TSHIFT	(2)
  
union external_auxent {
	struct {
		char x_tagndx[4];	/* str, un, or enum tag indx */
		union {
			struct {
			    char  x_lnno[2]; /* declaration line number */
			    char  x_size[2]; /* str/union/array size */
			} x_lnsz;
			char x_fsize[4];	/* size of function */
		} x_misc;
		union {
			struct {		/* if ISFCN, tag, or .bb */
			    char x_lnnoptr[4];	/* ptr to fcn line # */
			    char x_endndx[4];	/* entry ndx past block end */
			} x_fcn;
			struct {		/* if ISARY, up to 4 dimen. */
			    char x_dimen[E_DIMNUM][2];
			} x_ary;
		} x_fcnary;
		char x_tvndx[2];		/* tv index */
	} x_sym;

	union {
		char x_fname[E_FILNMLEN];
		struct {
			char x_zeroes[4];
			char x_offset[4];
		} x_n;
	} x_file;

	struct {
		char x_scnlen[4];	/* section length */
		char x_nreloc[2];	/* # relocation entries */
		char x_nlinno[2];	/* # line numbers */
		char x_checksum[4];	/* section COMDAT checksum */
		char x_associated[2];	/* COMDAT associated section index */
		char x_comdat[1];	/* COMDAT selection number */
	} x_scn;

        struct {
		char x_tvfill[4];	/* tv fill value */
		char x_tvlen[2];	/* length of .tv */
		char x_tvran[2][2];	/* tv range */
	} x_tv;		/* info about .tv section (in auxent of symbol .tv)) */


};

#define	SYMENT	struct external_syment
#define	SYMESZ	18	
#define	AUXENT	union external_auxent
#define	AUXESZ	18


#	define _ETEXT	"etext"


/********************** RELOCATION DIRECTIVES **********************/



struct external_reloc {
  unsigned int r_vaddr;
  unsigned int r_symndx;
  unsigned short r_type;
};


#define RELOC struct external_reloc
#define RELSZ 10

#define RELOC_REL32    0x14
#define RELOC_DIR32    0x06

#endif /* __COFF_I386_H__ */
