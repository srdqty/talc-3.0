#define data32	.byte 0x66
#define addr32	.byte 0x67

#define ALIGN 4
#ifdef __ELF__
#define EXT(x)  x
#define LEXT(x) x## :
#else
#define EXT(x)  _##x
#define LEXT(x) _##x## :
#endif
#define	ENTRY(x)	.globl EXT(x); .align ALIGN; LEXT(x)
#define IOSTALL   outb %al,$0x80


ENTRY(rdtsr)
	pushl	%ebp
	movl	%esp,%ebp
	
	xorl	%eax,%eax
	xorl	%edx,%edx
	.byte 0x0f
	.byte 0x31		/* RDTSC instr - results to %edx:%eax */

	leave
	ret
