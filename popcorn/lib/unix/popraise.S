	.text

/* RAISE A POPCORN EXCEPTION FROM C */

	.align 4
	.globl	raise_pop_exception
	
	/* This should only be called from the C frame immediately
	   following the Popcorn frame, otherwise the restoring of
	   %ebp won't work */
raise_pop_exception:
	movl	4(%esp), %eax	/* get exn value */
	movl    0(%ebp), %ebp	/* restore %ebp	*/
	movl	%ebp, %esp	/* switch to the exn stack */
	ret			/* jump to the top value on the stack */

	.align 4
	.globl	exn_handler

/* SIGNAL HANDLING */
	
#define RESTORE()                               \
        popl    %ebx;                           \
        popl    %edi;                           \
        popl    %esi;                           \
        popl    %ebp;

	.align 4

	/* if an exception is thrown in the handler, ignore it---
	   just restore the callee-save registers and go back to the
	   C code */
exn_handler:
	RESTORE()
	ret
	
	.align 4
	.globl	call_sig_handler

call_sig_handler:
	pushl	%ebp                      # save ebp for C
        pushl	%esi                      # Save callee-save, sp-=4.
        pushl	%edi                      # Save callee-save, sp-=4.
        pushl	%ebx                      # Save callee-save, sp-=4.
        pushl	$exn_handler              # set up exn handler
        movl	%esp,%ebp
	pushl	24(%esp)                  # signal argument
	movl	32(%esp),%ebx             # fun to jump to
	call	*%ebx
	addl	$8,%esp                   # remove exn handler & args
	RESTORE()                         # restore callee-saves registers
	ret                               # done
