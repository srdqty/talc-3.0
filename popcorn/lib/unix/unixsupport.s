	.file	"unixsupport.c"
	.version	"01.01"
gcc2_compiled.:
.text
	.align 4
.globl convert_flags
	.type	 convert_flags,@function
convert_flags:
	pushl %ebp
	movl %esp,%ebp
	subl $8,%esp
	pushl %ebx
	movl $0,-8(%ebp)
	movl $0,-4(%ebp)
.L2:
	movl 8(%ebp),%eax
	movl -4(%ebp),%edx
	cmpl %edx,(%eax)
	jg .L5
	jmp .L3
	.align 4
.L5:
	movl 8(%ebp),%eax
	movl -4(%ebp),%edx
	movl %edx,%ecx
	leal 0(,%ecx,4),%edx
	movl 4(%eax),%eax
	movl (%eax,%edx),%edx
	movl %edx,%eax
	leal 0(,%eax,4),%edx
	movl %edx,%eax
	addl 12(%ebp),%eax
	leal -4(%eax),%edx
	movl (%edx),%ebx
	orl %ebx,-8(%ebp)
.L4:
	incl -4(%ebp)
	jmp .L2
	.align 4
.L3:
	movl -8(%ebp),%eax
	jmp .L1
	.align 4
.L1:
	movl -12(%ebp),%ebx
	leave
	ret
.Lfe1:
	.size	 convert_flags,.Lfe1-convert_flags
	.align 4
.globl copy_array
	.type	 copy_array,@function
copy_array:
	pushl %ebp
	movl %esp,%ebp
	subl $12,%esp
	pushl %ebx
	movl $0,-4(%ebp)
.L7:
	movl -4(%ebp),%eax
	movl %eax,%edx
	leal 0(,%edx,4),%eax
	movl 12(%ebp),%edx
	cmpl $0,(%edx,%eax)
	jne .L9
	jmp .L8
	.align 4
.L9:
	incl -4(%ebp)
	jmp .L7
	.align 4
.L8:
	cmpl $0,-4(%ebp)
	jne .L10
	pushl $4
	call GC_malloc
	addl $4,%esp
	movl %eax,-12(%ebp)
	movl -12(%ebp),%eax
	movl $0,(%eax)
	movl -12(%ebp),%eax
	jmp .L6
	.align 4
	jmp .L11
	.align 4
.L10:
	pushl $4
	call GC_malloc
	addl $4,%esp
	movl %eax,-12(%ebp)
	movl -12(%ebp),%eax
	movl -4(%ebp),%edx
	movl %edx,(%eax)
	movl -4(%ebp),%eax
	movl %eax,%edx
	leal 0(,%edx,4),%eax
	pushl %eax
	call GC_malloc
	addl $4,%esp
	movl %eax,%eax
	movl -12(%ebp),%edx
	movl %eax,4(%edx)
	movl $0,-8(%ebp)
.L12:
	movl -8(%ebp),%eax
	cmpl %eax,-4(%ebp)
	jg .L15
	jmp .L13
	.align 4
.L15:
	movl -8(%ebp),%eax
	movl %eax,%edx
	leal 0(,%edx,4),%eax
	movl 12(%ebp),%edx
	movl (%edx,%eax),%eax
	pushl %eax
	movl 8(%ebp),%ebx
	call *%ebx
	addl $4,%esp
	movl %eax,%eax
	movl -12(%ebp),%edx
	movl -8(%ebp),%ecx
	movl %ecx,%ebx
	leal 0(,%ebx,4),%ecx
	movl 4(%edx),%edx
	movl %eax,(%edx,%ecx)
.L14:
	incl -8(%ebp)
	jmp .L12
	.align 4
.L13:
	movl -12(%ebp),%eax
	jmp .L6
	.align 4
.L11:
.L6:
	movl -16(%ebp),%ebx
	leave
	ret
.Lfe2:
	.size	 copy_array,.Lfe2-copy_array
	.align 4
.globl copy_string_array
	.type	 copy_string_array,@function
copy_string_array:
	pushl %ebp
	movl %esp,%ebp
	movl 8(%ebp),%eax
	pushl %eax
	pushl $Cstring_to_string
	call copy_array
	addl $8,%esp
	movl %eax,%edx
	movl %edx,%eax
	jmp .L16
	.align 4
.L16:
	leave
	ret
.Lfe3:
	.size	 copy_string_array,.Lfe3-copy_string_array
.data
	.align 4
	.type	 unix_exn.8,@object
	.size	 unix_exn.8,4
unix_exn.8:
	.long 0
.section	.rodata
.LC0:
	.string	"%s:%d"
.text
	.align 4
.globl get_unix_error
	.type	 get_unix_error,@function
get_unix_error:
	pushl %ebp
	movl %esp,%ebp
	subl $16,%esp
	pushl %ebx
	pushl $100
	call GC_malloc_atomic
	addl $4,%esp
	movl %eax,-4(%ebp)
	movl 12(%ebp),%eax
	pushl %eax
	movl 8(%ebp),%eax
	pushl %eax
	pushl $.LC0
	movl -4(%ebp),%eax
	pushl %eax
	call sprintf
	addl $16,%esp
	cmpl $0,unix_exn.8
	jne .L18
	pushl $8
	call GC_malloc
	addl $4,%esp
	movl %eax,-8(%ebp)
	pushl $8
	call GC_malloc
	addl $4,%esp
	movl %eax,-12(%ebp)
	pushl $8
	call GC_malloc
	addl $4,%esp
	movl %eax,-16(%ebp)
	pushl $12
	call GC_malloc
	addl $4,%esp
	movl %eax,unix_exn.8
	movl unix_exn.8,%eax
	movl Unix_error?exn,%edx
	movl %edx,(%eax)
	movl unix_exn.8,%eax
	movl -12(%ebp),%edx
	movl %edx,4(%eax)
	movl unix_exn.8,%eax
	movl -8(%ebp),%edx
	movl %edx,8(%eax)
.L18:
	movl unix_exn.8,%eax
	movl 4(%eax),%ebx
	movl -4(%ebp),%eax
	pushl %eax
	call strlen
	addl $4,%esp
	movl %eax,(%ebx)
	movl unix_exn.8,%eax
	movl 4(%eax),%edx
	movl -4(%ebp),%eax
	movl %eax,4(%edx)
	call __errno_location
	movl %eax,%edx
	movl %edx,%eax
	movl unix_exn.8,%edx
	movl 8(%edx),%ecx
	movl (%eax),%eax
	movl %eax,(%ecx)
	movl unix_exn.8,%eax
	movl 8(%eax),%edx
	movl 4(%edx),%ebx
	movl 16(%ebp),%eax
	pushl %eax
	call strlen
	addl $4,%esp
	movl %eax,(%ebx)
	movl unix_exn.8,%eax
	movl 8(%eax),%edx
	movl 4(%edx),%eax
	movl 16(%ebp),%edx
	movl %edx,4(%eax)
	movl unix_exn.8,%eax
	jmp .L17
	.align 4
.L17:
	movl -20(%ebp),%ebx
	leave
	ret
.Lfe4:
	.size	 get_unix_error,.Lfe4-get_unix_error
	.ident	"GCC: (GNU) 2.7.2.3"
