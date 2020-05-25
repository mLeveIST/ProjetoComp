	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 13
	.globl	_power                  ## -- Begin function power
	.p2align	4, 0x90
_power:                                 ## @power
	.cfi_startproc
## %bb.0:
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset %ebp, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register %ebp
	subl	$16, %esp
	movl	12(%ebp), %eax
	movl	8(%ebp), %ecx
	movl	12(%ebp), %edx
	movl	%edx, -4(%ebp)
	movl	$1, -8(%ebp)
	movl	%eax, -12(%ebp)         ## 4-byte Spill
	movl	%ecx, -16(%ebp)         ## 4-byte Spill
LBB0_1:                                 ## =>This Inner Loop Header: Depth=1
	cmpl	$0, -4(%ebp)
	jle	LBB0_3
## %bb.2:                               ##   in Loop: Header=BB0_1 Depth=1
	movl	8(%ebp), %eax
	imull	-8(%ebp), %eax
	movl	%eax, -8(%ebp)
	movl	-4(%ebp), %eax
	addl	$-1, %eax
	movl	%eax, -4(%ebp)
	jmp	LBB0_1
LBB0_3:
	movl	-8(%ebp), %eax
	addl	$16, %esp
	popl	%ebp
	retl
	.cfi_endproc
                                        ## -- End function
	.globl	_strcompr               ## -- Begin function strcompr
	.p2align	4, 0x90
_strcompr:                              ## @strcompr
	.cfi_startproc
## %bb.0:
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset %ebp, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register %ebp
	pushl	%esi
	subl	$16, %esp
	.cfi_offset %esi, -12
	movl	12(%ebp), %eax
	movl	8(%ebp), %ecx
	movl	%eax, -16(%ebp)         ## 4-byte Spill
	movl	%ecx, -20(%ebp)         ## 4-byte Spill
LBB1_1:                                 ## =>This Inner Loop Header: Depth=1
	movl	8(%ebp), %eax
	movl	%eax, %ecx
	addl	$1, %ecx
	movl	%ecx, 8(%ebp)
	movb	(%eax), %dl
	movb	%dl, -9(%ebp)
	movl	12(%ebp), %eax
	movl	%eax, %ecx
	addl	$1, %ecx
	movl	%ecx, 12(%ebp)
	movb	(%eax), %dl
	movb	%dl, -10(%ebp)
	movzbl	-9(%ebp), %eax
	movzbl	-10(%ebp), %ecx
	cmpl	%ecx, %eax
	je	LBB1_3
## %bb.2:
	movl	$1, %eax
	movl	$4294967295, %ecx       ## imm = 0xFFFFFFFF
	movzbl	-9(%ebp), %edx
	movzbl	-10(%ebp), %esi
	cmpl	%esi, %edx
	cmovll	%ecx, %eax
	movl	%eax, -8(%ebp)
	jmp	LBB1_7
LBB1_3:                                 ##   in Loop: Header=BB1_1 Depth=1
	cmpb	$0, -9(%ebp)
	jne	LBB1_5
## %bb.4:
	jmp	LBB1_6
LBB1_5:                                 ##   in Loop: Header=BB1_1 Depth=1
	jmp	LBB1_1
LBB1_6:
	movl	$0, -8(%ebp)
LBB1_7:
	movl	-8(%ebp), %eax
	addl	$16, %esp
	popl	%esi
	popl	%ebp
	retl
	.cfi_endproc
                                        ## -- End function

.subsections_via_symbols
