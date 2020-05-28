; GLOBL
global	$_exemplo:function
; TEXT
segment	.text
; ALIGN
align	4
; LABEL
$_exemplo:
; ENTER
	push	ebp
	mov	ebp, esp
	sub	esp, 0
; LOCAL
	lea	eax, [ebp+8]
	push	eax
; LOAD
	pop	eax
	push	dword [eax]
; IMM
	push	dword 12
; LE
	pop	eax
	xor	ecx, ecx
	cmp	[esp], eax
	setle	cl
	mov	[esp], ecx
; POP
	pop	eax
; LEAVE
	leave
; RET
	ret
; IMM
	push	dword 0
; POP
	pop	eax
; LEAVE
	leave
; RET
	ret
