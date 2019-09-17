bits 64

global _start
extern ipb.here

[section .text]

_start:
	; sys_brk(NULL) -- note that this returns the current break (see NOTES of
	; the manpage)
	mov eax, 12
	xor edi, edi
	syscall

	; Increase the break by a meg, and use it as HERE.
	lea rdi, [rax+0x100000]
	mov qword [ipb.here], rdi
	mov eax, 12
	syscall

	mov eax, 60
	mov edi, 0x5a
	syscall
