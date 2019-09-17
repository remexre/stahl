bits 64

global to_number

[section .text]

; default base = rdi
; len = rcx
; addr = r8
; number is in rax as a qword. success flag is written to [r9] as a byte (0/1).
to_number:
	xor eax, eax

	test rcx, rcx
	jz .fail

.test_for_base:
	mov dl, [r8]
	cmp dl, '#'
	je .base_dec
	cmp dl, '$'
	je .base_hex

.test_for_negative:
	xor r10d, r10d
	mov dl, [r8]
	cmp dl, '-'
	je .negative

.loop:
	mul rdi

	mov dl, [r8]
	and rdx, 0xff
	cmp dl, '0'
	jb .fail
	cmp dl, '9'
	ja .hex

	sub dl, '0'
	jmp .loopend

.hex:
	cmp dl, 'A'
	jb .fail
	cmp dl, 'F'
	ja .hex_lower
	sub dl, 'A'-10
	jmp .loopend

.hex_lower:
	cmp dl, 'a'
	jb .fail
	cmp dl, 'f'
	ja .fail
	sub dl, 'a'-10
	; jmp .loopend ; Fall through instead

.loopend:
	; Check the digit was legal in this base.
	cmp rdi, rdx
	jbe .fail

	; Add this character in.
	add rax, rdx

	; Go to the next character.
	inc r8
	dec rcx
	jz .success
	jmp .loop

.success:
	test r10, r10
	jz .real_success
	neg rax
.real_success:
	mov byte [r9], 1
	ret
.fail:
	mov byte [r9], 0
	ret

.base_dec:
	mov rdi, 10
	inc r8
	dec rcx
	jz .fail
	jmp .test_for_negative

.base_hex:
	mov rdi, 16
	inc r8
	dec rcx
	jz .fail
	jmp .test_for_negative

.negative:
	inc r10
	inc r8
	dec rcx
	jz .fail
	jmp .loop

; vi: cc=80 ft=nasm
