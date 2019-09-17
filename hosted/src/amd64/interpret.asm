bits 64

%include "src/amd64/macros.inc"

global interpret

[section .text]

; Interprets the string in the source buffer, then jumps to the address in rsi.
interpret:
	; Push rsi to the return stack.
	sub rbp, 8
	mov [rbp], rsi

	; Try to lex out a word.
	mov r8, [r15+16]
	mov r9, [r15+24]
	call lex_word
	mov [r15+24], r9
	lea r9, [r8+rcx]
	mov [r15+16], r9

	; If we're out of words, jump to rsi.
	test rcx, rcx
	jnz .find_word
	add rbp, 8
	jmp rsi

.find_word:
	xchg rcx, r9 ; Remaining length is in rcx.
	call find_word

.die:
	dbg `hit interpret.die\n`
	jmp .die

; Finds the unsmudged word whose name's address is given by r8 and whose name's
; length is given by rcx, returning its address in r9. Returns 0 in r9 if none
; exists. Trashes rdx, r10.
find_word:
	mov r9, [r15]
	xor r10d, r10d
.word_loop:
	mov r10w, [r9+8]
	test r10w, 0x0200
	jnz .continue

	cmp cl, r10b
	jne .continue

	xor r10d, r10d
.char_loop:
	cmp r10, rcx
	je .end

	mov dl, [r8+r10]
	shl dx, 8
	mov dl, [r9+10+r10]
	inc r10
	cmp dh, dl
	je .char_loop

.continue:
	mov r9, [r9]
	test r9, r9
	jnz .word_loop
.end:
	ret

; Lexes out a word from the buffer whose address is given by r8 and whose
; length is given by r9. Returns the address of the start of the token in r8,
; and the length in rcx. Returns the number of remaining characters in the
; buffer in r9. The length will be 0 when no words remain. Trashes rax.
;
; INVARIANT: r8_initial + r9_initial = r8_final + r9_final + rcx_final
lex_word:
	; Advance r8 and decrement r9 until is_space returns false or r9 is zero.
.skip_spaces_loop:
	test r9, r9
	jz .no_remaining_words

	mov al, [r8]
	call is_space
	test al, al
	jz .find_end_of_word

	inc r8
	dec r9
	jmp .skip_spaces_loop

.find_end_of_word:
	xor ecx, ecx

	; Increment rcx and decrement r9 until is_space returns true or r9 is zero.
.find_end_of_word_loop:
	cmp rcx, r9
	je .end

	mov al, [r8+rcx]
	call is_space
	test al, al
	jnz .end

	inc rcx
	jmp .find_end_of_word_loop

.no_remaining_words:
	xor ecx, ecx

.end:
	sub r9, rcx
	ret

; Checks if the character in al is a whitespace character. al is zero if it is
; not. Trashes rax.
;
; Supported whitespace characters are newline, tab, and space.
is_space:
	and rax, 0xff
	lea rax, [rax-9]
	cmp al, 1
	setbe ah
	cmp al, 0x17
	sete al
	or al, ah
	ret

; vi: cc=80 ft=nasm
