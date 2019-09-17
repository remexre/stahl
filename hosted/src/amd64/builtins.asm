bits 64

%include "src/amd64/chacha20.inc"
%include "src/amd64/macros.inc"

extern aes_decrypt
extern aes_encrypt
extern aes_mode_decrypt
extern aes_mode_encrypt
extern allot_overflow
extern ipb
extern ipb.here
extern ipb.quantum
extern ipb.quantum_max
extern to_number
extern underflow
extern underflow_return

global forth_docolon.impl
global forth_last_builtin

[section .text]

defcode abs, "ABS", 1, 0x00, "n -- u"
	test rbx, rbx
	jns .end
	neg rbx
.end:
endcode

defcode add, "+", 2, 0x00, "x y -- x+y"
	pop rax
	add rbx, rax
endcode

defcode add_store, "+!", 2, 0x00, "n addr --"
	pop rax
	add [rbx], rax
	pop rbx
endcode

; This is just in assembly for convenience (oddly enough). 3 instructions of
; assembly versus 7 words (shortest I could find was DUP >R - SWAP R> + SWAP).
defcode adjust_string, "/STRING", 3, 0x00, "addr u n -- addr+n u-n"
	sub [rsp], rbx
	add [rsp+8], rbx
	pop rbx
endcode

defcode allot, "ALLOT", 1, 0x00, "n -- "
	mov rax, rbx
	xadd [ipb.here], rax
	add rbx, rax
	cmp rbx, 0x300000
	jae allot_overflow
	pop rbx
endcode

defcode and, "AND", 2, 0x00, "x y -- x&y"
	pop rax
	and rbx, rax
endcode

defcode arith_right_shift, "ARSHIFT", 2, 0x00, "x y -- x>>>y"
	mov rcx, rbx
	pop rbx
	sar rbx, cl
endcode

defcode base_decimal, "DECIMAL", 0, 0x00, "--"
	and byte [r15+0x28], 0xfe
endcode

defcode base_hex, "HEX", 0, 0x00, "--"
	or byte [r15+0x28], 0x01
endcode

defcode cell, "CELL", 0, 0x00, "-- u"
	push rbx
	mov rbx, 8
endcode

defcode cells, "CELLS", 1, 0x00, "u -- u"
	shl rbx, 3
endcode

defcode cell_minus, "CELL-", 1, 0x00, "u -- u"
	sub rbx, 8
endcode

defcode cell_plus, "CELL+", 1, 0x00, "u -- u"
	add rbx, 8
endcode

defcode chacha20, "CHACHA20", 1, 0x00, "block-addr out-addr --"
	mov rdx, rbx
	pop rax
	pop rbx

	movdqa xmm5, [chacha20_mask8]
	movdqa xmm6, [chacha20_mask16]

	movdqa xmm0, [rax]
	movdqa xmm1, [rax+16]
	movdqa xmm2, [rax+32]
	movdqa xmm3, [rax+48]

	chacha20_twenty_rounds xmm0, xmm1, xmm2, xmm3, xmm4, xmm5, xmm6

	paddd xmm0, [rax]
	paddd xmm1, [rax+16]
	paddd xmm2, [rax+32]
	paddd xmm3, [rax+48]

	movdqa [rdx],    xmm0
	movdqa [rdx+16], xmm1
	movdqa [rdx+32], xmm2
	movdqa [rdx+48], xmm3
endcode

defcode context_switch, "CONTEXT-SWITCH", 1, 0x00, "addr --"
	mov [r15+0xe8], rsi
	mov [r15+0xf0], rsp
	mov [r15+0xf8], rbp
	mov r15, rbx
	mov rsi, [r15+0xe8]
	mov rsp, [r15+0xf0]
	mov rbp, [r15+0xf8]
	lea r13, [r15+0x400]
	lea r14, [r15+0x200]
	mov rax, [ipb.quantum_max]
	mov [ipb.quantum], rax
	pop rbx
endcode

defcode decr, "1-", 1, 0x00, "n -- n"
	dec rbx
endcode

defcode depth, "DEPTH", 0, 0x00, "u_k ... u_0 -- u_k ... u_0 k"
	push rbx
	mov rbx, r13
	sub rbx, rsp
	shr rbx, 3
	dec rbx
endcode

defcode docolon, "((DOCOLON))"
	jmp near .impl
.jmp_len equ $ - .cfa
.pfa:
	dq forth_literal_impl.cfa
	dq .impl
	dq forth_exit.cfa
.impl:
	sub rbp, 8
	mov [rbp], rsi
	lea rsi, [rax+.jmp_len]
endcode

defcode dodoes, "((DODOES))"
	mov rcx, [rsp]
	mov [rsp], rbx
	lea  rbx, [rax+forth_docolon.jmp_len]
	sub rbp, 8
	mov [rbp], rsi
	mov rsi, rcx
endcode

defcode dovar, "((DOVAR))"
	push rbx
	lea rbx, [rax+5]
endcode

defcode drop, "DROP", 1, 0x00, "x --"
	pop rbx
endcode

defcode dup, "DUP", 1, 0x00, "x -- x x"
	push rbx
endcode

defcode dup_nonzero, "?DUP", 1, 0x00, "x -- 0 | x x"
	test rbx, rbx
	jz .exit
	push rbx
.exit:
endcode

defcode empty_return_stack, "EMPTY-RETURN-STACK", 0, 0x00, "R: u_k ... u_0 --"
	lea rbp, [r14-8]
endcode

defcode equal, "=", 2, 0x00, "x y -- flag"
	pop rax
	xor edx, edx
	cmp rax, rbx
	setnz dl
	dec rdx
	mov rbx, rdx
endcode

defcode erase, "ERASE", 2, 0x00, "addr len --"
	pop rdi
	mov rcx, rbx
	xor al, al
	rep stosb
	pop rbx
endcode

defcode execute, "EXECUTE", 1, 0x00, "i*x xt -- j*x"
	mov rax, rbx
	pop rbx
	jmp rax
endcode

defcode exit, "EXIT"
	; Check for return underflow.
	lea rcx, [rbp+8]
	cmp r14, rcx
	jb underflow_return

	mov rsi, [rbp]
	add rbp, 8
endcode

defcode false, "FALSE", 0, 0x00, "-- 0"
	push rbx
	xor ebx, ebx
endcode

defcode fetch, "@", 1, 0x00, "addr -- x"
	mov rbx, [rbx]
endcode

defcode fetch_char, "C@", 1, 0x00, "addr -- char"
	movzx rbx, byte [rbx]
endcode

defcode fetch_dword, "D@", 1, 0x00, "addr -- dword"
	mov ebx, [rbx]
endcode

defcode fetch_word, "W@", 1, 0x00, "addr -- word"
	movzx rbx, word [rbx]
endcode

defcode fnv1a, "FNV1A", 2, 0x00, "addr len -- hash"
	xor r9, r9 ; idx
	pop rdi ; addr
	mov rax, 0xcbf29ce484222325 ; hash
	mov r8, 1099511628211 ; prime

.loop:
	cmp rbx, r9
	je .end
	xor al, [rdi+r9]
	mul r8
	inc r9
	jmp .loop

.end:
	mov rbx, rax
endcode

defcode from_r, "R>"
	; Check for return underflow.
	lea rcx, [rbp+8]
	cmp r14, rcx
	jb underflow_return

	push rbx
	mov rbx, [rbp]
	add rbp, 8
endcode

defcode from_r_2, "2R>"
	; Check for return underflow.
	lea rcx, [rbp+16]
	cmp r14, rcx
	jb underflow_return

	sub rsp, 16
	mov [rsp+8], rbx
	mov rax, [rbp+8]
	mov [rsp], rax
	mov rbx, [rbp]
	add rbp, 16
endcode

defcode get_base, "GET-BASE", 0, 0x00, "-- base"
	test byte [r15+0x28], 0x01
	push rbx
	mov rbx, 10
	mov rax, 16
	cmovnz rbx, rax
endcode

defcode get_state, "GET-STATE", 0, 0x00, "-- flag"
	push rbx
	xor ebx, ebx
	test byte [r15+0x28], 0x02
	setz bl
	dec rbx
endcode

defcode greater, ">", 2, 0x00, "n1 n2 -- n1>n2"
	pop rax
	xor edx, edx
	cmp rax, rbx
	setle dl
	dec rdx
	mov rbx, rdx
endcode

defcode here, "HERE", 0, 0x00, "-- addr"
	push rbx
	mov rbx, [ipb.here]
endcode

defcode hlt, "HLT", 0, 0x00, "--"
	hlt
endcode

defcode if_impl, "(IF)", 1
	test rbx, rbx
	pop rbx
	lodsq
	cmovz rsi, rax
endcode

defcode inb, "INB", 1, 0x00, "port -- char"
	mov dx, bx
	in al, dx
	movzx rbx, al
endcode

defcode incr, "1+", 1, 0x00, "n -- n+1"
	inc rbx
endcode

defcode int3, "INT3", 0, 0x00, "--"
	int3
endcode

defcode invert, "INVERT", 1, 0x00, "u -- ~u"
	not rbx
endcode

defcode jump_impl, "(JUMP)"
	lodsq
	mov rsi, rax
endcode

defcode left_shift, "LSHIFT", 2, 0x00, "x y -- x<<y"
	mov rcx, rbx
	pop rbx
	shl rbx, cl
endcode

defcode left_rotate, "LROT", 2, 0x00, "x y -- x<<<y"
	mov rcx, rbx
	pop rbx
	rol rbx, cl
endcode

defcode less, "<", 2, 0x00, "n1 n2 -- n1<n2"
	pop rax
	xor edx, edx
	cmp rax, rbx
	setge dl
	dec rdx
	mov rbx, rdx
endcode

defcode literal_impl, "(LITERAL)"
	push rbx
	lodsq
	mov rbx, rax
endcode

defcode literal_r_impl, "(LITERAL-R)"
	lodsq
	sub rbp, 8
	mov [rbp], rax
endcode

defcode max, "MAX", 2, 0x00, "n n -- n"
	pop rax
	cmp rbx, rax
	cmovg rax, rbx
	mov rbx, rax
endcode

defcode min, "MIN", 2, 0x00, "n n -- n"
	pop rax
	cmp rbx, rax
	cmovl rax, rbx
	mov rbx, rax
endcode

defcode move, "MOVE", 3, 0x00, "from-addr to-addr len --"
	mov rcx, rbx
	mov rdx, rdi
	pop rdi
	pop rsi
	pop rbx
	rep movsb
	mov rdi, rdx
endcode

defcode mul_d, "*D", 2
	mov rax, [rsp]
	imul rbx
	mov [rsp], rdx
	mov rbx, rax
endcode

defcode mul_div_mod, "*/MOD", 3, 0x00, "x y z -- x*y%z x*y/z"
	pop rax
	pop rdx
	imul rdx
	idiv rbx
	mov rbx, rax
	push rdx
endcode

defcode n_to_str, "N>STR", 1
	mov rax, rbx ; rax = current val

	xor r8d, r8d
	test rax, rax
	jns .not_neg
	mov r8b, 1 ; r8b = negative flag
	neg rax

.not_neg:
	test byte [r15+0x28], 0x01
	mov rbx, 10
	mov rcx, 16
	cmovnz rbx, rcx ; rbx = base

	mov rcx, 32 ; rcx = current offset in str

.loop:
	xor edx, edx
	idiv rbx
	mov dl, [.chrs+rdx]
	mov [rcx+.buf-1], dl
	dec rcx

	test rax, rax
	jnz .loop

	test r8b, r8b
	jz .end

	; Add the '-'
	mov byte [rcx+.buf-1], '-'
	dec rcx

.end:
	lea rbx, [rcx+.buf]
	push rbx
	mov rbx, 32
	sub rbx, rcx
endcode
.buf: times 32 db 0
.chrs: db "0123456789abcdef"

defcode negate, "NEGATE", 1, 0x00, "n -- -n"
	neg rbx
endcode

defcode noop, "NOOP", 0, 0x00, "--"
	nop
endcode

defcode or, "OR", 2, 0x00, "x y -- x|y"
	pop rax
	or rbx, rax
endcode

defcode outb, "OUTB", 2, 0x00, "char port --"
	mov dx, bx
	pop rax
	pop rbx
	out dx, al
endcode

defcode outw, "OUTW", 2, 0x00, "word port --"
	mov dx, bx
	pop rax
	pop rbx
	out dx, ax
endcode

defcode pick, "PICK", 1
	lea rbx, [rsp+rbx*8]
	cmp r13, rbx
	jb underflow
	mov rbx, [rbx]
endcode

defcode r_depth, "RDEPTH"
	push rbx
	mov rbx, r14
	sub rbx, rbp
	shr rbx, 3
endcode

defcode r_drop, "RDROP", 0, 0x00, "R: x --"
	; Check for return underflow.
	lea rcx, [rbp+8]
	cmp r14, rcx
	jb underflow_return
	add rbp, 8
endcode

defcode r_fetch, "R@"
	; Check for return underflow.
	lea rcx, [rbp+8]
	cmp r14, rcx
	jb underflow_return

	push rbx
	mov rbx, [rbp]
endcode

defcode rdtsc, "RDTSC", 0, 0x00, "-- u"
	push rbx
	rdtsc
	mov rbx, rdx
	shl rbx, 32
	or rbx, rax
endcode

defcode rev_rot, "-ROT", 3, 0x00, "x y z -- z x y"
	mov rax, [rsp+8]
	mov [rsp+8], rbx
	mov rbx, [rsp]
	mov [rsp], rax
endcode

defcode right_shift, "RSHIFT", 2, 0x00, "x y -- x>>y"
	mov rcx, rbx
	pop rbx
	shr rbx, cl
endcode

defcode rot, "ROT", 3, 0x00, "x y z -- y z x"
	mov rax, [rsp]
	mov [rsp], rbx
	mov rbx, [rsp+8]
	mov [rsp+8], rax
endcode

defcode rpick, "RPICK", 1, 0x00, "u -- R_u"
	lea rbx, [rbp+rbx*8]
	cmp r14, rbx
	jb underflow_return
	mov rbx, [rbx]
endcode

defcode rpick_addr, "RPICK-ADDR", 1
	lea rbx, [rbp+rbx*8]
	cmp r14, rbx
	jb underflow_return
endcode

defcode s_quote_impl, '(S")',
	push rbx
	lodsb
	movzx rbx, al
	push rsi
	add rsi, rbx
endcode

defcode s_to_d, "S>D", 1
	push rbx
	sar rbx, 31
endcode

defcode sat_sub, "SAT-", 1, 0x00, "a b -- a-b | 0"
	pop rax
	mov rdx, rbx
	xor rbx, rbx
	sub rax, rdx
	cmovnc rbx, rax
endcode

defcode set_flag, "SET-FLAG", 2, 0x00, "u bit -- u"
	pop rax
	xor edx, edx
	inc edx
	mov rcx, rbx
	shl rdx, cl
	or rax, rdx
	mov rbx, rax
endcode

defcode source_buffer, "SOURCE-BUFFER"
	push rbx
	lea rbx, [r15+0x08]
endcode

defcode source_length, "SOURCE-LENGTH"
	push rbx
	lea rbx, [r15+0x10]
endcode

defcode state_compile, "]"
	or byte [r15+0x28], 0x02
endcode

defcode state_interpret, "[", 0, 0x01
	and byte [r15+0x28], 0xfd
endcode

defcode store, "!", 2, 0x00, "x addr --"
	pop rax
	mov [rbx], rax
	pop rbx
endcode

defcode store_char, "C!", 2, 0x00, "char addr --"
	pop rax
	mov [rbx], al
	pop rbx
endcode

defcode store_dword, "D!", 2, 0x00, "dword addr --"
	pop rax
	mov [rbx], eax
	pop rbx
endcode

defcode store_word, "W!", 2, 0x00, "word addr --"
	pop rax
	mov [rbx], ax
	pop rbx
endcode

defcode streq, "STRING=", 4, 0x00, "addr u addr u -- flag"
	; ( addr1 len1 addr2 len2 ) mapped to
	; ( rdx   rcx  rdi   rax  )
	mov rdi, [rsp]
	mov rcx, [rsp+8]
	mov rdx, [rsp+16]
	mov rax, rbx
	add rsp, 24
	xor ebx, ebx

	cmp rax, rcx
	jne .fail

	xor r8d, r8d
.loop:
	cmp r8, rcx
	je .success

	mov al, [rdx+r8]
	mov r9b, [rdi+r8]
	cmp r9b, al
	jne .fail

	inc r8
	jmp .loop

.success:
	dec rbx
.fail:
endcode

defcode sub, "-", 2, 0x00, "x y -- x-y"
	pop rax
	sub rax, rbx
	mov rbx, rax
endcode

defcode swap, "SWAP", 2, 0x00, "x y -- y x"
	xchg [rsp], rbx
endcode

defcode test_flag, "TEST-FLAG", 2, 0x00, "x y -- flag"
	pop rdx
	mov rcx, rbx
	mov rax, 1
	shl rax, cl
	xor ebx, ebx
	test rax, rdx
	jz .exit
	dec rbx
.exit:
endcode

defcode to_in, ">IN", 0, 0x00, "-- addr"
	push rbx
	lea rbx, [r15+0x18]
endcode

defcode to_number, ">NUMBER", 2
	; ( addr len -- num 1 | 0 )
	test byte [r15+0x28], 0x01
	mov rdi, 10
	mov rax, 16
	cmovnz rdi, rax

	mov rcx, rbx
	mov r8, [rsp]
	mov r9, rsp
	call to_number

	movzx rbx, byte [rsp]
	mov [rsp], rax
	test rbx, rbx
	jnz .end
	add rsp, 8
.end:
endcode

defcode to_r, ">R", 1
	sub rbp, 8
	mov [rbp], rbx
	pop rbx
endcode

defcode to_r_2, "2>R", 2
	sub rbp, 16
	mov [rbp], rbx
	mov rax, [rsp]
	mov [rbp+8], rax
	mov rbx, [rsp+8]
	add rsp, 16
endcode

defcode true, "TRUE"
	push rbx
	xor ebx, ebx
	dec rbx
endcode

defcode try_rdseed, "TRY-RDSEED", 0, 0x00, "-- u flag"
	push rbx
	push rbx
	mov eax, 7
	xor ecx, ecx
	cpuid
	test ebx, (1<<18)
	jz .fail
	mov rcx, 10
.loop:
	rdseed rax
	jc .ok
	dec rcx
	jz .fail
	jmp .loop
.ok:
	mov [rsp], rax
	xor ebx, ebx
	dec rbx
	lodsq
	jmp rax
.fail:
	xor ebx, ebx
	mov [rsp], rbx
endcode

defcode u_greater, "U>", 2
	pop rax
	xor edx, edx
	cmp rax, rbx
	setbe dl
	dec rdx
	mov rbx, rdx
endcode

defcode u_less, "U<", 2
	pop rax
	xor edx, edx
	cmp rax, rbx
	setae dl
	dec rdx
	mov rbx, rdx
endcode

defcode u_max, "UMAX", 2, 0x00, "u u -- u"
	pop rax
	cmp rbx, rax
	cmova rax, rbx
	mov rbx, rax
endcode

defcode u_min, "UMIN", 2, 0x00, "u u -- u"
	pop rax
	cmp rbx, rax
	cmovb rax, rbx
	mov rbx, rax
endcode

defcode udiv, "U/", 2, 0x00, "x y -- x/y"
	xor edx, edx
	pop rax
	div rbx
	mov rbx, rax
endcode

defcode umul, "U*", 2, 0x00, "x y -- x*y"
	pop rax
	mul rbx
	mov rbx, rax
endcode

defcode process_pointer, "PROCESS-POINTER", 0, 0x00, "-- addr"
	push rbx
	mov rbx, r15
endcode

defcode unset_flag, "UNSET-FLAG", 2, 0x00, "u bit -- u"
	pop rax
	xor edx, edx
	inc edx
	mov rcx, rbx
	shl rdx, cl
	not rdx
	and rax, rdx
	mov rbx, rax
endcode

defcode xor, "XOR", 2, 0x00, "x y -- x^y"
	pop rax
	xor rbx, rax
endcode

defcode zero_ge, "0>=", 1
	xor edx, edx
	cmp rbx, 0
	setl dl
	dec rdx
	mov rbx, rdx
endcode

defcode zero_gt, "0>", 1
	xor edx, edx
	cmp rbx, 0
	setle dl
	dec rdx
	mov rbx, rdx
endcode

defcode zero_le, "0<=", 1
	xor edx, edx
	cmp rbx, 0
	setg dl
	dec rdx
	mov rbx, rdx
endcode

defcode zero_lt, "0<", 1
	xor edx, edx
	cmp rbx, 0
	setge dl
	dec rdx
	mov rbx, rdx
endcode

defcode zero_equal, "0=", 1
	xor edx, edx
	test rbx, rbx
	setnz dl
	dec rdx
	mov rbx, rdx
endcode

defcode zero_ne, "0<>", 1
	xor edx, edx
	test rbx, rbx
	setz dl
	dec rdx
	mov rbx, rdx
endcode

; This is a no-name no-op word, as a marker and safety guard.
defcode last_builtin, "", 0
endcode

; vi: cc=80 ft=nasm
