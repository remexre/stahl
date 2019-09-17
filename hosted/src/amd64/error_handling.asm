bits 64

%include "src/amd64/macros.inc"

global allot_overflow
global cold_exited
global init_kill
global jmp_null
global no_code_field
global overflow
global overflow_return
global undefined_word
global underflow
global underflow_return

[section .text]

allot_overflow:
	dbg `PANIC: allot_overflow loop\n`
	cli
	hlt
	jmp allot_overflow

cold_exited:
	dbg `PANIC: cold_exited loop\n`
	cli
	hlt
	jmp cold_exited

init_kill:
	dbg `PANIC: init_kill loop\n`
	cli
	hlt
	jmp init_kill

jmp_null:
	dbg `PANIC: jmp_null loop\n`
	cli
	hlt
	jmp jmp_null

no_code_field:
	dbg `PANIC: no_code_field loop\n`
	cli
	hlt
	jmp cold_exited

overflow:
	dbg `PANIC: overflow loop\n`
	cli
	hlt
	jmp overflow

overflow_return:
	dbg `PANIC: overflow_return loop\n`
	cli
	hlt
	jmp overflow_return

undefined_word:
	dbg `PANIC: undefined_word loop: `
	xor ecx, ecx
	mov rsi, [rsp]
.loop:
	cmp rbx, rcx
	je .loop_end

	mov al, [rsi+rcx]
	out 0xe9, al
	inc rcx

	jmp .loop
.loop_end:
	dbg `\n`
	cli
	hlt
	jmp undefined_word

underflow:
	dbg `PANIC: underflow loop\n`
	cli
	hlt
	jmp underflow

underflow_return:
	dbg `PANIC: underflow_return loop\n`
	cli
	hlt
	jmp underflow_return

; vi: cc=80 ft=nasm
