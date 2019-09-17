bits 64

extern forth_last_pseudobuiltin
extern ifa
extern undefined_word

global gdtr
global idt
global idt.end
global idtr
global ipb ; The Important Pointer Block.
global ipb.free_list
global ipb.here
global ipb.mb2
global ipb.quantum
global ipb.quantum_max
global p3 ; aka PDPT
global p4 ; aka PML4

[section .data]

ipb:
	db "IPB HERE"
.mb2:
	dq 0 ; Gets filled in with address of multiboot2 information structure.
.free_list:
	dq 0 ; Head of free list for allocators.
.here:
	dq 0 ; The address returned by HERE and increased by ALLOT.
	dq 0 ; On StahlOS, the current level 4 page table.
	dq undefined_word ; The hook to call when an undefined word is encountered.
.quantum:
	dq 0 ; The number of ticks remaining in the quantum.
.quantum_max:
	dq 10 ; The number of total ticks per quantum.
.run_queue:
	dq 0 ; Address of the run queue
.int_queue:
	dq 0 ; Address of the interrupt process queue
	dq ipb

; vi: cc=80 ft=nasm
