%macro dbg 1
%strlen len %1
%assign i 1
%rep len
%substr ch %1 i
	mov al, ch
	out 0xe9, al
%assign i i+1
%endrep
%endmacro

%define last_defined_word 0
%define indef 0 ; 1 = defcode, 2 = defcolon

%macro defcode 5 ; symbol name, name, num args, flags, docs
	%if indef
		%error "defcode in existing definition"
	%else
		%define indef 1
	%endif
%%docs:
	%strlen %%doclen %5
	%if %%doclen <> 0
		db %%doclen, %5
	%endif
global forth_%1.cfa
%strlen %%namelen %2
forth_%1:
	dq last_defined_word
	%if %%doclen = 0
		dq 0
	%else
		dq %%docs
	%endif
%define last_defined_word forth_%1
	db %4, %%namelen, %2
.cfa:
	%if %3 <> 0
		lea rcx, [rsp+8*%3]
		cmp r13, rcx
		jb underflow
	%endif
%endmacro

%macro endcode 0
	%if indef = 1
		%define indef 0
	%else
		%error "endcode outside of defcode"
	%endif
	lodsq
	jmp rax
%endmacro

%macro defcode 4
	defcode %1, %2, %3, %4, ""
%endmacro
%macro defcode 3
	defcode %1, %2, %3, 0x00
%endmacro
%macro defcode 2
	defcode %1, %2, 0
%endmacro

%macro begincolon 0
	%if indef
		%error "defcolon in existing definition"
	%else
		%define indef 2
	%endif
%endmacro

%macro defcolon 3
begincolon
%strlen %%namelen %2
global forth_%1.cfa
forth_%1:
	dq last_defined_word
	dq 0
%define last_defined_word forth_%1
	db %3, %%namelen, %2
.cfa:
	jmp near forth_docolon.impl
.pfa:
%endmacro

%macro defcolon 2
	defcolon %1, %2, 0x00
%endmacro

%macro lit 1
	%if indef <> 2
		%error "lit outside of defcolon"
	%endif
	word literal_impl
	dq %1
%endmacro

%macro word 1
	extern forth_%1.cfa
	wordl %1
%endmacro

%macro wordl 1
	%if indef <> 2
		%error "word outside of defcolon"
	%endif
	dq forth_%1.cfa
%endmacro

%macro endcolon 0
	word exit
	%if indef = 2
		%define indef 0
	%else
		%error "endcolon outside of defcolon"
	%endif
%endmacro

; vi: cc=80 ft=nasm
