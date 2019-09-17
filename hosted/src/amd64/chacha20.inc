bits 64

%macro chacha20_prold 3 ; dst, src, tmp
	movdqa %3, %1
	pslld %1, %2
	psrld %3, 32-%2
	por %1, %3
%endmacro

%macro chacha20_qr 7 ; a, b, c, d, tmp, mask8, mask16
	paddd %1, %2
	pxor %4, %1
	pshufb %4, %7

	paddd %3, %4
	pxor %2, %3
	chacha20_prold %2, 12, %5

	paddd %1, %2
	pxor %4, %1
	pshufb %4, %6

	paddd %3, %4
	pxor %2, %3
	chacha20_prold %2, 7, %5
%endmacro

%macro chacha20_two_rounds 7
	chacha20_qr %1, %2, %3, %4, %5, %6, %7
	pshufd %2, %2, 0x39
	pshufd %3, %3, 0x4e
	pshufd %4, %4, 0x93
	chacha20_qr %1, %2, %3, %4, %5, %6, %7
	pshufd %2, %2, 0x93
	pshufd %3, %3, 0x4e
	pshufd %4, %4, 0x39
%endmacro

%macro chacha20_twenty_rounds 7
	chacha20_two_rounds %1, %2, %3, %4, %5, %6, %7
	chacha20_two_rounds %1, %2, %3, %4, %5, %6, %7
	chacha20_two_rounds %1, %2, %3, %4, %5, %6, %7
	chacha20_two_rounds %1, %2, %3, %4, %5, %6, %7
	chacha20_two_rounds %1, %2, %3, %4, %5, %6, %7
	chacha20_two_rounds %1, %2, %3, %4, %5, %6, %7
	chacha20_two_rounds %1, %2, %3, %4, %5, %6, %7
	chacha20_two_rounds %1, %2, %3, %4, %5, %6, %7
	chacha20_two_rounds %1, %2, %3, %4, %5, %6, %7
	chacha20_two_rounds %1, %2, %3, %4, %5, %6, %7
%endmacro

[section .rodata]

align 16
chacha20_mask8:  dd 0x02010003, 0x06050407, 0x0a09080b, 0x0e0d0c0f
chacha20_mask16: dd 0x01000302, 0x05040706, 0x09080b0a, 0x0d0c0f0e

; vi: cc=80 ft=nasm
