ASM_UNITS := builtins error_handling interpret pseudobuiltins start structures to_number
NASM ?= nasm
NASMFLAGS += -gdwarf -Werror

all: out/stahl-linux
clean:
	test ! -d out || rm -r out
	test ! -d tmp || rm -r tmp
.PHONY: all clean

out/stahl-linux: $(patsubst %,tmp/%.o,$(ASM_UNITS))
	@mkdir -p $(dir $@)
	ld -o $@ $^

tmp/%.o: src/amd64/%.asm
	@mkdir -p $(dir $@)
	$(NASM) -felf64 -o $@ $(NASMFLAGS) $<
