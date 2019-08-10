PROOF_DIRS += cam-forth

all:
	@for dir in $(PROOF_DIRS); do $(MAKE) -C $$dir; done
clean:
	@for dir in $(PROOF_DIRS); do $(MAKE) -C $$dir clean; done
.PHONY: all clean
