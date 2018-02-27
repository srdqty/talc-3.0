USE_RTCG=X
POP_IN_POP=X

HOME_DIR = ../../..

include ../../Makefile.inc

POPFLAGS += -no-inline -edge-stats

EXES = rsa-cg.exe rsa-nocg.exe rsa-gcc.exe

compile: $(EXES)

rsa-gcc.exe: rsa-gcc.c
	cl /O2 -o $@ $^
#	gcc -O3 -o $@ $^

rsa-gcc-pop.exe: rsa-gcc-pop.c
	cl /O2 -o $@ $^ ../../../runtime/gc.lib

rsabench: rsa-cg.exe rsa-nocg.exe rsa-gcc.exe
	@echo
	@echo RSA without run time code generation
	./rsa-nocg.exe 50000
	@echo
	@echo RSA with run time code generation
	./rsa-cg.exe 50000
	@echo
	@echo RSA with gcc -O3
	./rsa-gcc.exe 50000

bench: rsabench

exe_only:
	rm -f *~ *.obj *.tal* *.lst

clean: exe_only
	rm -f *.to *.pil
	rm -f *.exe
	rm -f *.ver
	rm -f *.pdb # produced by debugging
	rm -f *.ilk # produced by debugging
