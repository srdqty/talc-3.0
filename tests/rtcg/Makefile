HOME_DIR = ../..
RUNTIME_DIR = $(HOME_DIR)/runtime
POPLIB_DIR = $(HOME_DIR)/popcorn/lib
NO_STACK_TRACE=

ifdef WINDIR
CC=cl
CFLAGS= /O2
COUT= /Fe
O=obj
A=lib
LINK=link
else
CC=gcc
COUT=-o
CFLAGS = -O2
O=o
A=a
LINK=ld
endif

LIBS = core list string char math
PRELUDE = prelude
STDLIBS = stdlib pop_runtime

POP        = popcorn.exe
POPFLAGS   = 
POPLIBS    = $(LIBS)
POPPRELUDE = $(PRELUDE)
POPSTDLIBS = $(STDLIBS)

PIP        = $(HOME_DIR)/apps/popcorn/build/popcorn_slow.exe
PIPFLAGS   = -no-inline
PIPLIBS    = $(addsuffix _strap, $(LIBS))
PIPPRELUDE = $(addsuffix new, $(PRELUDE))
PIPSTDLIBS = $(addsuffix new, $(STDLIBS))


TALC      = talc.exe 
TALCFLAGS = --verify-link --verify-program
PRELUDE   = prelude
TALCLIBS  =
ASSEMBLER = 

ifndef NO_STACK_TRACE
      POPFLAGS += --stack-trace 
endif

TALCFLAGS += --no-internals
CYCLONELIB = $(RUNTIME_DIR)/cyclonelib.$(O)

POPFLAGS += $(addprefix $(POPLIB_DIR)/, $(addsuffix .$(O), $(POPLIBS)))
POPTALCLIBS = $(addprefix $(POPLIB_DIR)/, $(addsuffix .to, $(POPLIBS))) \
	    $(RUNTIME_DIR)/$(POPPRELUDE).to $(addprefix --std-lib , \
	    $(POPSTDLIBS)) $(CYCLONELIB)

PIPFLAGS += -edge-stats $(addprefix $(POPLIB_DIR)/, $(addsuffix .$(O), $(PIPLIBS)))
PIPTALCLIBS = $(addprefix $(POPLIB_DIR)/, $(addsuffix .to, $(PIPLIBS))) \
	    $(RUNTIME_DIR)/$(PIPPRELUDE).to $(addprefix --std-lib , \
	    $(PIPSTDLIBS)) $(CYCLONELIB)

POPTALCLIBS += -T timer/timer_old.$(O)
PIPTALCLIBS += -T timer/timer_new.$(O)

CLIBS = timer/timer.$(O) -lm

OUTFILE=test.out
REFFILE=ref.out

BENCH=cheb csi dot dotf fft poly polyf pow powf pow2 powf2 romberg scale

BENCH_P_SRC= $(addsuffix _p.pop, $(BENCH))
BENCH_R_SRC= $(addsuffix _r.pop, $(BENCH))

BENCH_I=$(BENCH_P_SRC:.pop=_i.exe) $(BENCH_R_SRC:.pop=_i.exe)
BENCH_O=$(BENCH_P_SRC:.pop=_o.exe) $(BENCH_R_SRC:.pop=_o.exe)

BENCH_C=$(BENCH_P_SRC:_p.pop=_c_c.exe) $(BENCH_P_SRC:_p.pop=_p_c.exe)

all:
	echo All does nothing.

bench_i: $(BENCH_I)
	echo Done

bench_o: $(BENCH_O)
	echo Done

bench_c: $(BENCH_C)
	echo Done

run_i: bench_i
	./cheb_p_i.exe 20
	./cheb_r_i.exe 20
	./csi_p_i.exe 100
	./csi_r_i.exe 100
	./dot_p_i.exe 20 20
	./dot_r_i.exe  20 20
	./dotf_p_i.exe 20 20
	./dotf_r_i.exe 20 20
	./fft_p_i.exe 5
	./fft_r_i.exe 5
	./poly_p_i.exe 8
	./poly_r_i.exe 8
	./polyf_p_i.exe 8
	./polyf_r_i.exe 8
	./pow_p_i.exe 5
	./pow_r_i.exe 5
	./powf_p_i.exe 5
	./powf_r_i.exe 5
	./pow2_p_i.exe 5
	./pow2_r_i.exe 5
	./powf2_p_i.exe 5
	./powf2_r_i.exe 5
	./romberg_p_i.exe 8
	./romberg_r_i.exe 8
	./scale_p_i.exe 10
	./scale_r_i.exe 10

run_o: bench_o
	./cheb_p_o.exe 20
	./cheb_r_o.exe 20
	./csi_p_o.exe 100
	./csi_r_o.exe 100
	./dot_p_o.exe 20 20
	./dot_r_o.exe  20 20
	./dotf_p_o.exe 20 20
	./dotf_r_o.exe 20 20
	./fft_p_o.exe 5
	./fft_r_o.exe 5
	./poly_p_o.exe 8
	./poly_r_o.exe 8
	./polyf_p_o.exe 8
	./polyf_r_o.exe 8
	./pow_p_o.exe 5
	./pow_r_o.exe 5
	./powf_p_o.exe 5
	./powf_r_o.exe 5
	./pow2_p_o.exe 5
	./pow2_r_o.exe 5
	./powf2_p_o.exe 5
	./powf2_r_o.exe 5
	./romberg_p_o.exe 8
	./romberg_r_o.exe 8
	./scale_p_o.exe 10
	./scale_r_o.exe 10

dot_all: dot_p_i.exe dot_r_i.exe dot_c_c.exe dot_p_c.exe

pow_all: pow_p_i.exe pow_r_i.exe pow_c_c.exe pow_p_c.exe

pow2_all: pow2_p_i.exe pow2_r_i.exe pow2_c_c.exe pow2_p_c.exe

poly_all: poly_p_i.exe poly_r_i.exe poly_c_c.exe poly_p_c.exe

scale_all: scale_p_i.exe scale_r_i.exe scale_c_c.exe scale_p_c.exe

clean_exe_only:
	rm -f *~ *.$(O) *.to *.tal* *.lst

.PHONY : clean
clean: clean_exe_only
	rm -f *.exe
	rm -f *.ver
	rm -f *.pdb # produced by debugging
	rm -f *.ilk # produced by debugging
	rm -f *.pil
	rm -f $(OUTFILE)

### Implicit rules

fft_spec%.inc: ./fftgen_c_c.exe
	./fftgen_c_c.exe $@ $(*F)

fft_cspec%.inc: ./cfftgen_c_c.exe
	./cfftgen_c_c.exe $@ $(*F)

fft_spec%.c: fft_cspec%.inc fft_kernel.c
	cat fft_kernel.c $< > $@

fft_spec%.pop: fft_spec%.inc fft_kernel.pop
	cat fft_kernel.pop $< > $@


%_tal.exe: %.tal
	$(TALC) --no-internals --verify-link --verify-program -o $@ $(<:.tal=.tal) $(PIPTALCLIBS)

%_o.exe: %.pop poptiming.h
	$(POP) $(POPFLAGS) -D COMPILER="\'o\'" -c $<
	$(TALC) $(TALCFLAGS) -o $@ $(<:.pop=.to) $(POPTALCLIBS)

%_i.exe: %.pop poptiming.h
	$(PIP) $(PIPFLAGS) -D COMPILER="\'i\'" -c $<
	$(TALC) $(TALCFLAGS) -o $@ $(<:.pop=.to) $(PIPTALCLIBS)

%_a.exe: %.pop poptiming.h
	$(PIP) -no-cfg-analysis $(PIPFLAGS) -D COMPILER="\'a\'" -c $<
	mv $(<:.pop=.pil) $(<:.pop=_a.pil)
	$(TALC) $(TALCFLAGS) -o $@ $(<:.pop=.to) $(PIPTALCLIBS)

%_c_c.exe: %.c ctiming.h
	$(CC) $(CFLAGS) $(COUT)$@ $< $(CLIBS)

%_p_c.exe: %.c ctiming.h
	$(CC) $(CFLAGS) -DPOP $(COUT)$@ $< $(CLIBS) $(RUNTIME_DIR)/gc.$(A)

%_i.pop: %.pop
	ln $< $@

%_o.pop: %.pop
	ln $< $@

%_i.to: %_i.pop
	$(PIP) $(PIPFLAGS) -D COMPILER="\'i\'" -c $<

%_o.to: %_o.pop
	$(POP) $(POPFLAGS) -D COMPILER="\'o\'" -c $<

