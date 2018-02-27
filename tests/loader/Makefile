USE_CYCLONE=
USE_LOADER=y
#NO_STACK_TRACE=y

HOME_DIR = ../..
LIBFILES += hashtable poploader filename

include ../Makefile.inc

out=test.out
ref=sample.out

exes = simple_call.exe call.exe dyncall.exe gen_call.exe gen_add_call.exe \
  poploader_test.exe simple_callhand.exe mutual_call.exe mwebtest.exe \
  funcall.exe
objs = simp.$(O) foo.$(O) simple_call.$(O) call.$(O) dyncall.$(O) \
  gen.$(O) gen_call.$(O) gen_add_call.$(O) gen_add1.$(O) gen_add2.$(O) \
  poploader_test.$(O) lr.$(O) simphand.$(O) simple_callhand.$(O) \
  mutual_call.$(O) even.$(O) odd.$(O) webtest.$(O) dwebtest.$(O) \
  mwebtest.$(O) remotecall.$(O) funcall.$(O)

tos = $(objs:.$(O)=.to)

all: clean compile run check 

compile: simple_call gen_add_call gen_call poploader_test simple_callhand \
  mutual_call
	cd updating; $(MAKE) compile

run:
	rm -f $(out)
	./simple_call.exe >> $(out)
	./gen_call.exe >> $(out)
	./gen_add_call.exe >> $(out)
	./poploader_test.exe >> $(out)
	./simple_callhand.exe >> $(out)
	./mutual_call.exe >> $(out)
	cd updating; $(MAKE) run

check:
	diff $(out) $(ref)
	cd updating; $(MAKE) check

clean:
	cd updating; $(MAKE) clean; cd ..
	cd flash; $(MAKE) clean; cd ..
	rm -f *~ $(objs) $(exes) $(tos) *.pdb *.pil *.tal* *.lst $(out)

simple_call: simple_call.pop simp.pop
	$(MAKE) simple_call.exe
	$(MAKE) MAKE_LOADABLE=y simp.$(O)

gen_call: gen_call.pop gen.pop
	$(MAKE) gen_call.exe
	$(MAKE) MAKE_LOADABLE=y gen.$(O)

gen_add_call: gen_add_call.pop gen_add1.pop gen_add2.pop
	$(MAKE) gen_add_call.exe
	$(MAKE) MAKE_LOADABLE=y gen_add1.$(O)
	$(MAKE) MAKE_LOADABLE=y gen_add2.$(O)

mutual_call: mutual_call.pop even.pop odd.pop
	$(MAKE) mutual_call.exe
	$(MAKE) MAKE_LOADABLE=y even.$(O)
	$(MAKE) MAKE_LOADABLE=y odd.$(O)

call.exe: call.o rdtsr.o pentium_cc_stub.o
	$(TALC) $(TALCFLAGS) -o $@ $^ $(TALCLIBS)

bootstrap_test: bootstrapped.pop bootstrap_test.pop
	$(MAKE) export=y bootstrap_test.$(O)
	$(MAKE) MAKE_LOADABLE=y bootstrapped.$(O)

pentium_cc_stub.tali: pentium_cc_stub.tali.sav
	cp $< $@

poploader_test: poploader_test.exe foo.pop 
	$(MAKE) USE_LOADER= foo.$(O)	

# same as simple call above, but hand-translated
simple_callhand:
	$(MAKE) simple_callhand.exe
	$(MAKE) USE_LOADER= simphand.$(O)

funcall.exe: funcall.to remotecall.to
	$(TALC) $(TALCFLAGS) -o $@ $^ $(TALCLIBS) ../../popcorn/lib/array.to

