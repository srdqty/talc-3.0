HOME_DIR = ../../..

include ../../Makefile.inc

LIBFILES += arg sockets
RT_BASE=tal_sockets
RT= $(addsuffix .$(O), $(RT_BASE))

#########################################################
# all
#########################################################

all: server.exe client.exe args.exe s c 

#########################################################

server.exe: $(PLIB) $(OBJ) server.$(O)
	$(POPCORN) $(POPFLAGS) -o $@ $^ $(RT)

client.exe: $(PLIB) $(OBJ) client.$(O)
	$(POPCORN) $(POPFLAGS) -o $@ $^ $(RT)

args.exe: $(PLIB) args.$(O)
	$(POPCORN) $(POPFLAGS) -o $@ -I  $^ $(RT)

%.tal: %.pop
	$(POPCORN) $(POPFLAGS) --generate-tal-only $<

%.to %.$(O) %.tal: %.pop
	$(POPCORN) $(POPFLAGS) -c $<

%.to %.$(O): %.tal
	$(TALC) $(TALCFLAGS) -c $<


%.exe: %.to
	$(TALC) $(TALCFLAGS) -o $@ $< $(TALCLIBS)

%.$(O): %.c 
	$(CC) -c $<

clean:
	rm -f *~ *.$(O) *.to *.tal *.tali *.lst *.pdb *.ilk *.exe

#########################################################
# Non-regression suite
#########################################################

REF_FILE=reference.out
CHECK_FILE=check.out

run-local:
	rm -f $(CHECK_FILE)

check-local:
	diff $(CHECK_FILE) $(REF_FILE)

ref-local:
	cp $(CHECK_FILE) $(REF_FILE)





