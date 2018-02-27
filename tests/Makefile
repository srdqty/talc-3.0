all: all_tests

all_tests:
ifndef POP_IN_POP
	cd loader; $(MAKE) compile run check; exit 0
endif
	cd popcorn; $(MAKE) compile run check; exit 0
	cd cyclone; $(MAKE) compile run check; exit 0

clean:
	cd loader; $(MAKE) clean
	cd popcorn; $(MAKE) clean
	cd cyclone; $(MAKE) clean
	
