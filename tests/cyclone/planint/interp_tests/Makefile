# Makefile for interpreter tests
#
# $Id: Makefile,v 1.1 2000/06/03 11:54:45 hornof Exp $
# $Revision: 1.1 $
# 
SRCS		= Helloworld.plan binops.plan blob.plan cons.plan consts.plan \
		chunks1a.plan chunks1b.plan chunks2a.plan chunks2b.plan \
		chunks3a.plan chunks3b.plan except.plan foldl.plan foldr.plan \
		funnest.plan member.plan norecurse.plan store.plan \
		tuples.plan unops.plan
PARSE_RESULTS	= $(SRCS:.plan=.pout) typeerror.pout
TINFER_RESULTS  = $(SRCS:.plan=.tout)

all: tests

tests: $(PARSE_RESULTS) $(TINFER_RESULTS)

%.pout: %.plan
	../bin/parsetest $< > $@

%.tout: %.plan
	../bin/typetest $< > $@


clean:
	$(RM) $(PARSE_RESULTS) $(TINFER_RESULTS)
