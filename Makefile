
HPXDIR=$(HOME)/working_copies/hpx

LIBDIR=$(HPXDIR)/libhpx/.libs
INCDIR=$(HPXDIR)/include

all:
	c2hs --cppopts='-I$(INCDIR)' Foreign/HPX.chs
	ghc Test.hs Hpx.hs $(LIBDIR)/libhpx.a -lxml2 -lnuma -lpthread
