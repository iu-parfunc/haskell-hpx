
HPXDIR=../hpx

LIBDIR=$(HPXDIR)/libhpx/.libs
INCDIR=$(HPXDIR)/include

all:
	c2hs --cppopts='-I$(INCDIR)' Hpx.chs
	ghc Test.hs Hpx.hs $(LIBDIR)/libhpx.a -lxml2 -lnuma -lpthread -lpciaccess
