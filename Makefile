

LIBDIR=../libhpx/.libs/

all:
	c2hs --cppopts='-I./hpx/include/' Hpx.chs
	ghc Test.hs Hpx.hs $(LIBDIR)/libhpx.a -lxml2 -lnuma -lpthread
