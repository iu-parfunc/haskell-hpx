

all: run

test.exe: Safe_stub.h test.c Safe.hs
	ghc --make -no-hs-main -optc-O test.c Safe -o test.exe

Safe_stub.h: Safe.hs
	ghc -c Safe.hs

run: test.exe
	./test.exe

clean:
	rm -f Safe_stub.h Safe.o test.o test.exe
