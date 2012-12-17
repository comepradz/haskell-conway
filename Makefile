all: life

clean:
	rm -f *.hi *.o life Flarn/*.hi Flarn/*.o

life: life.hs Flarn/Conway.hs
	ghc --make -O2 -rtsopts -threaded life

run: life
	./life +RTS -s -N4 -qa -qg
