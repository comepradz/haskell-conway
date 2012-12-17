all: life

clean:
	rm -f *.hi *.o life Flarn/*.hi Flarn/*.o

life:
	ghc --make -O2 -rtsopts -threaded life
