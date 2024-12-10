# Commands:

.PHONY: build init test clean doc deploy stage

build: 
	ghc --make -O -o solve Solver.hs

prof: 
	ghc --make -prof -o solve Solver.hs

all: build test

# Cleaning commands:
clean:
	rm -f solve
	rm -f *.hi
	rm -f *.o

test: build
	./solve --test
