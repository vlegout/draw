all: main

main: main.hs Diagram.hs Tasks.hs
	ghc --make main.hs

clean:
	rm -f main main.o main.hi
	rm -f Diagram.o Diagram.hi
	rm -f Tasks.o Tasks.hi
