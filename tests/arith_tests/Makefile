all:arith

arith: ../../arith.hs
	ghc -o arith ../../arith.hs

clean:
	rm -r arith ../../arith.hi ../../arith.o

install:
	stack install base parsec haskeline mtl