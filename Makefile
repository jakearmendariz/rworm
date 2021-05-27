all:worm-pl

worm-pl:
	cargo build
	cp target/debug/rworm worm-pl
	cp worm-pl tests/arith_tests/worm-pl
	mkdir tests/arith_tests/worm
	cp worm/arith.c tests/arith_tests/worm/arith.c
	cp worm/wormstd.c tests/arith_tests/worm/wormstd.c

clean:
	rm worm-pl
	rm -r tests/arith_tests/worm
	rm tests/arith_tests/worm-pl