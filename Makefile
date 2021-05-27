all:worm-pl

worm-pl:
	cargo build
	cp target/debug/rworm worm-pl
	cp worm-pl tests/arith_tests/worm-pl
	cp -r worm tests/arith_tests/worm

clean:
	rm worm-pl
	rm -r tests/arith_tests/worm
	rm tests/arith_tests/worm-pl