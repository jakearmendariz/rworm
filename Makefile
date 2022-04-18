all:worm-pl

release: worm-pl-release

worm-pl-release:
	cargo build --release
	cp target/release/rworm worm-pl-release
	cp worm-pl-release tests/arith_tests/worm-pl
	mkdir tests/arith_tests/worm
	cp worm/arith.c tests/arith_tests/worm/arith.c
	cp worm/wormstd.c tests/arith_tests/worm/wormstd.c

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

clean-release:
	rm worm-pl-release
	rm -r tests/arith_tests/worm
	rm tests/arith_tests/worm-pl
