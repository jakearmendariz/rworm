all:worm-pl

worm-pl:
	cargo build
	cp target/debug/rworm worm-pl

clean:
	rm worm-pl