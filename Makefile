
all: test

dist/setup-config: h8.cabal
	./setup configure

test: v8/libv8_g.so dist/setup-config V8.chs
	./setup build

