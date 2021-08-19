ifeq ($(PREFIX),)
	PREFIX := /usr/local
endif

default: build

build:
	cargo build --release

doc:
	cargo doc --release --no-deps

install:
	install -d $(PREFIX)/bin/
	install -m 755 target/release/jibi2 $(PREFIX)/bin/

test:
	cargo test

clean:
	cargo clean

.PHONY: default build doc install test clean gh-pages
