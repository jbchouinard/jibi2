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
	cargo clippy
	cargo test
	cargo run -- tests/*.jibi

clean:
	cargo clean

.PHONY: default build doc install test clean gh-pages
