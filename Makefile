ifeq ($(PREFIX),)
	PREFIX := /usr/local
endif

TARGET ?= x86_64-unknown-linux-gnu

default: build

build:
	CARGO_BUILD_TARGET=$(TARGET) cargo build --release

run:
	CARGO_BUILD_TARGET=$(TARGET) cargo run --release

doc:
	CARGO_BUILD_TARGET=$(TARGET) cargo doc --release --no-deps

install:
	install -d $(PREFIX)/bin/
	install -m 755 target/release/jibi2 $(PREFIX)/bin/

test:
	CARGO_BUILD_TARGET=$(TARGET) cargo clippy
	CARGO_BUILD_TARGET=$(TARGET) cargo test
	CARGO_BUILD_TARGET=$(TARGET) cargo run -- tests/*.jibi

clean:
	cargo clean

.PHONY: default build run doc install test clean
