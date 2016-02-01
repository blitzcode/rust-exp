
# Build project by invoking Haskell's Stack and Rust's Cargo build tools

ifeq (, $(shell which stack))
$(error "Can't find Haskell 'stack' tool, please install from http://haskellstack.org/")
endif

ifeq (, $(shell which cargo))
$(error "Can't find Rust 'cargo' tool, please install from https://www.rust-lang.org/")
endif

.PHONY: all
all:
	cargo build --release
	stack build

.PHONY: clean
clean:
	cargo clean
	stack clean
	$(RM) -rf .stack-work

