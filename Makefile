
# Build project by invoking Haskell's Stack and Rust's Cargo build tools

.PHONY: all
all:
	cargo build --release
	stack build

.PHONY: clean
clean:
	cargo clean
	stack clean
	$(RM) -rf .stack-work

