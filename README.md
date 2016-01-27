
# Rust Experiments

This project contains a number of experiments in the simulation / graphics category. I frequently write Haskell projects where I write the performance critical, numerical code in C/C++ or offload it to the GPU. I wanted to try using Rust as a safer and more functional alternative. Here we have a Haskell OpenGL application doing the display, user interaction, benchmarking and non-inner-loop parts with the actual computations done in a Rust library.

**If you want to read actual algorithm descriptions and references of these experiments and see more, higher quality images visit the following links to my website**

- [Blitzcode.net](http://www.blitzcode.net/)

# Images

A few low-resolution previews of pictures generated, visit the links above for more, higher quality images.

![rust-exp](https://raw.github.com/blitzcode/rust-exp/master/img/rust-exp.png)

# Building

This project uses the Stack / Cabal tools (`stack build`) for building the Haskell code and Cargo (`cargo build --release`) for building the Rust code. There's a top-level Makefile invoking both, simply do

    make

once you have Stack and Cargo / Rust installed.

# Legal

This program is published under the [MIT License](http://en.wikipedia.org/wiki/MIT_License).

# Author

Developed by Tim C. Schroeder, visit my [website](http://www.blitzcode.net) to learn more.

