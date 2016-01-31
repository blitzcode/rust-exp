
# Rust Experiments

This project contains a number of experiments in the simulation / graphics category. I frequently write Haskell projects where I delegate the performance critical, numerical code to C/C++ or offload it to the GPU. I wanted to try using Rust for the performance critical parts as a safer and more functional alternative. Here we have a Haskell OpenGL application doing the display, user interaction, benchmarking and non-inner-loop parts with the actual computations done in a Rust library.

![rust-exp](https://raw.github.com/blitzcode/rust-exp/master/experiments.png)

Experiments include...

- Simple 'scrolling sine waves' visualizer, proof-of-concept of the Rust -> Haskell -> OpenGL pipeline
- The famous 'Game of Life' cellular automata, with a reasonably optimized and parallelized implementation plus recallable patterns
- Gravitational N-Body simulation, both a brute force O(N^2) and the O(n log n) Barnes-Hut algorithm are implemented

**If you want to read actual algorithm descriptions and references for these experiments, including more, higher quality images visit the following link to my website**

[Rust projects on Blitzcode.net](http://www.blitzcode.net/rust.shtml)

The Haskell application itself might also be of interests. It features a pluggable experiment framework, modern OpenGL 3/4.x style rendering, fonts, quad rendering, screenshots, framebuffer system, FPS counter, benchmarking, GLSL, logging etc. A good starting point for your own Haskell + OpenGL adventures.

# Building

This project uses the Stack / Cabal tools (`stack build`) for building the Haskell code and Cargo (`cargo build --release`) for building the Rust code. There's a top-level Makefile invoking both, simply do

    make

once you have Stack and Cargo / Rust installed.

# Legal

This program is published under the [MIT License](http://en.wikipedia.org/wiki/MIT_License).

# Author

Developed by Tim C. Schroeder, visit my [website](http://www.blitzcode.net) to learn more.

