<img src="logo/just-logo-full.png" alt="logo image" />

**Next in line experimental branch for master. Contains lots of codes for AST bulding which is not related to primary experiment. They need to be in master irrespective of success or failure of primary experiment.**

# just - JS on Rust
A ground up implementation of ES6 JavaScript engine complete from parser to executor.

This is more of an academic curiosity rather than top any charts.

The grammar has been coded in [Pest](https://pest.rs/) language as per the specs at https://262.ecma-international.org/6.0/ .

This folder contains project file for Jetbrain's IntelliJ IDEA editor.

To build run `cargo build`.

To run unit tests run `cargo test --package just --lib parser::unit_tests`.

To run all tests run `cargo test`.

* Parser - Work in progress
  * Lexical Parser - Work in progress
  * AST builder - Work in progress
* Runner - Work in progress
