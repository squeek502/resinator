resinator
=========

A (very work-in-progress) cross-platform Windows resource-definition script (.rc) to resource file (.res) compiler. The intention is for this to eventually get merged into the Zig compiler as per [this accepted proposal](https://github.com/ziglang/zig/issues/3702).

## Overview / How it will fit into Zig

A Windows resource-definition file is made up of both C/C++ preprocessor commands and resource definitions.

- The preprocessor commands will be evaluated first via Zig (this would be either clang or arocc)
- The preprocessed `.rc` file will be compiled into a `.res` (that's what this project does)
- The `.res` will be linked into executables by Zig's linker

## Goals

Similar to `llvm-rc` and GNU's `windres`, `resinator` aims to be a cross-platform alternative to the MSVC++ `rc` tool.

However, unlike `llvm-rc` and `windres`, `resinator` aims to get as close to 1:1 compatibility with the MSVC++ `rc` tool as possible. That is, the ideal would be:

- The `.res` output of `resinator` should match the `.res` output of the Windows `rc` tool in as many cases as possible (if not exactly, then functionally). However, `resinator` may not support all valid `.rc` files (i.e. `#pragma code_page` support might be limited to particular code pages).
- `resinator` should fail to compile `.rc` files that the Windows `rc` tool fails to compile.
