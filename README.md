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

The plan is to use fuzz testing with the `rc` tool as an oracle to ensure that `resinator` generates the same output for every input.

### Intentional divergences from the MSVC++ `rc` tool

- In `resinator`, using the number `6` as a resource type is an error and will fail to compile.
  + The Windows RC compiler allows the number `6` (i.e. `RT_STRING`) to be specified as a resource type. When this happens, the Windows RC compiler will output a `.res` file with a resource that has the format of a user-defined resource, but with the type `RT_STRING`. The resulting `.res` file is basically always invalid/bogus/unreadable, as `STRINGTABLE`/`RT_STRING` has [a very particular format](https://devblogs.microsoft.com/oldnewthing/20040130-00/?p=40813).
- In `resinator`, embedded `NUL` (`<0x00>`) characters are always illegal anywhere in a `.rc` file.
  + The Windows RC compiler behaves very strangely when embedded `NUL` characters are in a `.rc` file. For example, `1 RCDATA { "a<0x00>" }` will give the error "unexpected end of file in string literal", but `1 RCDATA { "<0x00>" }` will "successfully" compile and result in an empty `.res` file (the `RCDATA` resource won't be included at all). Even stranger, whitespace seems to matter in terms of when it will error; if you add a space to the beginning of the `1 RCDATA { "a<0x00>" }` version then it "successfully" compiles but also results in an empty `.res`.
    - TODO: This might be related to the Windows RC compiler's handling/inferring of UTF-16 encoded files, which `resinator` doesn't handle yet.
  + This is also somewhat unavoidable as the Clang preprocessor will convert `NUL` characters outside string/char literals to space characters, which could cause the `.rc` file to parse very differently.
- *[Not final]* In `resinator`, embedded 'End of Transmission' (`<0x04>`) characters are always illegal outside of string literals.
  + The Windows RC compiler seemingly treats `<0x04>` characters outside of string literals as a 'skip the next character' instruction when parsing, i.e. `RCDATA<0x04>x` gets parsed as if it were `RCDATA`. It's possible to emulate this behavior in `resinator`, but it seems unlikely that any real `.rc` files are intentionally using this behavior, so by making it a compile error it avoids running into strange behavior if a `<0x04>` character is ever inserted into a `.rc` file accidentally.
- *[Not final]* In `resinator`, embedded 'Delete' (`<0x7F>`) characters are always illegal anywhere in a `.rc` file.
  + The Windows RC compiler seemingly treats `<0x7F>` characters as a terminator in some capacity. A few examples:
    - `1 RC<0x7F>DATA {}` gets parsed as `1 RC DATA {}` 
    - `<0x7F>1 RCDATA {}` "succeeds" but results in an empty `.res` file (no RCDATA resource)
    - `1 RCDATA { "<0x7F>" }` fails with `unexpected end of file in string literal`
  + It's possible to emulate this behavior in `resinator`, but it seems unlikely that any real `.rc` files are intentionally using this behavior, so by making it a compile error it avoids running into strange behavior if a `<0x7F>` character is ever inserted into a `.rc` file accidentally.
- In `resinator`, the sequence `\"` within a string literal is an error, noting that `""` should be used to escape quotes within string literals.
  + The Windows RC compiler is super permissive in what input it will accept, but in this case it is overly so. For example, if you have `1 RCDATA { "\""BLAH" }` with `#define BLAH 2` elsewhere in the file:
    - A preprocessor would treat the `\"` as an escaped quote and parse the part after the `{` as: `"\""`, `BLAH`, `" }`; it would then replace `BLAH` with `2` since it thinks it's outside of a string literal (note: the preprocessor would also normally result in a `missing terminating '"' character` warning since the `" }` string is never closed; in the Windows RC compiler this warning is either not emitted or not shown to the user).
    - The RC compiler would then get the resulting `1 RCDATA { "\""2" }` and parse the part after the `{` as: `"\""2"`, `}`, since in `.rc` string literals, `""` is an escaped quote, not `\"`. In the Windows RC compiler, `\"` is a weird special case in which it both treats the `\` as an escape character (in that it doesn't appear in the parsed string), but doesn't actually escape the `"` (note that other invalid escapes like e.g. `\k` end up with both the `\` and `k` in the parsed string).
  + The fact that `\"` makes it possible for macro expansion to silently happen within what the RC compiler thinks are string literals is enough of a footgun that it makes sense to make it an error instead. Note also that it can lead to really bizarre edge cases/compile errors when, for example, particular control characters follow a `\""` sequence in a string literal.

### Unavoidable divergences from the MSVC++ `rc` tool

- In `resinator`, splices (`\` at the end of a line) are removed by the preprocessor before checking if any string literals are too long.
  + The Windows RC compiler includes the splice characters in the string literal length check (even though they don't show up in the string literal).
- In `resinator`, embedded 'carriage return' characters (that are not part of a `CRLF` pair) can lead to files being parsed differently than the Windows RC compiler would parse them.
  + The `clang` preprocessor treats carriage return characters (`'\r'`) as a line separator when unpaired, and always converts them to new lines (`'\n'`). The Windows RC tool instead seemingly ignores/skips all `\r` characters.
  + For example, `RC<\r>DATA` will be compiled by the Windows RC tool as if it were `RCDATA`, but `clang`'s preprocessor will convert it to `RC<\n>DATA` which `resinator` will parse as separate `RC` and `DATA` tokens.

## Status

- Lexer
  + Mostly working for what's been implemented so far. Still possible that this will significantly change pending future discoveries about `.rc` files.
- Parsing
  + Converts the token list into an AST. Supports a few of the simpler resource types (RCDATA, ICON, CURSOR, user-defined), but doesn't handle malformed resources all that well.
- Compiling
  + Converts the AST into the binary `.res` file. Supports most of the resources that the parser supports (RCDATA, ICON, CURSOR, etc).
    + ICON and CURSOR `.ico` parsing is supported, but may not be fully correct yet.
