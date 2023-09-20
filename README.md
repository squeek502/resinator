![](https://www.ryanliptak.com/images/resinator-dynamic-32.svg) resinator
=========

A cross-platform Windows resource-definition script (.rc) to resource file (.res) compiler. The intention is for this to [get merged into the Zig compiler](https://github.com/ziglang/zig/pull/17069) as per [this accepted proposal](https://github.com/ziglang/zig/issues/3702), but it will also be maintained as a separate tool.

- This is a fully [clean-room](https://en.wikipedia.org/wiki/Clean_room_design) implementation, using [fuzz testing](#testing-resinator) as the primary method of determining how `rc.exe` works and how compatible `resinator` is with its implementation.
  + See [this talk](https://www.youtube.com/watch?v=RZczLb_uI9E) for a deeper dive into this
- As of now, `resinator` can successfully compile every `.rc` file in the [Windows-classic-samples repo](https://github.com/microsoft/Windows-classic-samples) byte-for-byte identically to the Windows RC compiler when using the includes from MSVC/the Windows SDK. This is tested via [win32-samples-rc-tests](https://github.com/squeek502/win32-samples-rc-tests).
- [Documentation](https://squeek502.github.io/resinator/)

## Overview

A Windows resource-definition file (`.rc`) is made up of both C/C++ preprocessor commands and resource definitions.

- The preprocessor commands are evaluated first via either `zig` or `clang` (one or the other must be available)
- The preprocessed `.rc` file will then be compiled into a `.res` file
- The `.res` file can then be linked into an executable by a linker

`resinator` is similar to `llvm-rc` and GNU's `windres`, in that it aims to be a cross-platform alternative to [the Windows `rc.exe` tool](https://learn.microsoft.com/en-us/windows/win32/menurc/using-rc-the-rc-command-line-).

However, unlike `llvm-rc` and `windres` (see [this section](#an-example-of-the-differences-between-the-implementations)), `resinator` aims to get as close to 1:1 compatibility with the Windows `rc.exe` implementation as possible. That is, the ideal would be:

- The `.res` output of `resinator` should match the `.res` output of the Windows `rc.exe` in as many cases as possible (if not exactly, then functionally). However, `resinator` will not support all valid `.rc` files (i.e. `#pragma code_page` support will be limited to particular code pages).
- `resinator` should fail to compile `.rc` files that the Windows `rc.exe` tool fails to compile.

In practice, though, 1:1 compatibility is not actually desirable, as there are quirks/bugs in the `rc.exe` implementation that `resinator` attempts to handle better.

## Comparison to `rc.exe` (the Win32 RC compiler)

`resinator` is a drop-in replacement for `rc.exe` with some improvements, some known exceptions, and some caveats.

#### `resinator` relies on an external preprocessor

`rc.exe` is self-contained and includes its own preprocessor implementation, while `resinator` relies on either `zig` or `clang` (in that order of preference) to handle preprocessing.

#### `resinator` has better error messages

![](https://github.com/squeek502/resinator/assets/2389051/1d2a6dd1-dbcb-4df8-88b1-ab1b84ea36eb)

#### `resinator` handles edge cases better and avoids miscompilations

`resinator` avoids some miscompilations of the Win32 RC compiler and generally handles edge cases better (e.g. resource data size overflow). See [this exhaustive list of intentional differences between `resinator` and `rc.exe`](https://squeek502.github.io/resinator/divergences.html).

Also, `resinator` will emit helpful warnings/errors for many of these differences:

![](https://github.com/squeek502/resinator/assets/2389051/85150176-a06f-47a4-9e80-ed15961a1d72)

#### `resinator` does not support UTF-16 encoded scripts

This is a limitation of the preprocessor that is used by `resinator` (see [this issue](https://github.com/squeek502/resinator/issues/5)).

#### `resinator` will auto-detect system include paths by default

`.rc` files can `#include` files from MSVC/Windows SDKs (most commonly, `windows.h`). The paths for these files are typically provided via the `INCLUDE` environment variable or the `/i` command line option. `resinator` supports the same options, but it will also try to auto-detect system include paths on Windows, or use the MinGW includes bundled with Zig if `zig` is being used as the preprocessor (meaning that if `zig` is used as the preprocessor, `#include "windows.h"` will work by default on Linux).

This behavior can be controlled with the `/:auto-includes` CLI option.

## Comparison to `windres` and `llvm-rc`

| Feature | `resinator` | `windres` | `llvm-rc` |
| --- | --- | --- | --- |
| [Identical outputs for `.rc` files in `Windows-classic-samples`](https://github.com/squeek502/win32-samples-rc-tests) | ✅ | ❌ | ❌ |
| UTF-16 encoded `.rc` support | ❌ | ❌ | ❌ |
| CLI compatibility | ✅ | ❌ | ✅ |
| Cross-platform | ✅ | ✅ | ✅ |
| Support for outputting `.rc` files | ❌ | ✅ | ❌ |
| Support for outputting COFF object files | ❌ | ✅ | ❌ |

### An example of the differences between the implementations

Here is an example `.rc` script that is handled differently by each of `windres`, `llvm-rc`, and the canonical Windows `rc.exe` implementation:

```c
// <id> <resource type> { <data> }
1 "FOO" { "bar" }
```

- `rc.exe` compiles this to a `.res` file with a resource of ID `1` that has the type `"FOO"` (the quotes are part of the user-defined resource type name)
- `windres` compiles this to a `.res` file with a resource of ID `1` that has the type `FOO` (the `"FOO"` is parsed as a quoted string)
- `llvm-rc` errors on this file with: `Error: expected int or identifier, got "FOO"`
- `resinator` matches the `rc.exe` behavior exactly in this case

This particular example is mostly inconsequential in terms of real-world `.rc` files, but it is indicative of how closely the different implementations conform with the `rc.exe` behavior. See [win32-samples-rc-tests](https://github.com/squeek502/win32-samples-rc-tests) for a more wide-ranging comparison on real-world `.rc` files.

## Compiling `resinator`

See [the version of Zig used in CI](https://github.com/squeek502/resinator/blob/master/.github/workflows/ci.yml#L23) to determine which version of Zig should be used to build `resinator`.

```
git clone https://github.com/squeek502/resinator
cd resinator
zig build
```

## Testing `resinator`

```
zig build test
```

### 'Fuzzy' tests

The 'fuzzy' tests are a collection of tests that may be similar to either fuzz testing or property testing. With default settings, the more fuzzing-like tests will run for 1000 iterations. These tests rely on `rc.exe` being available on `PATH` since each test uses `rc.exe` as an oracle to determine if `resinator` behavior is expected or not.

```
zig build test_fuzzy
```

will run all of the 'fuzzy' tests.

Each 'fuzzy' test can be run individually, too. For example, this will run the `fuzzy_numbers` test infinitely:

```
zig build test_fuzzy_numbers -Diterations=0
```

### Fuzz testing

#### On Windows

- Requires [winafl](https://github.com/googleprojectzero/winafl) to be installed/built.
- Requires `rc.exe` to be available on the `PATH`

To build the fuzzer:

```
zig build fuzz_winafl
```

To run the fuzzer, from within the `winafl/build64/bin` directory (replace `C:\path\to\` with the actual path to the relevant directories):

```
set PATH_TO_INPUTS_DIR=C:\path\to\resinator\test\inputs
set PATH_TO_OUTPUTS_DIR=C:\path\to\resinator\test\outputs
set PATH_TO_DYNAMORIO_BIN=C:\path\to\DynamoRIO-Windows\bin64
set PATH_TO_RESINATOR_FUZZER=C:\path\to\resinator\zig-out\bin\fuzz_winafl.exe
afl-fuzz.exe -i "%PATH_TO_INPUTS_DIR%" -o "%PATH_TO_OUTPUTS_DIR%" -D "%PATH_TO_DYNAMORIO_BIN%" -t 20000 -- -coverage_module fuzz_winafl.exe -target_module fuzz_winafl.exe -target_method fuzzMain -fuzz_iterations 5000 -nargs 2 -- "%PATH_TO_RESINATOR_FUZZER%" @@
```

#### On Linux

Currently, Linux fuzz testing only tests the `resinator` implementation for crashes/bugs, and does not check for correctness against `rc.exe`.

- Requires [afl++](https://github.com/AFLplusplus/AFLplusplus) with `afl-clang-lto` to be installed.

```
zig build fuzz_rc
```

To run the fuzzer:
```
afl-fuzz -i test/inputs -o test/outputs -- ./zig-out/bin/fuzz_rc
```

Ideally, this fuzzer would compare the outputs against `rc.exe` but `wine` was not able to perfectly emulate `rc.exe` last I tried (need to investigate this more, I was using a rather old version of wine).
