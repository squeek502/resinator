resinator
=========

A (work-in-progress) cross-platform Windows resource-definition script (.rc) to resource file (.res) compiler. The intention is for this to eventually get merged into the Zig compiler as per [this accepted proposal](https://github.com/ziglang/zig/issues/3702), but it will also be maintained as a separate tool.

[WIP dumping ground for various `.rc`/`.res` documentation](https://squeek502.github.io/resinator/)

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
- In `resinator`, embedded 'End of Transmission' (`<0x04>`) characters are always illegal outside of string literals.
  + The Windows RC compiler seemingly treats `<0x04>` characters outside of string literals as a 'skip the next character' instruction when parsing, i.e. `RCDATA<0x04>x` gets parsed as if it were `RCDATA`. It's possible to emulate this behavior in `resinator`, but it seems unlikely that any real `.rc` files are intentionally using this behavior, so by making it a compile error it avoids running into strange behavior if a `<0x04>` character is ever inserted into a `.rc` file accidentally.
- In `resinator`, embedded 'Delete' (`<0x7F>`) characters are always illegal anywhere in a `.rc` file.
  + The Windows RC compiler seemingly treats `<0x7F>` characters as a terminator in some capacity. A few examples:
    - `1 RC<0x7F>DATA {}` gets parsed as `1 RC DATA {}`
    - `<0x7F>1 RCDATA {}` "succeeds" but results in an empty `.res` file (no RCDATA resource)
    - `1 RCDATA { "<0x7F>" }` fails with `unexpected end of file in string literal`
  + It's possible to emulate this behavior in `resinator`, but it seems unlikely that any real `.rc` files are intentionally using this behavior, so by making it a compile error it avoids running into strange behavior if a `<0x7F>` character is ever inserted into a `.rc` file accidentally.
- In `resinator`, embedded 'Substitute' (`<0x1A>`) characters are always illegal anywhere in a `.rc` file. Note: The preprocessor treats it as an 'end of file' marker so instead of getting an error it will likely end up as an EOF error. This would change if `resinator` uses a custom preprocessor.
  + The Windows RC compiler treats `<0x1A>` characters as an 'end of file' maker but it can also lead to (presumed) infinite loops. For example, `1 MENUEX FIXED<0x1A>VERSION` will cause the Win32 implementation to hang, but `1 RCDATA {} <0x1A> 2 RCDATA {}` will succeed but only the `1 RCDATA {}` resource will make it into the `.res`.
- In `resinator`, the sequence `\"` within a string literal is an error, noting that `""` should be used to escape quotes within string literals.
  + The Windows RC compiler is super permissive in what input it will accept, but in this case it is overly so. For example, if you have `1 RCDATA { "\""BLAH" }` with `#define BLAH 2` elsewhere in the file:
    - A preprocessor would treat the `\"` as an escaped quote and parse the part after the `{` as: `"\""`, `BLAH`, `" }`; it would then replace `BLAH` with `2` since it thinks it's outside of a string literal (note: the preprocessor would also normally result in a `missing terminating '"' character` warning since the `" }` string is never closed; in the Windows RC compiler this warning is either not emitted or not shown to the user).
    - The RC compiler would then get the resulting `1 RCDATA { "\""2" }` and parse the part after the `{` as: `"\""2"`, `}`, since in `.rc` string literals, `""` is an escaped quote, not `\"`. In the Windows RC compiler, `\"` is a weird special case in which it both treats the `\` as an escape character (in that it doesn't appear in the parsed string), but doesn't actually escape the `"` (note that other invalid escapes like e.g. `\k` end up with both the `\` and `k` in the parsed string).
  + The fact that `\"` makes it possible for macro expansion to silently happen within what the RC compiler thinks are string literals is enough of a footgun that it makes sense to make it an error instead. Note also that it can lead to really bizarre edge cases/compile errors when, for example, particular control characters follow a `\""` sequence in a string literal.
- In `resinator`, trying to use a raw data block with resource types that don't support raw data is an error, noting that if `{` or `BEGIN` is intended as a filename, it should use a quoted string literal.
  + The Windows RC compiler will instead try to interpret `{`/`BEGIN` as a filename in these cases, which is extremely likely to fail and (if it succeeds) is almost certainly not what the user intended.
- In `resinator`, trying to use a number expression (e.g. `(1+1)`) as a filename is an error, noting that a quoted string literal should be used instead. Singular number literals are allowed, though (e.g. `-1`).
  + The Windows RC compiler will allow any number expression as a filename, but evaluate it such that it is almost certainly not what the user intended--it will take the last number literal in the expression and treat that as the filename. Examples: `(1+1)` -> `1`, `1+-1` -> `-1`.
- In `resinator`, the byte order mark (`<U+FEFF>`) is always illegal anywhere in a `.rc` file except the very start.
  + For the most part, the Windows RC compiler skips over BOMs everywhere, even within string literals, within names, etc [e.g. `RC<U+FEFF>DATA` will compile as if it were `RCDATA`]). However, there are edge cases where a BOM will cause cryptic 'compiler limit : macro definition too big' errors (e.g. `1<U+FEFF>1` as a number literal).
    - The use-case of BOM outside of the start of the file seems extremely minimal/zero, so emulating the Windows RC behavior doesn't seem worth the added complexity.
    - The potentially unexpected behavior of the BOM bytes missing from the compiled `.res` seems worth avoiding (e.g. a string literal with the contents `"<U+FEFF>"` would be compiled as if it were an empty string).
- In `resinator`, the private use character `<U+E000>` is always illegal anywhere in a `.rc` file.
  + This behaves similarly to the byte order mark (it gets skipped/ignored wherever it is), so the same reasoning applies (although `<U+E000>` seems to avoid causing errors like the BOM does).
- In `resinator`, control characters specified as a quoted string with a `^` in an `ACCELERATORS` resource (e.g. `"^C"`) must be in the range of `A-Z` (case insensitive).
  + The Windows RC compiler's error hints that this is the intended behavior (`control character out of range [^A - ^Z]`), but it actually allows for a large number of codepoints >= 0x80 to be used. Of those allowed, it treats them as if they were `A-Z` and subtracts `0x40` from the codepoint to convert it to a 'control character', but for arbitrary non-ASCII codepoints that just leads to garbage. The codepoints that are allowed may be based on some sort of Unicode-aware 'is character' function or something, but I couldn't really find a pattern to it. The full list of codepoints that trigger the error can be found [here](https://gist.github.com/squeek502/2e9d0a4728a83eed074ad9785a209fd0).
- In `resinator`, the codepoints `U+0900`, `U+0A00`, `U+0A0D`, `U+2000`, `U+FFFE`, and `U+0D00` are illegal outside of string literals, and emit a warning if used inside string literals
  + The Windows RC compiler will error and/or ignore these codepoints when used outside of string literals, but not always. When used within string literals, the Windows RC compiler will miscompile them (either swap the bytes of the UTF-16 code unit in the `.res`, omit it altogether, or some other strange interaction).
- `resinator` will avoid a miscompilation regarding padding bytes after 'extra data' in DIALOG controls, and will emit a warning when it detects that the Windows RC compiler would miscompile
  + The Windows RC compiler will erroneously add too many padding bytes after the 'extra data' section of a DIALOG control if the data ends on an odd offset. This is a miscompilation that results in the subsequent dialog control not to be DWORD aligned, and will likely cause the dialog to be unusable (due to parse errors during dialog initialization at program runtime).
    - As far as I can tell, there is no actual use-case for this extra data on controls in a templated DIALOG, as [the docs](https://learn.microsoft.com/en-us/windows/win32/menurc/common-control-parameters) say that "When a dialog is created, and a control in that dialog which has control-specific data is created, a pointer to that data is passed into the control's window procedure through the lParam of the WM_CREATE message for that control", but `WM_CREATE` is not sent for dialogs (instead only `WM_INITDIALOG` is sent after all of the controls have been created).
- `resinator` will avoid a miscompilation when a generic CONTROL has its control class specified as a number, and will emit a warning
  + The Windows RC compiler will incorrectly encode control classes specified as numbers, seemingly using some behavior that might be left over from the 16-bit RC compiler. As far as I can tell, it will always output an unusable dialog template if a CONTROL's class is specified as a number.
- `resinator` will avoid a miscompilation when a `VALUE` within a `VERSIONINFO` has the comma between its key and its first value omitted (but only if the value is a quoted string), and will emit a warning
  + The Windows RC compiler will fail to add padding to get to `DWORD`-alignment before the value and sometimes step on the null-terminator of the `VALUE`'s key string.
- `resinator` will avoid a miscompilation when a `VALUE` within a `VERSIONINFO` mixes numbers and strings, and will emit a warning
  + The Windows RC compiler will miscompile the byte count of values that have a mix of numbers and strings, as it will use UTF-16 code units as the 'byte count' for all of the string values, leading to an incorrect size value being written to the .res.
  + Note that there are further miscompilations with a mix of numbers and strings that `resinator` avoids but does not have specific warnings for:
    + `VALUE "key", 65535L "a"` will compile into `0xFFFF` and `"a\x00"` instead of the expected `0x0000FFFF` and `"a\x00`, but either (1) adding a comma after `65535L` or (2) bumping up the value to be larger than u16 max (e.g. `65538L`) will cause it to be compiled into a `u32`.
    + `VALUE "key", 65538L "a"` has a 'byte count' of 5 in the compiled `.res` while `VALUE "key", 65538L, "a"`  (added comma after `65538L`) has a 'byte count' of 6 in the compiled `.res` (note: both of these byte counts are incorrect as per the main miscompilation above; the accurate byte count would be 8 [4 for the `u32` and 4 for the string--2 for the `'a'` and 2 for the null terminator])
- *[Still being figured out]* `resinator` supports `NOT` expressions only in `STYLE` statements, `EXSTYLE` statements, and `style`/`exstyle` parameters, and the `NOT` must immediately precede a number literal (as opposed to a number expression--e.g. `NOT 1` works but `NOT (1)` does not)
  + There's a lot of things about the Windows RC compiler's implementation of the `NOT` expression that I either don't understand or don't think is worth emulating:
    - The Windows RC compiler allows the use of `NOT` expressions in places where it doesn't make sense (the `x`, `y`, etc parameters of `DIALOGEX` for example)
    - The parsing rules of `NOT` expressions change depending on the particular parameter they are being used in, e.g. `1 | NOT 2` is an error if used in the `type` parameter of a `MENUEX`'s `MENUITEM`, but perfectly fine if used in the `style` or `exstyle` parameters of a `DIALOG`/`DIALOGEX` control.
    - The parsing rules of `NOT` expressions are generally bizarre and the Windows RC compiler both allows for seemingly nonsensical expressions and evaluates seemingly normal expressions in nonsensical ways. For example:
      + `7 NOT NOT 4 NOT 2 NOT NOT 1`, `NOT (1 | 2)`, and `NOT () 2` are all allowed and all evaluate to `2`
- `resinator` will error if the resource type (`ICON`/`CURSOR`) doesn't match with the resource type specified in the `.ico`/`.cur` file
  + The Win32 RC compiler will happily compile such a mismatch, but the resulting `.res` will always fail to load the `CURSOR`/`ICON` at runtime, since it will compile the data as if it were the type specified in the file, but compile the resource as if it were the type specified in the `.rc` script.
- `resinator` will allow `PNG` encoded icons within `CURSOR` resource groups
  + The Win32 RC compiler will fail to compile such cursors, but there doesn't seem to be any reason why that is the case as far as I can tell--they can be loaded at runtime just fine in my (albeit limited) testing. Note also that `PNG` encoded icons are allowed within `ICON` resources by the Win32 RC compiler, and `CURSOR` resource groups have the same format.
- *[Not final, somewhat likely to change]* `resinator` will allow `RIFF` encoded animated icons within `ICON` (but not `CURSOR`) resource groups
  + The Win32 RC compiler will fail to compile such icons, but there seems to be some support for loading them at runtime if they are written to the `.res`; however, they don't work everywhere. This needs to be looked into more, but most likely this will just be made into a compile error since it's fully unknown territory--there's nothing anywhere that mentions combining `RIFF` animated icons and `RT_GROUP_ICON`.
- `resinator` will error if an `ICON`/`CURSOR` has an image with a data size that is impossibly large
  + The Win32 RC compiler takes the image's reported data size at face value, even if the data size is larger than the size of the entire `.ico`/`.cur` file. When the reported size extends past the EOF, it will still write the full size to the .res file, but everything past the EOF will be zeroes and then that pattern will repeat every 0x4000 bytes (the file contents again, zeroes until the next 0x4000 byte boundary, and so on). This means that a malicious `.ico`/`.cur` file could result in essentially unbounded `.res` filesize (each image within the resource group can report its data size as up to 4GB, and there can be 65535 images in a resource group).
    + There is a second bug which makes the Win32 RC compiler infinitely loop until all disk space has been exhausted: the data sizes of images are seemingly interpreted as signed integers, and values that it interprets as negative will (AFAICT) be treated as infinite. When using the `/v` flag it will output something like `Writing ICON:1, lang:0x409, size -6000000` and then keep writing to the `.res` file well beyond the limit of a `u32`.
- `resinator` will error if an `ICON`/`CURSOR` has an image with a data size that is impossibly small
  + If the reported data size of an image is zero, the Win32 RC compiler will miscompile and make up a bogus data size when writing the resource's `.res` data, but while also not including any actual data bytes (making the `.res` unusable)
  + If the reported data size of an image is smaller than the header of a bitmap/PNG, then the Win32 RC compiler will both use the reported size but also read the full header of the image and use it to inform the values of the resource (e.g. it will detect a PNG and set num_planes to 1).
  + By always erroring on sizes that can't possibly hold a real image, `resinator` avoids both of these behaviors that always lead to invalid `.res` files.
- `resinator` will allow bitmap `ICON`/`CURSOR` images with versions other than `Windows NT, 3.1x (BITMAPINFOHEADER)` (e.g. `BITMAPV5HEADER`, unknown versions, etc), and will emit a warning
  + The Win32 RC compiler is strict about the DIB bitmap version it allows, and errors on anything that's not `Windows NT, 3.1x (BITMAPINFOHEADER)`. This needs to be tested, but my hunch is that this limitation is artificial, and that if newer bitmap formats were included in a `.res` that they would be able to be loaded at runtime just fine.
- `resinator` will avoid a miscompilation when padding missing bytes in the color table of `BITMAP` resources; `resinator` will always fill the missing bytes with zeroes, and will emit a warning
  + The Win32 RC compiler will fill missing color palette bytes with zeroes if the size of the color palette is specified to be larger than it actually is in the file, but it will read into the pixel data and use that until it reaches EOF. This is almost certainly not what is intended, as pixel data and color table data do not have the same format so it'll essentially be filling the missing bytes with random data.
- `resinator` will error on `BITMAP` resources that contain more colors than their bit depth allows (2^n, e.g. bit depth 4 can have a maximum of 16 colors, bit depth 8 can have a maximum of 256 colors, etc)
  + The Win32 RC compiler does not enforce this, but such bitmaps are definitely malformed. Even for bit depths >= 16 where '[the] bmiColors color table is used for optimizing colors used on palette-based devices', it still wouldn't make sense for the number of colors in the palette to exceed the possible number of colors used by the pixel data.
  + Note: Enforcing this limit avoids malformed bitmaps potentially inducing really large `.res` files
- `resinator` will error on `BITMAP` resources with a color palette that contains too many missing bytes. The particular limit is configurable (note: this being configurable is still TODO) and the default is 4096.
  + The Win32 RC compiler does no checking of this, and will either give an out-of-memory error or just unconditionally write huge amounts of zeroes to the color palette (e.g. if the bit depth of the `.bmp` is 32 and the number of colors used is set to `200,000,000`, then `num_colors * 4` or `800,000,000` bytes will be written to the color palette). Such large amounts of colors are extremely likely be indicative of a malformed bitmap, as bit depths 16, 24, and 32 only use color tables for optimization purposes (["The bmiColors color table is used for optimizing colors used on palette-based devices, and must contain the number of entries specified by the biClrUsed member of the BITMAPINFOHEADER."](https://learn.microsoft.com/en-us/previous-versions//dd183376(v=vs.85))).
  + Note: Enforcing a limit here avoids malformed bitmaps potentially inducing really large `.res` files
- `resinator` will always error if the code page specified in a `#pragma code_page` directive would overflow a `u32`.
  + The Win32 RC compiler has different behavior depending on whether or not the value after wrapping on overflow ends up being a known code page ID or not:
    - If the overflowed `u32` wraps and becomes a known code page ID, then it will error/warn with "Codepage not valid:  ignored" (depending on the `/w` option)
    - If the overflowed `u32` wraps and does not become a known code page ID, then it will error with 'constant too big' and 'Codepage not integer'
- `resinator` will error if any expression is a single unquoted `)` character.
  + The Win32 RC compiler treats a single `)` as a 'valid' expression that essentially evaluates to something akin to a 'skip this' instruction when parsing.
    - When used as a filename it causes strange behavior where it parses as if it were the filename but then it uses the preceding token when actually doing the filename lookup. For example, `1 RCDATA )` will give the error `file not found: RCDATA` rather than the expected `file not found: )`.
    - When used within a raw data block, it will just skip it as if it wasn't there at all. For example, `1 RCDATA { 1, ), 2 }` will be treated as if it were `1 RCDATA { 1, 2 }`.
  + Note: The Win32 behavior is not emulated because it very likely has many unexplored edge cases that have very bizarre behavior, and it's very unlikely that (1) this is anything but a bug in the Win32 implementation and (2) there are any valid use-cases of this bug
- `resinator` will error if a resource's evaluated filename contains a `NUL` (`<0x00>`) character.
  + The Win32 RC compiler will treat the `NUL` character as a terminator (e.g. `1 RCDATA "hello\x00world"` will look for a file named `hello`), but that behavior seems unlikely to be useful and worth disallowing.
- `resinator` will error whenever a unary + operator is used, and emit a note about unary + not being supported in `resinator`'s implementation.
  + The Win32 RC compiler allows `+` as a unary operator in some places but not others. In things like raw data blocks, it gives an error like `unexpected value in RCDATA`. In `DIALOG` parameters, it does not always give an error (e.g. `+1` is allowed but `(+1)` gives an error).

#### Resource data and `.res` filesize limits

- `resinator` will error if a resource's data length exceeds the max of a `u32`, since the header of the resource needs to be able to specify its data length as a `u32`.
  + The Win32 RC compiler will `fatal error RW1023: I/O error seeking in file` if the resulting `.res` filesize ever exceeds 2GiB (2,147,483,648 bytes). This indirectly limits the size of individual resources; the largest possible resource can be slightly smaller than 2GiB if it's the only resource in the `.res` (slightly smaller than 2GiB to allow for the resource headers, etc).
- `resinator` will error if a `VERSIONINFO` resource contains a node tree that is larger than the max of a `u16`, since the root node needs to be able to specify its byte length (inclusive of all children) as a `u16`.
  + The Win32 RC compiler will not error and instead the node's byte length will overflow and wrap back around to 0. This leads to an invalid version node tree.
- `resinator` will error if a `DIALOG`/`DIALOGEX` resource contains more controls than the max of a `u16`, since the dialog needs to be able to specify its number of controls as a `u16`.
  + The Win32 RC compiler will not error and instead the number of controls will overflow and wrap back around to 0. This leads to an incorrect dialog resource.
- `resinator` will error if a control within a `DIALOG`/`DIALOGEX` resource contains more extra data than the max of a `u16`, since the dialog needs to be able to specify the number of extra data bytes of each control as a `u16`.
  + The Win32 RC compiler will not error and instead the extra data length of the control will overflow and wrap back around to 0. This leads to an incorrect dialog resource.

### Unavoidable divergences from the MSVC++ `rc` tool

- `resinator` does not support UTF-16 encoded `.rc` files.
  + The `clang` preprocessor does not handle UTF-16 encoded files (i.e. it will fail to parse `#include` directives, etc within UTF-16 encoded files). So, `resinator` would need a preprocessor that handles UTF-16 encoded files for UTF-16 support to be feasible.
- In `resinator`, splices (`\` at the end of a line) are removed by the preprocessor before checking if any string literals are too long.
  + The Windows RC compiler includes the splice characters in the string literal length check (even though they don't show up in the string literal).
- In `resinator`, embedded 'carriage return' characters (that are not part of a `CRLF` pair) can lead to files being parsed differently than the Windows RC compiler would parse them.
  + The `clang` preprocessor treats carriage return characters (`'\r'`) as a line separator when unpaired, and always converts them to new lines (`'\n'`). The Windows RC tool instead seemingly ignores/skips all `\r` characters.
  + For example, `RC<\r>DATA` will be compiled by the Windows RC tool as if it were `RCDATA`, but `clang`'s preprocessor will convert it to `RC<\n>DATA` which `resinator` will parse as separate `RC` and `DATA` tokens.
- `.rc` files that use splices (`\` at the end of a line) within strings that include whitespace after the splice will be handled differently.
  + The Win32 RC compiler's preprocessor seems to collapse whitespace after a splice that's within a string, while `clang`'s preprocessor does not. An example of a file for which this behavior difference can be reproduced can be found [here](https://github.com/microsoft/Windows-classic-samples/blob/7cbd99ac1d2b4a0beffbaba29ea63d024ceff700/Samples/Win7Samples/winui/shell/appshellintegration/NonDefaultDropMenuVerb/NonDefaultDropMenuVerb.rc#L10-L20).
- `.rc` files that have unescaped newline characters within string literals will be handled differently.
  + The Win32 RC compiler's preprocessor leaves whitespace alone on lines that it doesn't think are string literals, but the `clang` preprocessor will collapse whitespace on lines it doesn't think are string literals. Some of the RC compiler docs say that string literals ["must occupy a single line in the source file"](https://learn.microsoft.com/en-us/windows/win32/menurc/stringtable-resource) but multiline string literals without splices/line continuations are empirically allowed, so this difference in whitespace handling can lead to differences in the parsed string. Note that this is a similar difference to the splice difference in the previous point but in the reverse direction (that is, in this case, Windows does not collapse whitespace but clang does).

### Found divergences that haven't been decided on yet

- The Win32 RC compiler allows unclosed parentheses in certain locations. Examples: `1 DIALOGEX 1( 2, 3, 4 {}`, `1 DIALOGEX 1,(2, 3, 4 {}`, and many more.
  + This only happens with some things, e.g. `1 RCDATA { 1,(2, 3, 4 }` will error with `mismatched parentheses`.
  + **Current thoughts:** Seems like a bug in the Win32 implementation to not error with `mismatched parentheses`. Don't really see a reason to be bug-for-bug compatible with this one.
  + **Current `resinator` behavior:** `error: expected ')', got ','`
- The Win32 RC compiler allows the `style` parameter of `CONTROL`s within `DIALOG`/`DIALOGEX` resources to be specified as things like a quoted string, the character `=`, or even control class keywords (`EDIT`, `BUTTON`, etc) which are then always evaluated as `0`. Example: `CONTROL "text", 1, BUTTON, "50", 1, 2, 3, 4`; the `"50"` is the style parameter and gets written to the `.res` as `0x00000000`.
  + This does not apply to `x`, `y`, `w`, or `h`: Win32 compiler errors with `expected numerical dialog constant` if they are specified as a quoted string
  + This does not apply to `exstyle` or `helpid`, which seems to make it parse differently and gives errors like `invalid control type`, `END expected in dialog`.
  + This does not apply to non-`CONTROL` dialog statements, if e.g. `"50"` is specified as the `style` parameter of a `CHECKBOX` then it behaves like `exstyle` or `helpid` in the previous bullet point.
  + **Current thoughts:** Seems like a bug in the Win32 implementation to accept quoted strings here. Don't really see a reason to be bug-for-bug compatible with this one.
  + **Current `resinator` behavior:** `error: expected number or number expression; got '"50"'`
- The Win32 RC compiler allows an extra expression after the `style` parameter of `CONTROL`s within `DIALOG`/`DIALOGEX` resources, but only if there is no comma separating them. Example: `CONTROL "text", 1, BUTTON, 15 30, 1, 2, 3, 4`; the `15` is the style parameter and the `30` is completely ignored (i.e. the `1, 2, 3, 4` are `x`, `y`, `w`, `h`).
  + The extra expression can be various things (quoted string, `=`, etc) but not *anything*, e.g. if it's `;` then it will error with `expected numerical dialog constant`
  + If the extra expression is a number expression it will no longer ignore it but will behave strangely, e.g. if it is `(30+5)` then `5` will be used as the `x` parameter
  + **Current thoughts:** Definitely a bug in the Win32 implementation. Don't really see a reason to be bug-for-bug compatible with this one, but will need to warn/error when it would affect the output. Most foolproof thing might be to warn whenever the comma is omitted after the style parameter of a `CONTROL` statement, since accounting for all the various miscompilations possible here is not worth the effort.
  + **Current `resinator` behavior:** `error: expected number or number expression; got '"30"'` if the extra expression is not a number expression, or it will treat the extra expression as the `x` parameter (this can lead to different `.res` outputs)
- The Win32 RC compiler will sometimes error if subtraction is performed with the right-hand-side evaluating to 0. Example: `1 DIALOGEX 1, 2, 3, 4-0 {}` will error with `BEGIN expected in dialog` (note: the lack of whitespace between the two operands seems to matter). Such expressions never seem to be errors within raw data blocks (e.g. `1 RCDATA { 4-0 }` works fine).
  + Some more expressions that error: `4-0x0`, `(4-0)`
  + Some allowed expressions: `4 - 0`, `100--0`, `4-(0)`
  + Some insight into the ramifications: `1 DIALOGEX 1, 2, 3, 10-0x0+5 {} hello` will error with `file not found: hello` and the verbose output will show `Writing {}:+5`, meaning it is interpreting the `+5 {} hello` after the `10-0x0` as a new resource with a name of `+5` and a resource type of `{}`.
  + **Current thoughts:** Ideally, `resinator` should successfully parse/compile such expressions, but emit a warning that the Win32 RC compiler would fail to compile them. This is easier said than done, though, since the conditions for when the error would occur are tough to pin down or understand.
  + **Current `resinator` behavior:** Successful compilation, e.g. `1 DIALOGEX 1, 2, 3, 4-0 {}` compiles as if it were `1 DIALOGEX 1, 2, 3, 4 {}`
- The Win32 RC compiler will error with `version WORDs separated by commas expected` if the character `l`/`L` is anywhere in a number literal within a parameter of a `PRODUCTVERSION` or `FILEVERSION` statement of a `VERSIONINFO` resource.
  + This is due to PRODUCTVERSION and FILEVERSION being a `u16` and the `l`/`L` suffix of a number indicating that it should be a `u32`.
  + It doesn't complain about numbers that overflow a `u16` and uses wrapping overflow as is common everywhere else.
  + **Current thoughts:** `resinator` should allow the `l`/`L` suffix but warn about it. Will also need to double check that this is the only place where the Win32 RC compiler errors upon seeing the `L` suffix.
  + **Current `resinator` behavior:** Successful compilation, but no warning about the Win32 behavior

## Status

Pretty much all known/documented resource types are supported (pending further investigation). `resinator` can successfully compile every `.rc` file in the [Windows-classic-samples repo](https://github.com/microsoft/Windows-classic-samples) byte-for-byte identically to the Win32 RC compiler (this may not be technically true, there are some `.rc` files that need intermediate generated files that aren't fully tested yet).

Plans for the future:
- Address the huge amount of TODO's that have accumulated
- Fuzz test like crazy
- [*huge* maybe] Investigate trying to implement a preprocessor that would work with UTF-16 files.
