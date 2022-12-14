---
title: Home
layout: home
---

Currently a dumping ground for various pieces of information related to `.rc` and `.res` files that I haven't found satisfying documentation for elsewhere. Might be organized into something more useful later.

## Random quirks of the MSVC++ `rc` tool

- Resource definitions that specify a filename can specify files in the include path of the `rc` compiler, e.g. `windows.h` and they will be found/used.

## Common Resource Attributes / Memory Flags

### Flags

| Attribute | Value |
|-----------|-------|
| `MOVEABLE` | `0x10` |
| `SHARED` | `0x20` |
| `PURE` | `0x20` |
| `PRELOAD` | `0x40` |
| `DISCARDABLE` | `0x1000` |

### [`RT_`](https://learn.microsoft.com/en-us/windows/win32/menurc/resource-types) Default Flags

{: .note }
> Default values use the flags directly rather than 'applying' the relevant Common Resource Attribute(s), and therefore can have values that are not possible when specifying memory flags via Common Resource Attributes.

| Default | Types |
|----|---------|
| `MOVEABLE | DISCARDABLE` | `RT_ICON`, `RT_CURSOR` |
| `MOVEABLE | SHARED` | `RT_RCDATA`, `RT_BITMAP`, `RT_HTML`, `RT_ACCELERATOR` |
| `MOVEABLE | SHARED | DISCARDABLE` | `RT_GROUP_ICON`, `RT_GROUP_CURSOR`, `RT_STRING`, `RT_FONT`, `RT_DIALOG` |
| `MOVEABLE | PRELOAD` | `RT_FONTDIR` |

### Common Resource Attribute effects

| Attribute | Effect |
|-----------|--------|
| `PRELOAD` | `flags | PRELOAD` |
| `LOADONCALL` | `flags & ~PRELOAD` |
| `MOVEABLE` | `flags | MOVEABLE` |
| `FIXED` | `flags & ~(MOVEABLE | DISCARDABLE)` |
| `SHARED` | `flags | SHARED` |
| `NONSHARED` | `flags & ~(SHARED | DISCARDABLE)` |
| `PURE` | `flags | PURE` |
| `IMPURE` | `flags & ~(PURE | DISCARDABLE)` |
| `DISCARDABLE` | `flags | (DISCARDABLE | MOVEABLE | SHARED)` |

## `FONT` resource

- The `<id>` in the `<id> FONT` definition **must** be an ordinal, not a string.
- The `<id>` of each `FONT` must be unique, but this does not fail the compilation, only emits an error. The first `FONT` defined with the `<id>` takes precedence, all others with the same `<id>` are ignored.
- Each `FONT` is stored with type `RT_FONT`. The entire binary contents of the sepcified file are the data. No validation/parsing is done of the data.

At the end of the .res, a single `RT_FONTDIR` resource with the name `FONTDIR` is written with the data:

| Size/Type | Description |
|-----------|--------|
| `u16` | Number of total font resources |
| - | *Below is repeated for each `RT_FONT` in the `.res`* |
| `u16` | ID of the `RT_FONT` |
| 150 bytes | The first 150 bytes of the `FONT`'s file. If the file is smaller than 150 bytes, all missing bytes are filled with `0x00`. |

{: .note }
> There appears to be a bug in the MSVC++ `rc` tool where more than 150 bytes are written for a given `RT_FONT`, or the 150 bytes are not exactly the first 150 bytes of the file. It seems to be dictated by the length of the file and `0x00` bytes within the file.
> - If the file is 75 bytes or smaller with no null bytes, the `FONTDIR` data for it will be 149 bytes (the first `n` being the bytes from the file, then the rest are `0x00` padding bytes). After that, there will be `n` bytes from the file again, and then a final `0x00`.
> - If the file is between 76 and 140 bytes long with no null bytes, the MSVC++ `rc` tool will crash.
> - There seems to be a variable number of `0x00` bytes at the end of the 150 bytes in certain circumstances, e.g. if a file is 150 non-NULL bytes in a row, the `FONTDIR` data for it will be 148 bytes and then 2 `0x00` bytes. However, if a file is 31 non-NULL bytes, then a `0x00`, and then 110 more non-NULL bytes, the `FONTDIR` data for it will be the first 142 bytes from the file and then 8 `0x00` bytes.
>
> More investigation is needed to understand what's going on here, how it affects the usage, and what behavior actually matters/is correct in terms of what to replicate.

## `ACCELERATORS` resource

- Warning on `SHIFT` or `CONTROL` without `VIRTKEY`
- Warning on 'ASCII character not equivalent to virtual key code' (example: `"^C", 1, VIRTKEY`)
- If both `ASCII` and `VIRTKEY` are specified, `VIRTKEY` always takes precedence
- Things that differ or are unclear from the [documentation](https://learn.microsoft.com/en-us/windows/win32/menurc/accelerators-resource):
  + `options` and `type` can be intermixed, there is no enforced ordering
  + All parts of an accelerator require commas between them
  + "When VIRTKEY is specified and event contains a string, event ~~must be~~ *[will be transformed to be]* uppercase."
- Some notes about how `event` is compiled:
  + `"^^"` -> `^`
  + `"ab"` -> `ba`
  + `"aba"` (any > 2 char long string) -> invalid accelerator

## `DIALOG` and `DIALOGEX` resources

- [The Old New Thing: `DIALOG`](https://devblogs.microsoft.com/oldnewthing/20040621-00/?p=38793)
- [The Old New Thing: `DIALOGEX`](https://devblogs.microsoft.com/oldnewthing/20040623-00/?p=38753)

### `DIALOGEX`

| Size/Type | Description |
|-----------|--------|
| `u16` | Dialog version (1 for `DIALOGEX`) |
| `u16` | `0xFFFF` for `DIALOGEX` |
| `u32` | Help ID |
| `u32` | Extended style |
| `u32` | Style |
| `u16` | Number of controls |
| `u16` | X |
| `u16` | Y |
| `u16` | Width |
| `u16` | Height |
| `NameOrOrdinal` | Menu |
| `NameOrOrdinal` | Dialog class |
| Null-teriminated UTF-16 String | Title |

- Multiple of each optional statement is allowed, and the last one specified takes precedence, with the exception of `CLASS` and `MENU` (see below).

#### `CLASS` and `MENU` quirks

- `MENU` is a proper `NameOrOrdinal`, in that it can be unquoted, contain " within it, etc.
  + However, it is parsed differently than a `NameOrOrdinal` used for resource id/types. Whenever the first character is a number, it is treated as a number. Anything after that is treated as a digit with the value `<UTF-16 code unit> - '0'` (where `'0'` is `0x30`, and using wrapping underflow for ascii values < 0x30). Some examples:
    - `3200` -> `3200` (ordinal)
    - `1+1` -> `51` (ordinal)
    - `1|2` -> `862` (ordinal)
    - `1a2` -> `592` (ordinal)
    - `1a` -> `59` (ordinal)
    - `1A` -> `27` (ordinal)
    - `1!` -> `65531` (ordinal)
    - `0??` -> `0x122` (ordinal, `??` is `U+0152`, so `- 0x30` is `0x122`)
    - `0<U+10002>` -> `0x49F2` (ordinal, `<U+10002>` is `0xD800 0xDC02` in UTF-16, so `0xD800 - 0x30` is `0xD7D0`, multiplied by 10 (with wrapping overflow) that is `0x6E20`, then `0xDC02 - 0x30` is `0xDBD2`, added to `0x6E20` (wrapping on overflow) is `0x49F2`)
    - `1PleaseDon'tInterpretThisAsANumber` -> `28404` (ordinal)
    - `3200-1600` -> `24848` (ordinal)
    - `3200-1600+1` -> `59919` (ordinal)
    - `(3200-1600)` -> `(3200-1600)` (string)
- If `CLASS` or `MENU` is specified first as an ordinal with in a resource, then any more `CLASS` or `MENU` optional statements will also be treated as an ordinal, e.g. `1 DIALOGEX 1,2,3,4 CLASS 1 CLASS "this would normally be a string" {}` will result in a resource with a class of `47959` as an ordinal.
- `CLASS` must be either a number, number expression, or a quoted string literal. If a quoted string is to be interpretted as a number due to the quirk in the previous point, then it is parsed first and then uses the same evaluation as `MENU` outlined above.

#### `FONT`

- If a `FONT` optional statement is present, `DS_SETFONT` (`0x40`) is implied
- `italic` setting of a `FONT` statement is a number evaluated as a `u16` and then interpretted as a boolean (so `65536` overflows to 0 which is interpretted as `FALSE`, and so is `65536L` [but `65537` overflows to 1 which is `TRUE`]).
- `charset` setting of a `FONT` statement is truncated to a `u8`.
- The comma between `point_size` and `typeface` parameters of `FONT` is optional (and can also be > 1), e.g. `FONT 1 "name"` is valid and so is `FONT 1,, ,, "name"`. The commas between the rest of the parameters are required and there can only be exactly 1 between those parameters.