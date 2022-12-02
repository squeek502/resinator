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
| `MOVEABLE | PURE` | `RT_RCDATA`, `RT_BITMAP`, `RT_HTML` |
| `MOVEABLE | PURE | DISCARDABLE` | `RT_GROUP_ICON`, `RT_GROUP_CURSOR`, `RT_STRING`, `RT_FONT` |
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
| `DISCARDABLE` | `flags | (DISCARDABLE | MOVEABLE | PURE)` |

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