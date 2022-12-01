---
title: Home
layout: home
---

Currently a dumping ground for various pieces of information related to `.rc` and `.res` files that I haven't found satisfying documentation for elsewhere. Might be organized into something more useful later.

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
| `MOVEABLE | PURE | DISCARDABLE` | `RT_GROUP_ICON`, `RT_GROUP_CURSOR`, `RT_STRING` |

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
- The `<id>` of each `FONT` must be unique.
- Each `FONT` is stored as normal with type `RT_FONT`. The entire binary contents of the sepcified file are the data. No validation/parsing is done of the data.

At the end of the .res, a single `RT_FONTDIR` resource with the name `FONTDIR` is written, with the data:

| Size/Type | Description |
|-----------|--------|
| `u16` | Number of total font resources |
| - | *Below is repeated for each `RT_FONT` in the `.res`* |
| `u16` | ID of the `RT_FONT` |
| 150 bytes | The first 150 bytes of the `FONT`'s file. If the file is smaller than 150 bytes, all missing bytes are filled with `0x00`. |

{: .note }
> There appears to be a bug in the MSVC++ `rc` tool where more than 150 bytes are written for a given `RT_FONT` (e.g. 153, and the last bytes starting at byte 149 are a repeat of the first bytes). Unsure what triggers it. So far the only reproduction is with a file that is 60 bytes large.
