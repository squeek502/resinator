---
layout: default
title: DIALOG and DIALOGEX
parent: Resources
grand_parent: Windows RC compiler documentation
---

# `DIALOG` and `DIALOGEX` resources

- [The Old New Thing: `DIALOG`](https://devblogs.microsoft.com/oldnewthing/20040621-00/?p=38793)
- [The Old New Thing: `DIALOGEX`](https://devblogs.microsoft.com/oldnewthing/20040623-00/?p=38753)

## `DIALOGEX`

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

### `CLASS` and `MENU` quirks

- `MENU` is a proper `NameOrOrdinal`, in that it can be unquoted, contain " within it, etc.
  + However, it is parsed differently than a `NameOrOrdinal` used for resource id/types. Whenever the first character is a number, it is treated as a number. Anything after that is treated as a digit with the value `<UTF-16 code unit> - '0'` (where `'0'` is `0x30`, and using wrapping underflow for ascii values < 0x30). Some examples:
    - `3200` -> `3200` (ordinal)
    - `1+1` -> `51` (ordinal)
    - `1|2` -> `862` (ordinal)
    - `1a2` -> `592` (ordinal)
    - `1a` -> `59` (ordinal)
    - `1A` -> `27` (ordinal)
    - `1!` -> `65531` (ordinal)
    - `0Œ` -> `0x122` (ordinal, `Œ` is `U+0152`, so `- 0x30` is `0x122`)
    - `0<U+10002>` -> `0x49F2` (ordinal, `<U+10002>` is `0xD800 0xDC02` in UTF-16, so `0xD800 - 0x30` is `0xD7D0`, multiplied by 10 (with wrapping overflow) that is `0x6E20`, then `0xDC02 - 0x30` is `0xDBD2`, added to `0x6E20` (wrapping on overflow) is `0x49F2`)
    - `1PleaseDon'tInterpretThisAsANumber` -> `28404` (ordinal)
    - `3200-1600` -> `24848` (ordinal)
    - `3200-1600+1` -> `59919` (ordinal)
    - `(3200-1600)` -> `(3200-1600)` (string)
- If `CLASS` or `MENU` is specified first as an ordinal with in a resource, then any more `CLASS` or `MENU` optional statements will also be treated as an ordinal, e.g. `1 DIALOGEX 1,2,3,4 CLASS 1 CLASS "this would normally be a string" {}` will result in a resource with a class of `47959` as an ordinal.
- `CLASS` must be either a number, number expression, or a quoted string literal. If a quoted string is to be interpretted as a number due to the quirk in the previous point, then it is parsed first and then uses the same evaluation as `MENU` outlined above.

### `FONT`

- If a `FONT` optional statement is present, `DS_SETFONT` (`0x40`) is implied
- `italic` setting of a `FONT` statement is a number evaluated as a `u16` and then interpretted as a boolean (so `65536` overflows to 0 which is interpretted as `FALSE`, and so is `65536L` [but `65537` overflows to 1 which is `TRUE`]).
- `charset` setting of a `FONT` statement is truncated to a `u8`.
- The comma between `point_size` and `typeface` parameters of `FONT` is optional (and can also be > 1), e.g. `FONT 1 "name"` is valid and so is `FONT 1,, ,, "name"`. The commas between the rest of the parameters are required and there can only be exactly 1 between those parameters.