---
title: Home
layout: home
---

Currently a dumping ground for various pieces of information related to `.rc` and `.res` files that I haven't found satisfying documentation for elsewhere. Might be organized into something more useful later.

## Common Resource Attributes / Memory Flags

### Flags

| Attribute | Value |
|-----------|-------|
| MOVEABLE | `0x10` |
| SHARED | `0x20` |
| PURE | `0x20` |
| PRELOAD | `0x40` |
| DISCARDABLE | `0x1000` |

### [`RT_`](https://learn.microsoft.com/en-us/windows/win32/menurc/resource-types) Default Flags

Note: Default values use the flags directly rather than 'applying' the relevant Common Resource Attribute, and therefore can have values that are not possible when specifying memory flags via Common Resource Attributes.

| Default | Types |
|----|---------|
| `MOVEABLE | DISCARDABLE` | `RT_ICON`, `RT_CURSOR` |
| `MOVEABLE | PURE` | `RT_RCDATA`, `RT_BITMAP` |
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
