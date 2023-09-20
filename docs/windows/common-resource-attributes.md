---
layout: default
title: Common resource attributes
nav_order: 2
parent: Windows RC compiler documentation
---

# Common Resource Attributes / Memory Flags

## Flags

| Attribute | Value |
|-----------|-------|
| `MOVEABLE` | `0x10` |
| `SHARED` | `0x20` |
| `PURE` | `0x20` |
| `PRELOAD` | `0x40` |
| `DISCARDABLE` | `0x1000` |

## [`RT_`](https://learn.microsoft.com/en-us/windows/win32/menurc/resource-types) Default Flags

{: .note }
> Default values use the flags directly rather than 'applying' the relevant Common Resource Attribute(s), and therefore can have values that are not possible when specifying memory flags via Common Resource Attributes.

| Default | Types |
|----|---------|
| `MOVEABLE | DISCARDABLE` | `RT_ICON`, `RT_CURSOR` |
| `MOVEABLE | SHARED` | `RT_RCDATA`, `RT_BITMAP`, `RT_HTML`, `RT_ACCELERATOR` |
| `MOVEABLE | SHARED | DISCARDABLE` | `RT_GROUP_ICON`, `RT_GROUP_CURSOR`, `RT_STRING`, `RT_FONT`, `RT_DIALOG` |
| `MOVEABLE | PRELOAD` | `RT_FONTDIR` |

## Common Resource Attribute effects

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
