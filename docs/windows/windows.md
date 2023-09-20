---
layout: default
title: Windows RC compiler documentation
nav_order: 3
has_children: true
permalink: /windows
---

# Windows RC compiler documentation

## Random quirks of the MSVC++ `rc` tool

- Resource definitions that specify a filename can specify files in the include path of the `rc` compiler, e.g. `windows.h` and they will be found/used.
