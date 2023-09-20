---
layout: default
title: ACCELERATORS
parent: Resources
grand_parent: Windows RC compiler documentation
---

# `ACCELERATORS` resource

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
