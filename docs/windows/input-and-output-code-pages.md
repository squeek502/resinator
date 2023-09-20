---
layout: default
title: Input and output code pages
nav_order: 3
parent: Windows RC compiler documentation
---

# Input and output code pages

- It's possible to get the Win32 compiler to be in a weird state where it starts writing UTF-8 encoded bytes for ASCII string data. This seems to depend on the order of `#pragma code_page` directives in a file and/or the code page given on the CLI.
  + A Windows-1252 encoded file that has only `1 RCDATA { "Ó" }` in it will (with all default settings) be compiled into the single byte `0xD3` (`Ó` in Windows-1252). If the option `/c65001` is given to the CLI, it will instead be compiled into `0xEF 0xBF 0xBD`, the UTF-8 sequence for `U+FFFD` (the replacement character).
  + A Windows-1252 encoded file that has `1 RCDATA { "Ó" }` with `#pragma code_page(1252)` before it will be compiled into the single byte `0xD3` (`Ó` in Windows-1252). If the option `/c65001` is given to the CLI, it will instead be compiled into `0xC3 0x93`, the UTF-8 sequence for `Ó`.
    + The `/c65001` behavior can be 'counteracted' if there is any `#pragma code_page` before the `#pragma code_page(1252)` (it will compile into `0xD3` again).
  + A Windows-1252 encoded file that has `1 RCDATA { "Ó" }` with `#pragma code_page(65001)` before it will be compiled into the single byte `0x3F` (the `?` character). If the option `/c65001` is given to the CLI, it will instead be compiled into `0xEF 0xBF 0xBD`, the UTF-8 sequence for `U+FFFD` (the replacement character).
    + The `/c65001` behavior can be triggered without using the `/c` option if there is any `#pragma code_page` before the `#pragma code_page(65001)`
  + It seems like there are actually two distinct settings: input code page and output code page. The first `#pragma code_page` does not affect the output code page, but all the rest do. The `/c` CLI option affects both.