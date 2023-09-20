---
layout: default
title: BITMAP
parent: Resources
grand_parent: Windows RC compiler documentation
---

# `BITMAP` resource

- Win32 RC compiler allows any DIB with header size >= 40 or exactly 12 (Win 2.0). DIBs with unknown or `BITMAPV4HEADER`/`BITMAPV5HEADER` headers will fail to be loaded at runtime by LoadBitmap/LoadImage, but they are not a compile error.
- Does not support PNG at all. Side note: If a `.png` file is specified in a `.xml` file run through `uicc.exe`, it will generate a `.rc` file with e.g. `<id> IMAGE "image.png"` which is a custom resource with the type as the string `IMAGE`, while for `.bmp` files it will use `BITMAP`.
- Win32 `rc.exe` will always try to write `numColorPaletteBytes` (`<number of bytes per color palette index> * numColors`) bytes to the `.res`, filling in any bytes past the end of the file with the pixel data (this is a miscompilation I believe), and then `0x00` if it starts reading past the end of the file (note: `numColors` is `biClrUsed` from the `BITMAPINFOHEADER`).
  + If `numColorPaletteBytes` is >= 2 GiB, then `rc.exe` will fail with `out of memory`, e.g. if `numColors` is `0x262C8ACF`, then `rc.exe` will attempt to allocate `0x98B22B3C` bytes (2.386 GiB) and fail.
    - Certain values where `numColorPaletteBytes` is >= 2 GiB seem avoid the `out of memory` and have extremely long runtime (it seems like an infinite loop but will eventually finish), but I'm not sure exactly what the conditions are that avoid the `out of memory`.
  + If `numColorPaletteBytes` is < 2 GiB, then `rc.exe` will succeed (but may take a while)
- Win32 `rc.exe` will miscompile bitmaps where the `bfOffBits` field of the bitmap file header is set larger than `<the size of the bmp file> + <size of the DIB header>`, in which case the 'size' of the resource will become negative and a resource with the size set to the underflowed size (e.g. `-1` will underflow/wrap to `0xFFFFFFFF`) but with no actual data.
