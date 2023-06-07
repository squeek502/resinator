---
layout: default
title: FONT
parent: Resources
---

# `FONT` resource

- The `<id>` in the `<id> FONT` definition **must** be an ordinal, not a string.
- The `<id>` of each `FONT` must be unique, but this does not fail the compilation, only emits an error. The first `FONT` defined with the `<id>` takes precedence, all others with the same `<id>` are ignored.
- Each `FONT` is stored with type `RT_FONT`. The entire binary contents of the specified file are the data. No validation/parsing is done of the data.

At the end of the .res, a single `RT_FONTDIR` resource with the name `FONTDIR` is written. The `FONTDIR` resource doesn't seem to matter at all; it can even be entirely omitted seemingly without any practical consequences.

This is the format that the Win32 RC compiler writes:

| Size/Type | Description |
|-----------|--------|
| `u16` | Number of total font resources |
| - | *Below is repeated for each `RT_FONT` in the `.res`* |
| `u16` | ID of the `RT_FONT` |
| 148 bytes | The first 148 bytes of the `FONT`'s file. Any missing bytes are padded with `0x00` |
| At least 1 byte | `NUL`-terminated string containing the 'device name'. The device name is the string located at the offset specified by the `u32` value at offset `140` of the file (e.g. if the `u32` at file offset `140` contains the value `500`, then the string is at offset `500` from the start of the file) |
| At least 1 byte | `NUL`-terminated string containing the 'face name'. The face name is the `NUL`-terminated string located at the offset specified by the `u32` value at the offset `144` of the file (e.g. if the `u32` at file offset `140` contains the value `500`, then the string is at offset `500` from the start of the file) |

What follows is a more complete explanation of the `FONTDIR` resource, including why the above format may be undesirable/wrong in some sense.

## What is `FONT` used for anyway?

The `FONT` resource has two possible use-cases that I am aware of.

### `.FNT` bitmap files

The original and primary use-case of the `FONT` resource is bundling `.fnt` bitmap font files into `.fon` files (which are resource-only DLLs renamed to have a `.fon` extension; they are recognized as fonts and can be installed like any other font).

Generating `.fon` files is somewhat obsolete, and most information online that I could find will direct you to third-party applications that will generate them for you, but in the 16-bit `rc.exe`, generating `.fon` files was something that was more directly supported.

From [TN318C.txt: How to create Windows .FON font libraries for use](https://community.embarcadero.com/article/technical-articles/149-tools/14454-how-to-create-windows-fon-font-libraries-for-use):

> To  complete  the  .FON  file,  you  must  append  the  font resources onto the font library.  This is done with: RC xxxxxxxx.rc xxxxxxxx.fon

This usage of `rc.exe` is only possible in the 16-bit version of `rc.exe`, where it would do the linking of the resources itself (and the `.res` file was an optional intermediate output). The modern equivalent would be [Creating a resource-only DLL](https://learn.microsoft.com/en-us/cpp/build/creating-a-resource-only-dll?view=msvc-170), where `rc.exe` is used to generate a `.res` which then gets linked with a DLL that has no entry point.

{: .note }
> Modern Windows versions do still *use* `.fon` files (e.g. quite a few `.fon` files are distributed with the default Windows 10 installation in `C:\Windows\Fonts`). For example, the `Terminal` font distributed with Windows 10 is a `.fon` file with a copyright year of 1991.

### Including custom fonts in a program

There is one [`Windows-classic-samples`](https://github.com/microsoft/Windows-classic-samples) example program that uses `FONT` resources with `.TTF` files to include custom fonts in a program: [`Win7Samples/multimedia/DirectWrite/CustomFont`](https://github.com/microsoft/Windows-classic-samples/tree/main/Samples/Win7Samples/multimedia/DirectWrite/CustomFont). This is meant to be an example of using [the DirectWrite APIs described here](https://learn.microsoft.com/en-us/windows/win32/directwrite/custom-font-collections), but it's very possible that this is a misuse of the `FONT` resource.

There is another example that does something similar using some older APIs ([`Samples/DirectWriteCustomFontSets`](https://github.com/microsoft/Windows-classic-samples/tree/main/Samples/DirectWriteCustomFontSets)) that uses user-defined resources for its font files. This seems like a better choice for reasons that will be detailed later.

## So what *should* go in the `FONTDIR`?

Practically, it doesn't seem to matter what's in the `FONTDIR`; it's likely not even read/parsed since it's possible to just iterate over/access the `FONT` resources directly instead. A `.res` without a `FONTDIR` is seemingly treated the same at link-time, and a resource-only DLL without a `FONTDIR` is seen the same by the Windows font system as one with a `FONTDIR`.

If we only look at the relevant documentation for what should go in a `FONTDIR`, it does not match up with what the Win32 RC compiler produces:

- [`FONTGROUPHDR`](https://learn.microsoft.com/en-us/windows/win32/menurc/fontgrouphdr)
- [`DIRENTRY`](https://learn.microsoft.com/en-us/windows/win32/menurc/direntry)
- [`FONTDIRENTRY`](https://learn.microsoft.com/en-us/windows/win32/menurc/fontdirentry)

According to the available documentation, the `FONTDIR`'s data should be a `FONTGROUPHDR` with the total number of fonts, followed by a `DIRENTRY` and a `FONTDIRENTRY` for each font. The `FONTDIRENTRY` is not an actual struct--all fields are contiguous with no padding--so each `FONTDIRENTRY` is at least `115` bytes: `113` for the statically sized fields, and then `szDeviceName` and `szFaceName` are both variable-length `NUL`-terminated strings (so at the very least they will have a `NUL` byte).

This would look something like this:

| Size/Type | Description |
|-----------|--------|
| `u16` | Number of total font resources |
| - | *Below is repeated for each `RT_FONT` in the `.res`* |
| `u16` | ID of the `RT_FONT` |
| 113 bytes | The first 113 bytes of the `FONT`'s file (aka all of the fields of [`FONTDIRENTRY`](https://learn.microsoft.com/en-us/windows/win32/menurc/fontdirentry) without `szDeviceName` and `szFaceName` and no padding between any of the fields) |
| At least 1 byte | `NUL`-terminated string containing the 'device name'. The device name is the string located at the offset dictated by the [`dfDevice` field of `FONTDIRENTRY`](https://learn.microsoft.com/en-us/windows/win32/menurc/fontdirentry) (101 bytes into the file) |
| At least 1 byte | `NUL`-terminated string containing the 'face name'. The face name is the string located at the offset dictated by the [`dfFace` field of `FONTDIRENTRY`](https://learn.microsoft.com/en-us/windows/win32/menurc/fontdirentry) (105 bytes into the file) |

If we run the 16-bit version of `rc.exe`, this is exactly what you see. However, in the 32-bit version of `rc.exe`, it will output at least `150` bytes for each `FONTDIRENTRY`. This difference has an explanation: the Windows 3.0 `.FNT` specification has the header size as `148`, so `rc.exe` is writing the full v3.0 header and then the `szDeviceName` and `szFaceName` as `NUL`-terminated strings. However, this seems incorrect for two reasons:

- There does not seem to be any updated `FONTDIRENTRY` docs that accommodate this new size, so anyone trying to enumerate the `FONTDIRENTRY`s in a `FONTDIR` will end up reading garbage after the first entry (since the offset to the next `FONTDIRENTRY` won't match the `FONTDIRENTRY` size as specified in the documentation)
- The `szDeviceName` and `szFaceName` are wrong. The Win32 compiler seems to be doing subtraction from end of the `.FNT` header to get the `dfDevice` and `dfFace` fields (which themselves contain offsets from the beginning of the file to where the associated `NUL`-terminated string is located). Since the 32-bit version is using a header size of 148, this means it is grabbing the fields from within a reserved section of the header instead of where the fields actually are (that is, instead of doing `113 - 12 = 101` for `dfDevice` it's doing `148 - 12 = 136` and reading from there instead). Subsequently, the 32-bit version of `rc.exe` basically never writes the `szDeviceName` or `szFaceName` to the `FONTDIRENTRY` since it's looking in the wrong place for the `dfDevice` and `dfFace` fields. This can be confirmed by modifying byte offset `136` and `140` to contain an offset of a `NUL-terminated` string in the file and seeing that `rc.exe` will use them as `szDeviceName`/`szFaceName`.

{: .note }
> The 148 byte size corresponds to the format specified [here](https://web.archive.org/web/20080115184921/http://support.microsoft.com/kb/65123) up to the end of `dfReserved1`. Those last 16 reserved bytes are the section in which the 32-bit `rc.exe` is erroneously interpreting as the `dfDevice`/`dfFace` fields.

{: .note }
> `rc.exe` actually seems to interpret the [incorrectly located] `dfFace` and `dfName` values as signed integers. If a value in either field that is interpreted as negative it will lead to a `fatal error RW1023: I/O error seeking in file` error.

## So really, what should go in the `FONTDIR`?

My conclusion is that this is almost the wrong question to be asking. Instead, the question becomes: *what will cause the least friction*? Let's look at some possibilities.

### Do the right thing

One possible idea would be to "do the right thing" with regards to the available documentation and what the `.FNT` format seems to suggest--write each `FONTDIRENTRY` as `113` bytes plus the real `szDeviceName`/`szFaceName`. However, this is complicated by the potential for non-`.FNT` font formats to be used in a `FONT` resource.

Treating these non-`.FNT` formats as a `.FNT` file and attempting to read `szDeviceName`/`szFaceName` from them is guaranteed to lead to bogus results. This is actually what the Win32 RC compiler does, though, and it means that it could error on otherwise valid `.TTF`/`.OTF`/etc font files (if the `dfDevice`/`dfFace` values are interpreted as negative; see the note above). This is the reason that using `FONT` for non-`.FNT` font files is likely a mistake (alluded to previously in the 'Including custom fonts in a program' section).

In any case, it becomes unclear exactly how non-`.FNT` files should be handled if the goal is to "do the right thing," since there's no available precedent besides the obviously wrong current Win32 RC compiler behavior.

### Emulate the Win32 behavior

Another possible idea would be to emulate the Win32 RC compiler behavior exactly, bug-for-bug. This has the downside of having to intentionally misinterpret all `FONT` files and treat the wrong offsets as `dfDevice`/`dfFace`, meaning that the `szDeviceName`/`szFaceName` written to `FONTDIRENTRY` are basically guaranteed to always be wrong and therefore useless.

{: .note }
> If the font file is 140 bytes or fewer, the Win32 RC compiler seems to default to a `dfFace` of `0` (as the [incorrect] location of the `dfFace` field is past the end of the file).
> - If the file is 75 bytes or smaller with no null bytes, the `FONTDIR` data for it will be 149 bytes (the first `n` being the bytes from the file, then the rest are `0x00` padding bytes). After that, there will be `n` bytes from the file again, and then a final `0x00`.
> - If the file is between 76 and 140 bytes long with no `0x00` bytes, the MSVC++ `rc` tool will crash.

### Semi-compatibility while avoiding the sharp edges

The other possible idea would be to do something similar enough to the Win32 compiler in the common case, but avoid emulating the buggy behavior where it makes sense. In this case, that would look like a `FONTDIRENTRY` with the following format:

- The first 148 bytes from the file verbatim, with no interpretation whatsoever, followed by two `NUL` bytes (corresponding to 'device name' and 'face name' both being zero length strings)

This would allow the `FONTDIR` to match byte-for-byte with the Win32 RC compiler in the common case (since very often the misinterpreted `dfDevice`/`dfFace` will be `0` or point somewhere outside the bounds of the file and therefore will be written as a zero-length string anyway), and only differ in the case where the Win32 RC compiler writes some bogus string(s) to the `szDeviceName`/`szFaceName`.

This also enables the use-case of non-`.FNT` files without any loose ends.

{: .note }
> Always writing `szDeviceName`/`szFaceName` as zero length is very likely to not cause any problems since the Win32 RC compiler already writes bogus data for those fields, so there's no real way that anything expects those `FONTDIR` fields to contain reliable information.

## What does `resinator` do?

`resinator` has gone with the "Semi-compatibility while avoiding the sharp edges" possibility.
