---
layout: default
title: Command line interface
nav_order: 1
parent: Windows RC compiler documentation
---

# CLI

- Multiple options can be specified at a time, e.g. `/vnl409` gets resolved as if it were `/v /n /l409`.
- The `/sl` option ('Specify the resource string length limit in percentage') must be between 1 and 100 (if specified). If it's set to 100, the maximum length of a string literal becomes 8192.
  + The `rc` tool will crash if `/sl 100` is set and there is a string literal with exactly 8193 characters in it. If one more character is added to the string literal, it errors with 'string literal too long'.
  + The default maximum string literal length (if `/sl` is not specified) is 4097. If `/sl 50` is specified, the maximum string literal length becomes 4096 rather than 4097. So, there's no `/sl` setting that's equivalent to the default string literal length limit.
  + If `/sl 33` is set, the maximum string literal length becomes 2703 (`8192 * 0.33 = 2,703.36`). 2704 chars will error with `string literal too long`.
  + If `/sl 15` is set, the maximum string literal length becomes 1228 (`8192 * 0.15 = 1,228.8`). 1229 chars will error with `string literal too long`.

## Undocumented options

- `/a` seems to be a recognized option but it's unclear what it does and is totally undocumented AFAICT
- Either one of `/?c` or `/hc` will add a normally hidden 'Comments extracting switches:' section to the help menu, with `/t` and `/t`-prefixed options dealing with `.LCX` and `.LCE` files. Can find no info about any of this online. A generated `.LCE` file seems to be an XML file with some info about the comments and resources in the `.rc` file(s).
- `/p` will output the preprocessed version of the `.rc` file to `<filename>.rcpp` *instead of* outputting a `.res` file (i.e. it will only run the preprocessor).
  + There doesn't appear to be any way to control the name of the `.rcpp` file (`/fo` does not affect it)
- `/s <unknown>` will insert a bunch of resources with name `HWB` into the `.res`. I can't find any info on this except a note [on this page](https://learn.microsoft.com/en-us/cpp/windows/how-to-create-a-resource-script-file?view=msvc-170) saying that `HWB` is a resource name that is reserved by Visual Studio. The option seems to need a value but the value doesn't seem to have any affect on the `.res` contents and it seems to accept any value without complaint.
- `/z` seems to always error with `fatal error RC1212: invalid option - /z argument missing substitute font name`. Not sure what type of value it's looking for, or what it would affect if it were provided a valid value.
  + A value with `/` in it seems to get past the `argument missing substitute font name` error and will allow `rc.exe` to compile successfully.
