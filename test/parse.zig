const std = @import("std");
const resinator = @import("resinator");

test "basic icons" {
    try testParse("id ICON MOVEABLE filename.ico",
        \\root
        \\ resource_external id ICON [1 common_resource_attributes]
        \\  literal filename.ico
        \\
    );
    try testParse(
        \\id1 ICON MOVEABLE filename.ico
        \\id2 ICON filename.ico
    ,
        \\root
        \\ resource_external id1 ICON [1 common_resource_attributes]
        \\  literal filename.ico
        \\ resource_external id2 ICON [0 common_resource_attributes]
        \\  literal filename.ico
        \\
    );
    try testParse(
        \\id1 ICON MOVEABLE filename.ico id2 ICON filename.ico
    ,
        \\root
        \\ resource_external id1 ICON [1 common_resource_attributes]
        \\  literal filename.ico
        \\ resource_external id2 ICON [0 common_resource_attributes]
        \\  literal filename.ico
        \\
    );
    try testParse(
        \\"id1" ICON "filename.ico"
        \\L"id2" ICON L"filename.ico"
    ,
        \\root
        \\ resource_external "id1" ICON [0 common_resource_attributes]
        \\  literal "filename.ico"
        \\ resource_external L"id2" ICON [0 common_resource_attributes]
        \\  literal L"filename.ico"
        \\
    );
}

test "user-defined" {
    try testParse("id \"quoted\" file.bin",
        \\root
        \\ resource_external id "quoted" [0 common_resource_attributes]
        \\  literal file.bin
        \\
    );
}

test "raw data" {
    try testParse("id RCDATA {}",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 0
        \\
    );
    try testParse("id RCDATA { 1,2,3 }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 3
        \\  literal 1
        \\  literal 2
        \\  literal 3
        \\
    );
    try testParse("id RCDATA { L\"1\",\"2\",3 }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 3
        \\  literal L"1"
        \\  literal "2"
        \\  literal 3
        \\
    );
    try testParse("id RCDATA { 1\t,,  ,,,2,,  ,  3 ,,,  , }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 3
        \\  literal 1
        \\  literal 2
        \\  literal 3
        \\
    );
    try testParse("id RCDATA { 1 2 3 }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 3
        \\  literal 1
        \\  literal 2
        \\  literal 3
        \\
    );
}

test "number expressions" {
    try testParse("id RCDATA { 1-- }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 1
        \\  binary_expression -
        \\   literal 1
        \\   literal -
        \\
    );
    try testParse("id RCDATA { (1) }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 1
        \\  grouped_expression
        \\  (
        \\   literal 1
        \\  )
        \\
    );
    try testParse("id RCDATA { (1+-1) }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 1
        \\  grouped_expression
        \\  (
        \\   binary_expression +
        \\    literal 1
        \\    literal -1
        \\  )
        \\
    );
    // All operators have the same precedence, the result should be from left-to-right.
    // In C, this would evaluate as `7 | (7 + 1)`, but here it evaluates as `(7 | 7) + 1`.
    try testParse("id RCDATA { 7 | 7 + 1 }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 1
        \\  binary_expression +
        \\   binary_expression |
        \\    literal 7
        \\    literal 7
        \\   literal 1
        \\
    );
    // This looks like an invalid number expression, but it's interpreted as three separate data elements:
    // "str", - (evaluates to 0), and 1
    try testParse("id RCDATA { \"str\" - 1 }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 3
        \\  literal "str"
        \\  literal -
        \\  literal 1
        \\
    );
    // But this is an actual error since it tries to use a string as part of a number expression
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected number, number expression, or quoted string literal; got '&'" }},
        "1 RCDATA { \"str\" & 1 }",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected number or number expression; got '\"str\"'" }},
        "1 RCDATA { (\"str\") }",
        null,
    );
}

test "STRINGTABLE" {
    try testParse("STRINGTABLE { 0 \"hello\" }",
        \\root
        \\ string_table STRINGTABLE [0 common_resource_attributes]
        \\ {
        \\  string_table_string
        \\   literal 0
        \\   "hello"
        \\ }
        \\
    );
    try testParse("STRINGTABLE { 0, \"hello\" }",
        \\root
        \\ string_table STRINGTABLE [0 common_resource_attributes]
        \\ {
        \\  string_table_string
        \\   literal 0
        \\   "hello"
        \\ }
        \\
    );
    try testParse(
        \\STRINGTABLE {
        \\  (0+1), "hello"
        \\  -1, L"hello"
        \\}
    ,
        \\root
        \\ string_table STRINGTABLE [0 common_resource_attributes]
        \\ {
        \\  string_table_string
        \\   grouped_expression
        \\   (
        \\    binary_expression +
        \\     literal 0
        \\     literal 1
        \\   )
        \\   "hello"
        \\  string_table_string
        \\   literal -1
        \\   L"hello"
        \\ }
        \\
    );

    try testParse("STRINGTABLE FIXED { 0 \"hello\" }",
        \\root
        \\ string_table STRINGTABLE [1 common_resource_attributes]
        \\ {
        \\  string_table_string
        \\   literal 0
        \\   "hello"
        \\ }
        \\
    );

    try testParse("STRINGTABLE { 1+1 \"hello\" }",
        \\root
        \\ string_table STRINGTABLE [0 common_resource_attributes]
        \\ {
        \\  string_table_string
        \\   binary_expression +
        \\    literal 1
        \\    literal 1
        \\   "hello"
        \\ }
        \\
    );

    // duplicate optional statements are preserved in the AST
    try testParse("STRINGTABLE LANGUAGE 1,1 LANGUAGE 1,2 { 0 \"hello\" }",
        \\root
        \\ string_table STRINGTABLE [0 common_resource_attributes]
        \\  language_statement LANGUAGE
        \\   literal 1
        \\   literal 1
        \\  language_statement LANGUAGE
        \\   literal 1
        \\   literal 2
        \\ {
        \\  string_table_string
        \\   literal 0
        \\   "hello"
        \\ }
        \\
    );

    try testParse("STRINGTABLE FIXED VERSION 1 CHARACTERISTICS (1+2) { 0 \"hello\" }",
        \\root
        \\ string_table STRINGTABLE [1 common_resource_attributes]
        \\  simple_statement VERSION
        \\   literal 1
        \\  simple_statement CHARACTERISTICS
        \\   grouped_expression
        \\   (
        \\    binary_expression +
        \\     literal 1
        \\     literal 2
        \\   )
        \\ {
        \\  string_table_string
        \\   literal 0
        \\   "hello"
        \\ }
        \\
    );
}

test "control characters as whitespace" {
    // any non-illegal control character is treated as whitespace
    try testParse("id RCDATA { 1\x052 }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 2
        \\  literal 1
        \\  literal 2
        \\
    );
    // some illegal control characters are legal inside of string literals
    try testParse("id RCDATA { \"\x01\" }",
        \\root
        \\ resource_raw_data id RCDATA [0 common_resource_attributes] raw data: 1
        \\
    ++ "  literal \"\x01\"\n"); // needed to get the actual byte \x01 in the expected output
}

test "top-level statements" {
    try testParse("LANGUAGE 0, 0",
        \\root
        \\ language_statement LANGUAGE
        \\  literal 0
        \\  literal 0
        \\
    );
    try testParse("VERSION 0",
        \\root
        \\ simple_statement VERSION
        \\  literal 0
        \\
    );
    try testParse("CHARACTERISTICS 0",
        \\root
        \\ simple_statement CHARACTERISTICS
        \\  literal 0
        \\
    );
    // dangling tokens should be an error if they are a the start of a valid top-level statement
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected number or number expression; got '<eof>'" }},
        "LANGUAGE",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected number or number expression; got '<eof>'" }},
        "VERSION",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected number or number expression; got '<eof>'" }},
        "CHARACTERISTICS",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected '<'{' or BEGIN>', got '<eof>'" }},
        "STRINGTABLE",
        null,
    );
}

test "accelerators" {
    try testParse("1 ACCELERATORS FIXED VERSION 1 {}",
        \\root
        \\ accelerators 1 ACCELERATORS [1 common_resource_attributes]
        \\  simple_statement VERSION
        \\   literal 1
        \\ {
        \\ }
        \\
    );
    try testParse("1 ACCELERATORS { \"^C\", 1 L\"a\", 2 }",
        \\root
        \\ accelerators 1 ACCELERATORS [0 common_resource_attributes]
        \\ {
        \\  accelerator
        \\   literal "^C"
        \\   literal 1
        \\  accelerator
        \\   literal L"a"
        \\   literal 2
        \\ }
        \\
    );
    try testParse("1 ACCELERATORS { (1+1), -1+1, CONTROL, ASCII, VIRTKEY, ALT, SHIFT }",
        \\root
        \\ accelerators 1 ACCELERATORS [0 common_resource_attributes]
        \\ {
        \\  accelerator CONTROL, ASCII, VIRTKEY, ALT, SHIFT
        \\   grouped_expression
        \\   (
        \\    binary_expression +
        \\     literal 1
        \\     literal 1
        \\   )
        \\   binary_expression +
        \\    literal -1
        \\    literal 1
        \\ }
        \\
    );
}

test "dialogs" {
    try testParse("1 DIALOG FIXED 1, 2, 3, (3 - 1) LANGUAGE 1, 2 {}",
        \\root
        \\ dialog 1 DIALOG [1 common_resource_attributes]
        \\  x:
        \\   literal 1
        \\  y:
        \\   literal 2
        \\  width:
        \\   literal 3
        \\  height:
        \\   grouped_expression
        \\   (
        \\    binary_expression -
        \\     literal 3
        \\     literal 1
        \\   )
        \\  language_statement LANGUAGE
        \\   literal 1
        \\   literal 2
        \\ {
        \\ }
        \\
    );
    try testParse("1 DIALOGEX 1, 2, 3, 4, 5 {}",
        \\root
        \\ dialog 1 DIALOGEX [0 common_resource_attributes]
        \\  x:
        \\   literal 1
        \\  y:
        \\   literal 2
        \\  width:
        \\   literal 3
        \\  height:
        \\   literal 4
        \\  help_id:
        \\   literal 5
        \\ {
        \\ }
        \\
    );
    try testParse("1 DIALOG 1, 2, 3, 4 FONT 1 \"hello\" {}",
        \\root
        \\ dialog 1 DIALOG [0 common_resource_attributes]
        \\  x:
        \\   literal 1
        \\  y:
        \\   literal 2
        \\  width:
        \\   literal 3
        \\  height:
        \\   literal 4
        \\  font_statement FONT typeface: "hello"
        \\   point_size:
        \\    literal 1
        \\ {
        \\ }
        \\
    );
    // FONT allows empty values for weight and charset in DIALOGEX
    try testParse("1 DIALOGEX 1, 2, 3, 4 FONT 1,,, \"hello\", , 1, {}",
        \\root
        \\ dialog 1 DIALOGEX [0 common_resource_attributes]
        \\  x:
        \\   literal 1
        \\  y:
        \\   literal 2
        \\  width:
        \\   literal 3
        \\  height:
        \\   literal 4
        \\  font_statement FONT typeface: "hello"
        \\   point_size:
        \\    literal 1
        \\   italic:
        \\    literal 1
        \\ {
        \\ }
        \\
    );
    // but italic cannot be empty
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected number or number expression; got ','" }},
        "1 DIALOGEX 1, 2, 3, 4 FONT 1,,, \"hello\", 1, , 1 {}",
        null,
    );
    try testParse(
        \\1 DIALOGEX FIXED DISCARDABLE 1, 2, 3, 4
        \\STYLE 0x80000000L | 0x00800000L
        \\CAPTION "Error!"
        \\EXSTYLE 1
        \\CLASS "hello1"
        \\CLASS 2
        \\MENU 2+"4"
        \\MENU "1"
        \\FONT 12 "first", 1001-1, 65537L, 257-2
        \\FONT 8+2,, ,, "second", 0
        \\{}
    ,
        \\root
        \\ dialog 1 DIALOGEX [2 common_resource_attributes]
        \\  x:
        \\   literal 1
        \\  y:
        \\   literal 2
        \\  width:
        \\   literal 3
        \\  height:
        \\   literal 4
        \\  simple_statement STYLE
        \\   binary_expression |
        \\    literal 0x80000000L
        \\    literal 0x00800000L
        \\  simple_statement CAPTION
        \\   literal "Error!"
        \\  simple_statement EXSTYLE
        \\   literal 1
        \\  simple_statement CLASS
        \\   literal "hello1"
        \\  simple_statement CLASS
        \\   literal 2
        \\  simple_statement MENU
        \\   literal 2+"4"
        \\  simple_statement MENU
        \\   literal "1"
        \\  font_statement FONT typeface: "first"
        \\   point_size:
        \\    literal 12
        \\   weight:
        \\    binary_expression -
        \\     literal 1001
        \\     literal 1
        \\   italic:
        \\    literal 65537L
        \\   char_set:
        \\    binary_expression -
        \\     literal 257
        \\     literal 2
        \\  font_statement FONT typeface: "second"
        \\   point_size:
        \\    binary_expression +
        \\     literal 8
        \\     literal 2
        \\   weight:
        \\    literal 0
        \\ {
        \\ }
        \\
    );
}

test "dialog controls" {
    try testParse(
        \\1 DIALOGEX 1, 2, 3, 4
        \\{
        \\    AUTO3STATE,, "mytext",, 900,, 1 2 3 4, 0, 0, 100 { "AUTO3STATE" }
        \\    AUTOCHECKBOX "mytext", 901, 1, 2, 3, 4, 0, 0, 100 { "AUTOCHECKBOX" }
        \\    AUTORADIOBUTTON "mytext", 902, 1, 2, 3, 4, 0, 0, 100 { "AUTORADIOBUTTON" }
        \\    CHECKBOX "mytext", 903, 1, 2, 3, 4, 0, 0, 100 { "CHECKBOX" }
        \\    COMBOBOX 904,, 1 2 3 4, 0, 0, 100 { "COMBOBOX" }
        \\    CONTROL "mytext",, 905,, "\x42UTTON",, 1,, 2 3 4 0, 0, 100 { "CONTROL (BUTTON)" }
        \\    CONTROL 1,, 9051,, (0x80+1),, 1,, 2 3 4 0, 0, 100 { "CONTROL (0x80)" }
        \\    CONTROL 1,, 9052,, (0x80+1),, 1,, 2 3 4 0 { "CONTROL (0x80)" }
        \\    CTEXT "mytext", 906, 1, 2, 3, 4, 0, 0, 100 { "CTEXT" }
        \\    CTEXT "mytext", 9061, 1, 2, 3, 4 { "CTEXT" }
        \\    DEFPUSHBUTTON "mytext", 907, 1, 2, 3, 4, 0, 0, 100 { "DEFPUSHBUTTON" }
        \\    EDITTEXT 908, 1, 2, 3, 4, 0, 0, 100 { "EDITTEXT" }
        \\    HEDIT 9081, 1, 2, 3, 4, 0, 0, 100 { "HEDIT" }
        \\    IEDIT 9082, 1, 2, 3, 4, 0, 0, 100 { "IEDIT" }
        \\    GROUPBOX "mytext", 909, 1, 2, 3, 4, 0, 0, 100 { "GROUPBOX" }
        \\    ICON "mytext", 910, 1, 2, 3, 4, 0, 0, 100 { "ICON" }
        \\    LISTBOX 911, 1, 2, 3, 4, 0, 0, 100 { "LISTBOX" }
        \\    LTEXT "mytext", 912, 1, 2, 3, 4, 0, 0, 100 { "LTEXT" }
        \\    PUSHBOX "mytext", 913, 1, 2, 3, 4, 0, 0, 100 { "PUSHBOX" }
        \\    PUSHBUTTON "mytext", 914, 1, 2, 3, 4, 0, 0, 100 { "PUSHBUTTON" }
        \\    RADIOBUTTON "mytext", 915, 1, 2, 3, 4, 0, 0, 100 { "RADIOBUTTON" }
        \\    RTEXT "mytext", 916, 1, 2, 3, 4, 0, 0, 100 { "RTEXT" }
        \\    SCROLLBAR 917, 1, 2, 3, 4, 0, 0, 100 { "SCROLLBAR" }
        \\    STATE3 "mytext", 918, 1, 2, 3, 4, 0, 0, 100 { "STATE3" }
        \\    USERBUTTON "mytext", 919, 1, 2, 3, 4, 0, 0, 100 { "USERBUTTON" }
        \\}
    ,
        \\root
        \\ dialog 1 DIALOGEX [0 common_resource_attributes]
        \\  x:
        \\   literal 1
        \\  y:
        \\   literal 2
        \\  width:
        \\   literal 3
        \\  height:
        \\   literal 4
        \\ {
        \\  control_statement AUTO3STATE text: "mytext"
        \\   id:
        \\    literal 900
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "AUTO3STATE"
        \\  }
        \\  control_statement AUTOCHECKBOX text: "mytext"
        \\   id:
        \\    literal 901
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "AUTOCHECKBOX"
        \\  }
        \\  control_statement AUTORADIOBUTTON text: "mytext"
        \\   id:
        \\    literal 902
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "AUTORADIOBUTTON"
        \\  }
        \\  control_statement CHECKBOX text: "mytext"
        \\   id:
        \\    literal 903
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "CHECKBOX"
        \\  }
        \\  control_statement COMBOBOX
        \\   id:
        \\    literal 904
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "COMBOBOX"
        \\  }
        \\  control_statement CONTROL text: "mytext"
        \\   class:
        \\    literal "\x42UTTON"
        \\   id:
        \\    literal 905
        \\   x:
        \\    literal 2
        \\   y:
        \\    literal 3
        \\   width:
        \\    literal 4
        \\   height:
        \\    literal 0
        \\   style:
        \\    literal 1
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "CONTROL (BUTTON)"
        \\  }
        \\  control_statement CONTROL text: 1
        \\   class:
        \\    grouped_expression
        \\    (
        \\     binary_expression +
        \\      literal 0x80
        \\      literal 1
        \\    )
        \\   id:
        \\    literal 9051
        \\   x:
        \\    literal 2
        \\   y:
        \\    literal 3
        \\   width:
        \\    literal 4
        \\   height:
        \\    literal 0
        \\   style:
        \\    literal 1
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "CONTROL (0x80)"
        \\  }
        \\  control_statement CONTROL text: 1
        \\   class:
        \\    grouped_expression
        \\    (
        \\     binary_expression +
        \\      literal 0x80
        \\      literal 1
        \\    )
        \\   id:
        \\    literal 9052
        \\   x:
        \\    literal 2
        \\   y:
        \\    literal 3
        \\   width:
        \\    literal 4
        \\   height:
        \\    literal 0
        \\   style:
        \\    literal 1
        \\  {
        \\   literal "CONTROL (0x80)"
        \\  }
        \\  control_statement CTEXT text: "mytext"
        \\   id:
        \\    literal 906
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "CTEXT"
        \\  }
        \\  control_statement CTEXT text: "mytext"
        \\   id:
        \\    literal 9061
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\  {
        \\   literal "CTEXT"
        \\  }
        \\  control_statement DEFPUSHBUTTON text: "mytext"
        \\   id:
        \\    literal 907
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "DEFPUSHBUTTON"
        \\  }
        \\  control_statement EDITTEXT
        \\   id:
        \\    literal 908
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "EDITTEXT"
        \\  }
        \\  control_statement HEDIT
        \\   id:
        \\    literal 9081
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "HEDIT"
        \\  }
        \\  control_statement IEDIT
        \\   id:
        \\    literal 9082
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "IEDIT"
        \\  }
        \\  control_statement GROUPBOX text: "mytext"
        \\   id:
        \\    literal 909
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "GROUPBOX"
        \\  }
        \\  control_statement ICON text: "mytext"
        \\   id:
        \\    literal 910
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "ICON"
        \\  }
        \\  control_statement LISTBOX
        \\   id:
        \\    literal 911
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "LISTBOX"
        \\  }
        \\  control_statement LTEXT text: "mytext"
        \\   id:
        \\    literal 912
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "LTEXT"
        \\  }
        \\  control_statement PUSHBOX text: "mytext"
        \\   id:
        \\    literal 913
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "PUSHBOX"
        \\  }
        \\  control_statement PUSHBUTTON text: "mytext"
        \\   id:
        \\    literal 914
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "PUSHBUTTON"
        \\  }
        \\  control_statement RADIOBUTTON text: "mytext"
        \\   id:
        \\    literal 915
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "RADIOBUTTON"
        \\  }
        \\  control_statement RTEXT text: "mytext"
        \\   id:
        \\    literal 916
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "RTEXT"
        \\  }
        \\  control_statement SCROLLBAR
        \\   id:
        \\    literal 917
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "SCROLLBAR"
        \\  }
        \\  control_statement STATE3 text: "mytext"
        \\   id:
        \\    literal 918
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "STATE3"
        \\  }
        \\  control_statement USERBUTTON text: "mytext"
        \\   id:
        \\    literal 919
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 0
        \\   exstyle:
        \\    literal 0
        \\   help_id:
        \\    literal 100
        \\  {
        \\   literal "USERBUTTON"
        \\  }
        \\ }
        \\
    );

    // help_id param is not supported if the resource is DIALOG
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected '<'}' or END>', got ','" }},
        \\1 DIALOG 1, 2, 3, 4
        \\{
        \\    AUTO3STATE,, "mytext",, 900,, 1 2 3 4, 0, 0, 100 { "AUTO3STATE" }
        \\}
    ,
        null,
    );

    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected control class [BUTTON, EDIT, etc]; got 'SOMETHING'" }},
        \\1 DIALOG 1, 2, 3, 4
        \\{
        \\    CONTROL "", 900, SOMETHING, 0, 1, 2, 3, 4
        \\}
    ,
        null,
    );
}

test "optional parameters" {
    // Optional values (like style, exstyle, helpid) can be empty
    try testParse(
        \\1 DIALOGEX 1, 2, 3, 4,
        \\{
        \\    AUTO3STATE, "text", 900,, 1 2 3 4
        \\    AUTO3STATE, "text", 901,, 1 2 3 4,
        \\    AUTO3STATE, "text", 902,, 1 2 3 4, 1
        \\    AUTO3STATE, "text", 903,, 1 2 3 4, 1,
        \\    AUTO3STATE, "text", 904,, 1 2 3 4,  ,
        \\    AUTO3STATE, "text", 905,, 1 2 3 4, 1, 2
        \\    AUTO3STATE, "text", 906,, 1 2 3 4,  , 2
        \\    AUTO3STATE, "text", 907,, 1 2 3 4, 1, 2,
        \\    AUTO3STATE, "text", 908,, 1 2 3 4, 1,  ,
        \\    AUTO3STATE, "text", 909,, 1 2 3 4,  ,  ,
        \\    AUTO3STATE, "text", 910,, 1 2 3 4, 1, 2, 3
        \\    AUTO3STATE, "text", 911,, 1 2 3 4,  , 2, 3
        \\    AUTO3STATE, "text", 912,, 1 2 3 4,  ,  , 3
        \\    AUTO3STATE, "text", 913,, 1 2 3 4,  ,  ,
        \\}
    ,
        \\root
        \\ dialog 1 DIALOGEX [0 common_resource_attributes]
        \\  x:
        \\   literal 1
        \\  y:
        \\   literal 2
        \\  width:
        \\   literal 3
        \\  height:
        \\   literal 4
        \\ {
        \\  control_statement AUTO3STATE text: "text"
        \\   id:
        \\    literal 900
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\  control_statement AUTO3STATE text: "text"
        \\   id:
        \\    literal 901
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\  control_statement AUTO3STATE text: "text"
        \\   id:
        \\    literal 902
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 1
        \\  control_statement AUTO3STATE text: "text"
        \\   id:
        \\    literal 903
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 1
        \\  control_statement AUTO3STATE text: "text"
        \\   id:
        \\    literal 904
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\  control_statement AUTO3STATE text: "text"
        \\   id:
        \\    literal 905
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 1
        \\   exstyle:
        \\    literal 2
        \\  control_statement AUTO3STATE text: "text"
        \\   id:
        \\    literal 906
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   exstyle:
        \\    literal 2
        \\  control_statement AUTO3STATE text: "text"
        \\   id:
        \\    literal 907
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 1
        \\   exstyle:
        \\    literal 2
        \\  control_statement AUTO3STATE text: "text"
        \\   id:
        \\    literal 908
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 1
        \\  control_statement AUTO3STATE text: "text"
        \\   id:
        \\    literal 909
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\  control_statement AUTO3STATE text: "text"
        \\   id:
        \\    literal 910
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   style:
        \\    literal 1
        \\   exstyle:
        \\    literal 2
        \\   help_id:
        \\    literal 3
        \\  control_statement AUTO3STATE text: "text"
        \\   id:
        \\    literal 911
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   exstyle:
        \\    literal 2
        \\   help_id:
        \\    literal 3
        \\  control_statement AUTO3STATE text: "text"
        \\   id:
        \\    literal 912
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\   help_id:
        \\    literal 3
        \\  control_statement AUTO3STATE text: "text"
        \\   id:
        \\    literal 913
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 2
        \\   width:
        \\    literal 3
        \\   height:
        \\    literal 4
        \\ }
        \\
    );

    // Trailing comma after help_id is not allowed
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected '<'}' or END>', got ','" }},
        \\1 DIALOGEX 1, 2, 3, 4
        \\{
        \\    AUTO3STATE,, "mytext",, 900,, 1 2 3 4, , , ,
        \\}
    ,
        null,
    );
}

test "not expressions" {
    try testParse(
        \\1 DIALOGEX 1, 2, 3, 4
        \\{
        \\  AUTOCHECKBOX "", 0, 0, 0, 0, 0, NOT 1, NOT 2, 100
        \\  CONTROL "", 0, BUTTON, NOT 1 | 2, 0, 0, 0, 0, 1 | NOT 2, 100
        \\  AUTOCHECKBOX "",1,1,1,1,1,1 | NOT ~0 | 1
        \\}
    ,
        \\root
        \\ dialog 1 DIALOGEX [0 common_resource_attributes]
        \\  x:
        \\   literal 1
        \\  y:
        \\   literal 2
        \\  width:
        \\   literal 3
        \\  height:
        \\   literal 4
        \\ {
        \\  control_statement AUTOCHECKBOX text: ""
        \\   id:
        \\    literal 0
        \\   x:
        \\    literal 0
        \\   y:
        \\    literal 0
        \\   width:
        \\    literal 0
        \\   height:
        \\    literal 0
        \\   style:
        \\    not_expression NOT 1
        \\   exstyle:
        \\    not_expression NOT 2
        \\   help_id:
        \\    literal 100
        \\  control_statement CONTROL text: ""
        \\   class:
        \\    literal BUTTON
        \\   id:
        \\    literal 0
        \\   x:
        \\    literal 0
        \\   y:
        \\    literal 0
        \\   width:
        \\    literal 0
        \\   height:
        \\    literal 0
        \\   style:
        \\    binary_expression |
        \\     not_expression NOT 1
        \\     literal 2
        \\   exstyle:
        \\    binary_expression |
        \\     literal 1
        \\     not_expression NOT 2
        \\   help_id:
        \\    literal 100
        \\  control_statement AUTOCHECKBOX text: ""
        \\   id:
        \\    literal 1
        \\   x:
        \\    literal 1
        \\   y:
        \\    literal 1
        \\   width:
        \\    literal 1
        \\   height:
        \\    literal 1
        \\   style:
        \\    binary_expression |
        \\     binary_expression |
        \\      literal 1
        \\      not_expression NOT ~0
        \\     literal 1
        \\ }
        \\
    );
}

test "menus" {
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "empty menu of type 'MENU' not allowed" }},
        "1 MENU {}",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "empty menu of type 'MENUEX' not allowed" }},
        "1 MENUEX {}",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "empty menu of type 'POPUP' not allowed" }},
        "1 MENU { MENUITEM SEPARATOR POPUP \"\" {} }",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected '<'}' or END>', got 'hello'" }},
        "1 MENU { hello }",
        null,
    );
    try testParse(
        \\1 MENU FIXED VERSION 1 CHARACTERISTICS (1+2) {
        \\    MENUITEM SEPARATOR,,
        \\    MENUITEM "HELLO",, 100, CHECKED,, GRAYED,,
        \\    MENUITEM "HELLO" 100 GRAYED INACTIVE
        \\    MENUITEM L"hello" (100+2)
        \\    POPUP "hello" {
        \\        MENUITEM "goodbye", 100
        \\        POPUP "goodbye",, GRAYED CHECKED
        \\        BEGIN
        \\            POPUP "" { MENUITEM SEPARATOR }
        \\        END
        \\    }
        \\}
    ,
        \\root
        \\ menu 1 MENU [1 common_resource_attributes]
        \\  simple_statement VERSION
        \\   literal 1
        \\  simple_statement CHARACTERISTICS
        \\   grouped_expression
        \\   (
        \\    binary_expression +
        \\     literal 1
        \\     literal 2
        \\   )
        \\ {
        \\  menu_item_separator MENUITEM SEPARATOR
        \\  menu_item MENUITEM "HELLO" [2 options]
        \\   literal 100
        \\  menu_item MENUITEM "HELLO" [2 options]
        \\   literal 100
        \\  menu_item MENUITEM L"hello" [0 options]
        \\   grouped_expression
        \\   (
        \\    binary_expression +
        \\     literal 100
        \\     literal 2
        \\   )
        \\  popup POPUP "hello" [0 options]
        \\  {
        \\   menu_item MENUITEM "goodbye" [0 options]
        \\    literal 100
        \\   popup POPUP "goodbye" [2 options]
        \\   BEGIN
        \\    popup POPUP "" [0 options]
        \\    {
        \\     menu_item_separator MENUITEM SEPARATOR
        \\    }
        \\   END
        \\  }
        \\ }
        \\
    );

    try testParse(
        \\1 MENUEX FIXED 1000 VERSION 1 CHARACTERISTICS (1+2) {
        \\    MENUITEM "", -1, 0x00000800L
        \\    MENUITEM ""
        \\    MENUITEM "hello",,,,
        \\    MENUITEM "hello",,,1,
        \\    POPUP "hello",,,,, {
        \\        POPUP "goodbye",,,,3,
        \\        BEGIN
        \\            POPUP "" { MENUITEM "" }
        \\        END
        \\    }
        \\    POPUP "blah", , , {
        \\        MENUITEM "blah", , ,
        \\    }
        \\}
    ,
        \\root
        \\ menu 1 MENUEX [1 common_resource_attributes]
        \\  simple_statement VERSION
        \\   literal 1
        \\  simple_statement CHARACTERISTICS
        \\   grouped_expression
        \\   (
        \\    binary_expression +
        \\     literal 1
        \\     literal 2
        \\   )
        \\  help_id:
        \\   literal 1000
        \\ {
        \\  menu_item_ex MENUITEM ""
        \\   id:
        \\    literal -1
        \\   type:
        \\    literal 0x00000800L
        \\  menu_item_ex MENUITEM ""
        \\  menu_item_ex MENUITEM "hello"
        \\  menu_item_ex MENUITEM "hello"
        \\   state:
        \\    literal 1
        \\  popup_ex POPUP "hello"
        \\  {
        \\   popup_ex POPUP "goodbye"
        \\    help_id:
        \\     literal 3
        \\   BEGIN
        \\    popup_ex POPUP ""
        \\    {
        \\     menu_item_ex MENUITEM ""
        \\    }
        \\   END
        \\  }
        \\  popup_ex POPUP "blah"
        \\  {
        \\   menu_item_ex MENUITEM "blah"
        \\  }
        \\ }
        \\
    );
}

test "versioninfo" {
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected '<'{' or BEGIN>', got ','" }},
        \\1 VERSIONINFO PRODUCTVERSION 1,2,3,4,5 {}
    ,
        null,
    );
    try testParseErrorDetails(
        &.{
            .{ .type = .warning, .str = "the padding before this quoted string value would be miscompiled by the Win32 RC compiler" },
            .{ .type = .note, .str = "to avoid the potential miscompilation, consider adding a comma between the key and the quoted string" },
        },
        \\1 VERSIONINFO { VALUE "key" "value" }
    ,
        null,
    );
    try testParseErrorDetails(
        &.{
            .{ .type = .warning, .str = "the byte count of this value would be miscompiled by the Win32 RC compiler" },
            .{ .type = .note, .str = "to avoid the potential miscompilation, do not mix numbers and strings within a value" },
        },
        \\1 VERSIONINFO { VALUE "key", "value" 1 }
    ,
        null,
    );
    try testParse(
        \\1 VERSIONINFO FIXED
        \\FILEVERSION 1
        \\PRODUCTVERSION 1,3-1,3,4
        \\FILEFLAGSMASK 1
        \\FILEFLAGS (1|2)
        \\FILEOS 2
        \\FILETYPE 3
        \\FILESUBTYPE 4
        \\{
        \\  VALUE "hello"
        \\  BLOCK "something",,
        \\  BEGIN
        \\      BLOCK "something else",, 1,, 2
        \\      BEGIN
        \\          VALUE "key",,
        \\          VALUE "key",, 1,, 2,, 3,,
        \\          VALUE "key" 1 2 3 "4"
        \\      END
        \\  END
        \\}
    ,
        \\root
        \\ version_info 1 VERSIONINFO [1 common_resource_attributes]
        \\  version_statement FILEVERSION
        \\   literal 1
        \\  version_statement PRODUCTVERSION
        \\   literal 1
        \\   binary_expression -
        \\    literal 3
        \\    literal 1
        \\   literal 3
        \\   literal 4
        \\  simple_statement FILEFLAGSMASK
        \\   literal 1
        \\  simple_statement FILEFLAGS
        \\   grouped_expression
        \\   (
        \\    binary_expression |
        \\     literal 1
        \\     literal 2
        \\   )
        \\  simple_statement FILEOS
        \\   literal 2
        \\  simple_statement FILETYPE
        \\   literal 3
        \\  simple_statement FILESUBTYPE
        \\   literal 4
        \\ {
        \\  block_value VALUE "hello"
        \\  block BLOCK "something"
        \\  BEGIN
        \\   block BLOCK "something else"
        \\    block_value_value ,
        \\     literal 1
        \\    block_value_value
        \\     literal 2
        \\   BEGIN
        \\    block_value VALUE "key"
        \\    block_value VALUE "key"
        \\     block_value_value ,
        \\      literal 1
        \\     block_value_value ,
        \\      literal 2
        \\     block_value_value ,
        \\      literal 3
        \\    block_value VALUE "key"
        \\     block_value_value
        \\      literal 1
        \\     block_value_value
        \\      literal 2
        \\     block_value_value
        \\      literal 3
        \\     block_value_value
        \\      literal "4"
        \\   END
        \\  END
        \\ }
        \\
    );
}

test "dangling id at end of file" {
    try testParse(
        \\1 RCDATA {}
        \\END
        \\
    ,
        \\root
        \\ resource_raw_data 1 RCDATA [0 common_resource_attributes] raw data: 0
        \\ invalid context.len: 2
        \\  literal:END
        \\  eof:
        \\
    );
}

test "dlginclude" {
    try testParse(
        \\1 DLGINCLUDE "something.h"
        \\2 DLGINCLUDE FIXED L"Something.h"
    ,
        \\root
        \\ resource_external 1 DLGINCLUDE [0 common_resource_attributes]
        \\  literal "something.h"
        \\ resource_external 2 DLGINCLUDE [1 common_resource_attributes]
        \\  literal L"Something.h"
        \\
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected quoted string literal; got 'something.h'" }},
        "1 DLGINCLUDE something.h",
        null,
    );
}

test "toolbar" {
    try testParse(
        \\1 TOOLBAR DISCARDABLE 16, 15
        \\BEGIN
        \\  BUTTON 1
        \\  SEPARATOR
        \\  BUTTON 2
        \\END
    ,
        \\root
        \\ toolbar 1 TOOLBAR [1 common_resource_attributes]
        \\  button_width:
        \\   literal 16
        \\  button_height:
        \\   literal 15
        \\ BEGIN
        \\  simple_statement BUTTON
        \\   literal 1
        \\  literal SEPARATOR
        \\  simple_statement BUTTON
        \\   literal 2
        \\ END
        \\
    );
}

test "semicolons" {
    try testParse(
        \\STRINGTABLE
        \\BEGIN
        \\  512; this is all ignored
        \\  "what"
        \\END
        \\1; RC;DATA {
        \\  1;100
        \\  2
        \\}
        \\; This is basically a comment
        \\
    ,
        \\root
        \\ string_table STRINGTABLE [0 common_resource_attributes]
        \\ BEGIN
        \\  string_table_string
        \\   literal 512
        \\   "what"
        \\ END
        \\ resource_raw_data 1; RC;DATA [0 common_resource_attributes] raw data: 2
        \\  literal 1
        \\  literal 2
        \\
    );
}

test "parse errors" {
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "unfinished raw data block at '<eof>', expected closing '}' or 'END'" }},
        "id RCDATA { 1",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "unfinished string literal at '<eof>', expected closing '\"'" }},
        "id RCDATA \"unfinished string",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected ')', got '}'" }},
        "id RCDATA { (1 }",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "character '\\x1A' is not allowed" }},
        "id RCDATA { \"\x1A\" }",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "character '\\x01' is not allowed outside of string literals" }},
        "id RCDATA { \x01 }",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "character '@' is not allowed outside of string literals" }},
        // This @ is outside the string literal from the perspective of a C preprocessor
        // but inside the string literal from the perspective of the RC parser. We still
        // want to error to emulate the behavior of the Win32 RC preprocessor.
        "id RCDATA { \"hello\n@\" }",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "escaping quotes with \\\" is not allowed (use \"\" instead)" }},
        "id RCDATA { \"\\\"\"\" }",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected number or number expression; got '\"hello\"'" }},
        "STRINGTABLE { \"hello\" }",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected quoted string literal; got '1'" }},
        "STRINGTABLE { 1, 1 }",
        null,
    );
    try testParseErrorDetails(
        &.{
            .{ .type = .err, .str = "expected '<filename>', found '{' (resource type 'icon' can't use raw data)" },
            .{ .type = .note, .str = "if '{' is intended to be a filename, it must be specified as a quoted string literal" },
        },
        "1 ICON {}",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "id of resource type 'font' must be an ordinal (u16), got 'string'" }},
        "string FONT filename",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected accelerator type or option [ASCII, VIRTKEY, etc]; got 'NOTANOPTIONORTYPE'" }},
        "1 ACCELERATORS { 1, 1, NOTANOPTIONORTYPE",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected number, number expression, or quoted string literal; got 'hello'" }},
        "1 ACCELERATORS { hello, 1 }",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "expected number or number expression; got '\"hello\"'" }},
        "1 ACCELERATORS { 1, \"hello\" }",
        null,
    );
    try testParseErrorDetails(
        &.{
            .{ .type = .err, .str = "the number 6 (RT_STRING) cannot be used as a resource type" },
            .{ .type = .note, .str = "using RT_STRING directly likely results in an invalid .res file, use a STRINGTABLE instead" },
        },
        "1 6 {}",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "name or id is not allowed for resource type 'stringtable'" }},
        "1 STRINGTABLE { 1 \"\" }",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "unsupported code page 'utf7 (id=65000)' in #pragma code_page" }},
        "#pragma code_page( 65000 )",
        null,
    );
    try testParseErrorDetails(
        &.{
            .{ .type = .err, .str = "expected quoted string literal or unquoted literal; got ')'" },
            .{ .type = .note, .str = "the Win32 RC compiler would accept ')' as a valid expression, but it would be skipped over and potentially lead to unexpected outcomes" },
        },
        "1 RCDATA )",
        null,
    );
    try testParseErrorDetails(
        &.{
            .{ .type = .err, .str = "expected number, number expression, or quoted string literal; got ')'" },
            .{ .type = .note, .str = "the Win32 RC compiler would accept ')' as a valid expression, but it would be skipped over and potentially lead to unexpected outcomes" },
        },
        "1 RCDATA { 1, ), 2 }",
        null,
    );
}

test "max nested menu level" {
    var source_buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer source_buffer.deinit();

    try source_buffer.appendSlice("1 MENU {\n");
    for (0..resinator.parse.max_nested_menu_level) |_| {
        try source_buffer.appendSlice("POPUP \"foo\" {\n");
    }
    for (0..resinator.parse.max_nested_menu_level) |_| {
        try source_buffer.appendSlice("}\n");
    }
    try source_buffer.appendSlice("}");

    // Exactly hitting the nesting level is okay, but we still error
    // because the innermost nested POPUP is empty.
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "empty menu of type 'POPUP' not allowed" }},
        source_buffer.items,
        null,
    );

    // Now reset and nest until the nesting level is 1 more than the max
    source_buffer.clearRetainingCapacity();
    try source_buffer.appendSlice("1 MENU {\n");
    for (0..resinator.parse.max_nested_menu_level + 1) |_| {
        try source_buffer.appendSlice("POPUP \"foo\" {\n");
    }
    for (0..resinator.parse.max_nested_menu_level + 1) |_| {
        try source_buffer.appendSlice("}\n");
    }
    try source_buffer.appendSlice("}");

    // Now we should get the nesting level error.
    try testParseErrorDetails(
        &.{
            .{ .type = .err, .str = "menu contains too many nested children (max is 512)" },
            .{ .type = .note, .str = "max menu nesting level exceeded here" },
        },
        source_buffer.items,
        null,
    );
}

test "max nested versioninfo level" {
    var source_buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer source_buffer.deinit();

    try source_buffer.appendSlice("1 VERSIONINFO {\n");
    for (0..resinator.parse.max_nested_version_level) |_| {
        try source_buffer.appendSlice("BLOCK \"foo\" {\n");
    }
    for (0..resinator.parse.max_nested_version_level) |_| {
        try source_buffer.appendSlice("}\n");
    }
    try source_buffer.appendSlice("}");

    // This should succeed, but we don't care about validating the tree since it's
    // just giant nested nonsense.
    try testParseErrorDetails(
        &.{},
        source_buffer.items,
        null,
    );

    // Now reset and nest until the nesting level is 1 more than the max
    source_buffer.clearRetainingCapacity();
    try source_buffer.appendSlice("1 VERSIONINFO {\n");
    for (0..resinator.parse.max_nested_version_level + 1) |_| {
        try source_buffer.appendSlice("BLOCK \"foo\" {\n");
    }
    for (0..resinator.parse.max_nested_version_level + 1) |_| {
        try source_buffer.appendSlice("}\n");
    }
    try source_buffer.appendSlice("}");

    // Now we should get the nesting level error.
    try testParseErrorDetails(
        &.{
            .{ .type = .err, .str = "versioninfo contains too many nested children (max is 512)" },
            .{ .type = .note, .str = "max versioninfo nesting level exceeded here" },
        },
        source_buffer.items,
        null,
    );
}

test "max dialog controls" {
    var source_buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer source_buffer.deinit();

    const max_controls = std.math.maxInt(u16);

    try source_buffer.appendSlice("1 DIALOGEX 1, 2, 3, 4 {\n");
    for (0..max_controls) |_| {
        try source_buffer.appendSlice("CHECKBOX \"foo\", 1, 2, 3, 4, 5\n");
    }
    try source_buffer.appendSlice("}");

    // This should succeed, but we don't care about validating the tree since it's
    // just a dialog with a giant list of controls.
    try testParseErrorDetails(
        &.{},
        source_buffer.items,
        null,
    );

    // Now pop the } and add one more control
    _ = source_buffer.pop();
    try source_buffer.appendSlice("CHECKBOX \"foo\", 1, 2, 3, 4, 5\n");
    try source_buffer.appendSlice("}");

    // Now we should get the 'too many controls' error.
    try testParseErrorDetails(
        &.{
            .{ .type = .err, .str = "dialogex contains too many controls (max is 65535)" },
            .{ .type = .note, .str = "maximum number of controls exceeded here" },
        },
        source_buffer.items,
        null,
    );
}

test "max toolbar buttons" {
    var source_buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer source_buffer.deinit();

    const max_buttons = std.math.maxInt(u16);

    try source_buffer.appendSlice("1 TOOLBAR 1, 2 {\n");
    for (0..max_buttons) |_| {
        try source_buffer.appendSlice("BUTTON 1\n");
    }
    try source_buffer.appendSlice("}");

    // This should succeed, but we don't care about validating the tree since it's
    // just a dialog with a giant list of buttons.
    try testParseErrorDetails(
        &.{},
        source_buffer.items,
        null,
    );

    // Now pop the } and add one more button
    _ = source_buffer.pop();
    try source_buffer.appendSlice("BUTTON 1\n");
    try source_buffer.appendSlice("}");

    // Now we should get the 'too many buttons' error.
    try testParseErrorDetails(
        &.{
            .{ .type = .err, .str = "toolbar contains too many buttons (max is 65535)" },
            .{ .type = .note, .str = "maximum number of buttons exceeded here" },
        },
        source_buffer.items,
        null,
    );
}

test "max expression level" {
    var source_buffer = std.ArrayList(u8).init(std.testing.allocator);
    defer source_buffer.deinit();

    try source_buffer.appendSlice("1 RCDATA {\n");
    for (0..resinator.parse.max_nested_expression_level) |_| {
        try source_buffer.appendSlice("(\n");
    }
    try source_buffer.append('1');
    for (0..resinator.parse.max_nested_expression_level) |_| {
        try source_buffer.appendSlice(")\n");
    }
    try source_buffer.appendSlice("}");

    // This should succeed, but we don't care about validating the tree since it's
    // just a raw data block with a 1 surrounded by a bunch of parens
    try testParseErrorDetails(
        &.{},
        source_buffer.items,
        null,
    );

    // Now reset and add 1 more than the max expression level.
    source_buffer.clearRetainingCapacity();
    try source_buffer.appendSlice("1 RCDATA {\n");
    for (0..resinator.parse.max_nested_expression_level + 1) |_| {
        try source_buffer.appendSlice("(\n");
    }
    try source_buffer.append('1');
    for (0..resinator.parse.max_nested_expression_level + 1) |_| {
        try source_buffer.appendSlice(")\n");
    }
    try source_buffer.appendSlice("}");

    // Now we should get the 'too many controls' error.
    try testParseErrorDetails(
        &.{
            .{ .type = .err, .str = "expression contains too many syntax levels (max is 200)" },
            .{ .type = .note, .str = "maximum expression level exceeded here" },
        },
        source_buffer.items,
        null,
    );
}

test "code page pragma" {
    try testParseErrorDetails(
        &.{},
        "#pragma code_page(1252)",
        null,
    );
    try testParseErrorDetails(
        &.{},
        "#pragma code_page(DEFAULT)",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "invalid or unknown code page in #pragma code_page" }},
        "#pragma code_page(12)",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "unsupported code page 'utf7 (id=65000)' in #pragma code_page" }},
        "#pragma code_page(65000)",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "code page is not a valid integer in #pragma code_page" }},
        "#pragma code_page(0)",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "code page is not a valid integer in #pragma code_page" }},
        "#pragma code_page(00)",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "code page is not a valid integer in #pragma code_page" }},
        "#pragma code_page(123abc)",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "invalid or unknown code page in #pragma code_page" }},
        "#pragma code_page(01252)",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "code page too large in #pragma code_page" }},
        "#pragma code_page(4294967333)",
        null,
    );
}

test "numbers with exponents" {
    // Compatibility with error RC2021: expected exponent value, not '1'
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "base 10 number literal with exponent is not allowed: -002e6" }},
        "1 RCDATA { -002e645 }",
        null,
    );
    try testParseErrorDetails(
        &.{.{ .type = .err, .str = "base 10 number literal with exponent is not allowed: ~2E1" }},
        "1 RCDATA { ~2E1 }",
        null,
    );
    try testParseErrorDetails(
        &.{},
        "1 RCDATA { 0x2e1 }",
        null,
    );
    try testParseErrorDetails(
        &.{},
        "1 RCDATA { 2eA }",
        null,
    );
    try testParseErrorDetails(
        &.{},
        "1 RCDATA { -002ea }",
        null,
    );
    try testParseErrorDetails(
        &.{},
        "1 RCDATA { -002e }",
        null,
    );
}

test "unary plus" {
    try testParseErrorDetails(
        &.{
            .{ .type = .err, .str = "expected number, number expression, or quoted string literal; got '+'" },
            .{ .type = .note, .str = "the Win32 RC compiler may accept '+' as a unary operator here, but it is not supported in this implementation; consider omitting the unary +" },
        },
        "1 RCDATA { +2 }",
        null,
    );
}

test "control style potential miscompilation" {
    try testParseErrorDetails(
        &.{
            .{ .type = .warning, .str = "this token could be erroneously skipped over by the Win32 RC compiler" },
            .{ .type = .note, .str = "to avoid the potential miscompilation, consider adding a comma after the style parameter" },
        },
        "1 DIALOGEX 1, 2, 3, 4 { CONTROL \"text\", 100, BUTTON, 3 1, 2, 3, 4, 100 }",
        null,
    );
}

test "language with L suffixed part" {
    // As a top-level statement
    try testParseErrorDetails(
        &.{
            .{ .type = .warning, .str = "this language parameter would be an error in the Win32 RC compiler" },
            .{ .type = .note, .str = "to avoid the error, remove any L suffixes from numbers within the parameter" },
        },
        \\LANGUAGE 1L, 2
    ,
        null,
    );
    // As an optional statement in a resource
    try testParseErrorDetails(
        &.{
            .{ .type = .warning, .str = "this language parameter would be an error in the Win32 RC compiler" },
            .{ .type = .note, .str = "to avoid the error, remove any L suffixes from numbers within the parameter" },
        },
        \\STRINGTABLE LANGUAGE (2-1L), 1 { 1, "" }
    ,
        null,
    );
}

test "duplicate optional statements" {
    try testParseErrorDetails(
        &.{
            .{ .type = .warning, .str = "this statement was ignored; when multiple statements of the same type are specified, only the last takes precedence" },
            .{ .type = .warning, .str = "this statement was ignored; when multiple statements of the same type are specified, only the last takes precedence" },
            .{ .type = .warning, .str = "this statement was ignored; when multiple statements of the same type are specified, only the last takes precedence" },
            .{ .type = .warning, .str = "this statement was ignored; when multiple statements of the same type are specified, only the last takes precedence" },
            .{ .type = .warning, .str = "this statement was ignored; when multiple statements of the same type are specified, only the last takes precedence" },
            .{ .type = .warning, .str = "this statement was ignored; when multiple statements of the same type are specified, only the last takes precedence" },
            .{ .type = .warning, .str = "this statement was ignored; when multiple statements of the same type are specified, only the last takes precedence" },
            .{ .type = .warning, .str = "this statement was ignored; when multiple statements of the same type are specified, only the last takes precedence" },
            .{ .type = .warning, .str = "this statement was ignored; when multiple statements of the same type are specified, only the last takes precedence" },
            .{ .type = .warning, .str = "this statement was ignored; when multiple statements of the same type are specified, only the last takes precedence" },
        },
        \\1 DIALOGEX 1, 2, 3, 4
        \\  CHARACTERISTICS 1
        \\  LANGUAGE 0xFF, 0xFF
        \\  LANGUAGE 0x09, 0x01
        \\  CHARACTERISTICS 999
        \\  CHARACTERISTICS 0x1234
        \\  VERSION 999
        \\  VERSION 1
        \\  MENU 1
        \\  MENU 2
        \\  CLASS "foo"
        \\  CLASS "bar"
        \\  CAPTION "foo"
        \\  CAPTION "bar"
        \\  FONT 1, "foo"
        \\  FONT 2, "bar"
        \\  STYLE 1
        \\  STYLE 2
        \\  EXSTYLE 1
        \\  EXSTYLE 2
        \\{}
    ,
        null,
    );
}

fn testParse(source: []const u8, expected_ast_dump: []const u8) !void {
    const allocator = std.testing.allocator;
    var diagnostics = resinator.errors.Diagnostics.init(allocator);
    defer diagnostics.deinit();
    // TODO: test different code pages
    var lexer = resinator.lex.Lexer.init(source, .{});
    var parser = resinator.parse.Parser.init(&lexer, .{});
    var tree = parser.parse(allocator, &diagnostics) catch |err| switch (err) {
        error.ParseError => {
            diagnostics.renderToStdErrDetectTTY(std.fs.cwd(), source, null);
            return err;
        },
        else => |e| return e,
    };
    defer tree.deinit();

    var buf = std.ArrayList(u8).init(allocator);
    defer buf.deinit();

    try tree.dump(buf.writer());
    try std.testing.expectEqualStrings(expected_ast_dump, buf.items);
}

const ExpectedErrorDetails = struct {
    str: []const u8,
    type: resinator.errors.ErrorDetails.Type,
};

fn testParseErrorDetails(expected_details: []const ExpectedErrorDetails, source: []const u8, maybe_expected_output: ?[]const u8) !void {
    const allocator = std.testing.allocator;
    var diagnostics = resinator.errors.Diagnostics.init(allocator);
    defer diagnostics.deinit();

    const expect_fail = for (expected_details) |details| {
        if (details.type == .err) break true;
    } else false;

    const tree: ?*resinator.ast.Tree = tree: {
        var lexer = resinator.lex.Lexer.init(source, .{});
        var parser = resinator.parse.Parser.init(&lexer, .{});
        const tree = parser.parse(allocator, &diagnostics) catch |err| switch (err) {
            error.OutOfMemory => |e| return e,
            error.ParseError => {
                if (!expect_fail) {
                    diagnostics.renderToStdErrDetectTTY(std.fs.cwd(), source, null);
                    return err;
                }
                break :tree null;
            },
        };
        break :tree tree;
    };
    defer if (tree != null) tree.?.deinit();

    if (tree != null and expect_fail) {
        std.debug.print("expected parse error, got tree:\n", .{});
        try tree.?.dump(std.io.getStdErr().writer());
        return error.UnexpectedSuccess;
    }

    if (expected_details.len != diagnostics.errors.items.len) {
        std.debug.print("expected {} error details, got {}:\n", .{ expected_details.len, diagnostics.errors.items.len });
        diagnostics.renderToStdErrDetectTTY(std.fs.cwd(), source, null);
        return error.ErrorDetailMismatch;
    }
    for (diagnostics.errors.items, expected_details) |actual, expected| {
        std.testing.expectEqual(expected.type, actual.type) catch |e| {
            diagnostics.renderToStdErrDetectTTY(std.fs.cwd(), source, null);
            return e;
        };
        var buf: [256]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&buf);
        try actual.render(fbs.writer(), source, diagnostics.strings.items);
        try std.testing.expectEqualStrings(expected.str, fbs.getWritten());
    }

    if (maybe_expected_output) |expected_output| {
        var buf = std.ArrayList(u8).init(allocator);
        defer buf.deinit();

        try tree.?.dump(buf.writer());
        try std.testing.expectEqualStrings(expected_output, buf.items);
    }
}
