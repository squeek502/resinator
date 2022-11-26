const std = @import("std");
const builtin = @import("builtin");
const windows = std.os.windows;

/// Like std.io.FixedBufferStream but does no bounds checking
pub const UncheckedSliceWriter = struct {
    const Self = @This();

    pos: usize = 0,
    slice: []u8,

    pub fn write(self: *Self, char: u8) void {
        self.slice[self.pos] = char;
        self.pos += 1;
    }

    pub fn writeSlice(self: *Self, slice: []const u8) void {
        for (slice) |c| {
            self.write(c);
        }
    }

    pub fn getWritten(self: Self) []u8 {
        return self.slice[0..self.pos];
    }
};

// Similar to std.debug.TTY.Config
pub const Colors = enum {
    no_color,
    escape_codes,
    windows_api,

    pub fn detect() Colors {
        if (std.process.hasEnvVarConstant("NO_COLOR")) {
            return .no_color;
        } else {
            const stderr_file = std.io.getStdErr();
            if (stderr_file.supportsAnsiEscapeCodes()) {
                return .escape_codes;
            } else if (builtin.os.tag == .windows and stderr_file.isTty()) {
                return .windows_api;
            } else {
                return .no_color;
            }
        }
    }

    pub const Color = enum {
        reset,
        red,
        green,
        cyan,
        yellow,
        white,
        dim,
        bold,

        pub fn escapeSequence(self: Color) []const u8 {
            return switch (self) {
                .reset => return "\x1b[0m",
                .red => "\x1b[31;1m",
                .green => "\x1b[32;1m",
                .cyan => "\x1b[36;1m",
                .yellow => "\x1b[93;1m",
                .white => "\x1b[37;1m",
                .dim => "\x1b[2m",
                .bold => "\x1b[1m",
            };
        }

        pub fn characterAttributes(self: Color, reset_attrs: windows.WORD) windows.WORD {
            return switch (self) {
                .reset => reset_attrs,
                .red => windows.FOREGROUND_RED | windows.FOREGROUND_INTENSITY,
                .green => windows.FOREGROUND_GREEN | windows.FOREGROUND_INTENSITY,
                .cyan => windows.FOREGROUND_GREEN | windows.FOREGROUND_BLUE | windows.FOREGROUND_INTENSITY,
                .yellow => windows.FOREGROUND_RED | windows.FOREGROUND_GREEN | windows.FOREGROUND_INTENSITY,
                .white, .bold => windows.FOREGROUND_RED | windows.FOREGROUND_GREEN | windows.FOREGROUND_BLUE | windows.FOREGROUND_INTENSITY,
                .dim => windows.FOREGROUND_INTENSITY,
            };
        }
    };

    pub fn set(self: Colors, out_stream: anytype, color: Color) void {
        nosuspend switch (self) {
            .no_color => return,
            .escape_codes => out_stream.writeAll(color.escapeSequence()) catch return,
            .windows_api => if (builtin.os.tag == .windows) {
                const stderr_file = std.io.getStdErr();
                const S = struct {
                    var attrs: windows.WORD = undefined;
                    var init_attrs = false;
                };
                if (!S.init_attrs) {
                    S.init_attrs = true;
                    var info: windows.CONSOLE_SCREEN_BUFFER_INFO = undefined;
                    _ = windows.kernel32.GetConsoleScreenBufferInfo(stderr_file.handle, &info);
                    S.attrs = info.wAttributes;
                }
                _ = windows.SetConsoleTextAttribute(
                    stderr_file.handle,
                    color.characterAttributes(S.attrs),
                ) catch {};
            } else {
                unreachable;
            },
        };
    }
};
