const std = @import("std");
const DepfileFormat = @import("cli.zig").Options.DepfileFormat;

const Options = struct {
    target: []const u8,
    deps: []const []const u8,
    fmt: DepfileFormat,
};

const Error = std.Io.Writer.Error || error{TabInTargetOrPrereqPath};

pub fn write(w: *std.Io.Writer, options: Options) Error!void {
    switch (options.fmt) {
        .json => {
            var write_stream: std.json.Stringify = .{
                .writer = w,
                .options = .{ .whitespace = .indent_2 },
            };

            try write_stream.beginArray();
            for (options.deps) |dep_path| {
                try write_stream.write(dep_path);
            }
            try write_stream.endArray();
        },
        .make => {
            // Based on clang's implementation (DependencyFile.cpp and MakeSupport.cpp),
            // but TAB in any path is an error.
            const max_columns = 75;
            var columns: usize = 0;

            try escapePathForMake(w, options.target);
            columns += options.target.len;
            try w.writeByte(':');
            columns += 1;

            for (options.deps) |path| {
                if (columns + path.len + " \\\n".len > max_columns) {
                    try w.writeAll(" \\\n ");
                    columns = 1;
                }
                try w.writeByte(' ');
                try escapePathForMake(w, path);
                columns += path.len + 1;
            }
            try w.writeByte('\n');
        },
    }
}

fn escapePathForMake(w: *std.Io.Writer, path: []const u8) !void {
    for (path, 0..) |c, i| {
        switch (c) {
            ' ' => {
                try w.writeByte('\\');
                var j = i;
                while (j != 0) {
                    j -= 1;
                    if (path[j] != '\\') break;
                    try w.writeByte('\\');
                }
            },
            '$' => try w.writeByte('$'),
            '#' => try w.writeByte('\\'),
            '\t' => return error.TabInTargetOrPrereqPath,
            else => {},
        }
        try w.writeByte(c);
    }
}
