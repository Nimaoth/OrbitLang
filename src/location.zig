const std = @import("std");

usingnamespace @import("common.zig");

pub const Location = struct {
    file: String,
    index: u64 = 0,
    line: u64 = 0,
    column: u64 = 0,

    const Self = @This();

    pub fn format(
        self: *const Self,
        fmt: String,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try std.fmt.format(writer, "{s}:{}:{}", .{ self.file, self.line, self.column });
    }
};
