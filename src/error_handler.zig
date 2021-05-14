const std = @import("std");
const term = @import("ansi-term");

usingnamespace @import("location.zig");

pub const ErrorReporter = struct {
    reportFn: fn (self: *@This(), message: []const u8, location: *const Location) void,

    const Self = @This();

    pub fn report(self: *Self, message: []const u8, location: *const Location) void {
        self.reportFn(self, message, location);
    }
};

pub const ConsoleErrorReporter = struct {
    reporter: ErrorReporter = ErrorReporter{
        .reportFn = report,
    },

    stdOut: std.fs.File.Writer,

    const Self = @This();

    pub fn init(writer: std.fs.File.Writer) Self {
        return Self{
            .stdOut = writer,
        };
    }

    pub fn report(reporter: *ErrorReporter, message: []const u8, location: *const Location) void {
        const self = @fieldParentPtr(Self, "reporter", reporter);
        const style = term.Style{ .foreground = .Red };
        term.updateStyle(self.stdOut, style, null) catch {};
        std.fmt.format(self.stdOut, "{s}:{}:{}: {s}\n", .{ location.file, location.line, location.column, message }) catch {};
        term.updateStyle(self.stdOut, .{}, style) catch {};

        // @todo: Print the line containing the error.
    }
};

pub const NullErrorReporter = struct {
    reporter: ErrorReporter = ErrorReporter{
        .reportFn = report,
    },

    errors: usize = 0,

    const Self = @This();

    pub fn report(reporter: *ErrorReporter, message: []const u8, location: *const Location) void {
        const self = @fieldParentPtr(Self, "reporter", reporter);
        self.errors += 1;
    }
};
