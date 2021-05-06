const std = @import("std");

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

    const Self = @This();

    pub fn report(reporter: *ErrorReporter, message: []const u8, location: *const Location) void {
        const self = @fieldParentPtr(Self, "reporter", reporter);
        std.log.err("./examples/ideas.orb:{}:{}: {s}", .{ location.line, location.column, message });
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
