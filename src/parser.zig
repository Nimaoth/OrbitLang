const std = @import("std");

usingnamespace @import("location.zig");
usingnamespace @import("lexer.zig");
usingnamespace @import("ast.zig");
usingnamespace @import("error_handler.zig");

pub const Parser = struct {
    allocator: std.heap.ArenaAllocator,
    lexer: Lexer,
    errorReporter: *ErrorReporter,
    errorMsgBuffer: std.ArrayList(u8),

    const Self = @This();

    pub fn init(lexer: Lexer, allocator: *std.mem.Allocator, errorReporter: *ErrorReporter) Self {
        return Self{
            .allocator = std.heap.ArenaAllocator.init(allocator),
            .lexer = lexer,
            .errorReporter = errorReporter,
            .errorMsgBuffer = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.errorMsgBuffer.deinit();
        self.allocator.deinit();
    }

    fn reportError(self: *Self, location: *const Location, comptime format: []const u8, args: anytype) void {
        self.errorMsgBuffer.resize(0) catch unreachable;
        std.fmt.format(self.errorMsgBuffer.writer(), format, args) catch {};
        self.errorReporter.report(self.errorMsgBuffer.items, location);
    }

    pub fn parseExpression(self: *Self) ?Ast {
        while (self.lexer.peek()) |token| {
            switch (token.kind) {
                else => |kind| {
                    self.reportError(&token.location, "Unexpected token: {any}", .{kind});
                    _ = self.lexer.read();
                },
            }
        }
        return null;
    }
};
