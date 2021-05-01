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

    fn allocateAst(self: *Self, location: Location, spec: AstSpec) *Ast {
        var ast = self.allocator.allocator.create(Ast) catch unreachable;
        ast.* = Ast{
            .location = location,
            .spec = spec,
        };
        return ast;
    }

    fn skipToken(self: *Self) void {
        _ = self.lexer.read();
    }

    pub fn parseExpression(self: *Self) ?*Ast {
        while (self.lexer.peek()) |token| {
            if (self.parseAtomic()) |ast| {
                return ast;
            }
            //switch (token.kind) {
            //    else => |kind| {
            //        self.reportError(&token.location, "Unexpected token: {any}", .{kind});
            //        _ = self.lexer.read();
            //    },
            //}
        }
        return null;
    }

    pub fn parseAtomic(self: *Self) ?*Ast {
        if (self.lexer.peek()) |token| {
            switch (token.kind) {
                .Identifier => |name| {
                    self.skipToken();
                    var ast = self.allocateAst(token.location, AstSpec{ .Identifier = .{ .name = name } });
                    return ast;
                },
                else => |kind| {
                    self.skipToken();
                    self.reportError(&token.location, "Unexpected token: {any}", .{kind});
                },
            }
        }
        return null;
    }
};
