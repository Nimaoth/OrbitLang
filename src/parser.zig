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

    fn allocateAst(self: *Self, location: Location, spec: AstSpec) !*Ast {
        var ast = try self.allocator.allocator.create(Ast);
        ast.* = Ast{
            .location = location,
            .spec = spec,
        };
        return ast;
    }

    fn skipToken(self: *Self) void {
        _ = self.lexer.read();
    }

    fn check(self: *Self, kind: TokenKind) ?Token {
        if (self.lexer.peek()) |token| {
            if (token.kind == kind) {
                return token;
            } else {
                return null;
            }
        }
        return null;
    }

    fn consume(self: *Self, kind: TokenKind, report: bool) ?Token {
        if (self.lexer.peek()) |token| {
            if (token.kind == kind) {
                _ = self.lexer.read();
                return token;
            } else {
                if (report) {
                    self.reportError(&token.location, "Expected {any}, found {any}", .{ kind, token.kind });
                }
                return null;
            }
        }
        return null;
    }

    pub fn parseExpression(self: *Self) anyerror!?*Ast {
        while (self.lexer.peek()) |token| {
            switch (token.kind) {
                else => |kind| {
                    var expr = (try self.parsePipeOrLess()) orelse {
                        _ = self.lexer.read();
                        continue;
                    };

                    if (self.lexer.peek()) |next| {
                        switch (next.kind) {
                            else => return expr,
                        }
                    }
                    return expr;
                },
            }
        }
        return null;
    }

    pub fn parsePipeOrLess(self: *Self) anyerror!?*Ast {
        var expr = (try self.parseCallOrLess()) orelse return null;
        while (self.lexer.peek()) |next| {
            switch (next.kind) {
                .Arrow => {
                    self.skipToken();
                    var right = (try self.parseCallOrLess()) orelse return null;
                    var pipe = try self.allocateAst(expr.location, AstSpec{ .Pipe = .{
                        .left = expr,
                        .right = right,
                    } });
                    expr = pipe;
                },
                else => return expr,
            }
        }
        return expr;
    }

    pub fn parseCallOrLess(self: *Self) anyerror!?*Ast {
        var expr = (try self.parseAtomic()) orelse return null;
        if (self.lexer.peek()) |next| {
            switch (next.kind) {
                .ParenLeft => return try self.parseCall(expr),
                else => return expr,
            }
        }
        return expr;
    }

    pub fn parseBlock(self: *Self) anyerror!?*Ast {
        const braceLeft = self.consume(.BraceLeft, true) orelse return null;

        var body = std.ArrayList(*Ast).init(&self.allocator.allocator);
        errdefer body.deinit();

        while (self.lexer.peek()) |token| {
            if (token.kind == .BraceRight) {
                break;
            }
            if (try self.parseExpression()) |expr| {
                try body.append(expr);
            } else {
                self.reportError(&braceLeft.location, "Expected expression.", .{});
                self.skipToken();
            }
        }

        const braceRight = self.consume(.BraceRight, true) orelse return null;

        var ast = try self.allocateAst(braceLeft.location, AstSpec{ .Block = .{
            .body = body,
        } });
        return ast;
    }

    pub fn parseCall(self: *Self, func: *Ast) anyerror!?*Ast {
        const parenLeft = self.consume(.ParenLeft, true) orelse return null;

        var args = std.ArrayList(*Ast).init(&self.allocator.allocator);
        errdefer args.deinit();

        var state: enum { AfterOpen, AfterExpr, AfterComma } = .AfterOpen;

        while (self.lexer.peek()) |token| {
            if (token.kind == .ParenRight) {
                break;
            }

            switch (state) {
                .AfterOpen => {
                    if (try self.parseExpression()) |expr| {
                        try args.append(expr);
                    } else {
                        self.reportError(&parenLeft.location, "Expected argument.", .{});
                        self.skipToken();
                    }
                    state = .AfterExpr;
                },
                .AfterExpr => {
                    _ = self.consume(.Comma, true);
                    state = .AfterComma;
                },
                .AfterComma => {
                    if (try self.parseExpression()) |expr| {
                        try args.append(expr);
                    } else {
                        self.reportError(&parenLeft.location, "Expected argument.", .{});
                        self.skipToken();
                    }
                    state = .AfterExpr;
                },
            }
        }

        const parenRight = self.consume(.ParenRight, true) orelse return null;

        // If the call if followed by a specific kind of token parse the post call expression
        if (self.lexer.peek()) |token| {
            switch (token.kind) {
                .Identifier,
                .String,
                .Int,
                .Float,
                .BraceLeft,
                => {
                    var block = (try self.parseCallOrLess()) orelse return null;
                    try args.append(block);
                },
                else => {},
            }
        }

        var ast = try self.allocateAst(func.location, AstSpec{ .Call = .{
            .func = func,
            .args = args,
        } });
        return ast;
    }

    pub fn parseAtomic(self: *Self) anyerror!?*Ast {
        if (self.lexer.peek()) |token| {
            switch (token.kind) {
                .Identifier => {
                    self.skipToken();
                    var ast = try self.allocateAst(token.location, AstSpec{ .Identifier = .{ .name = token.data.text } });
                    return ast;
                },
                .String => {
                    self.skipToken();
                    var ast = try self.allocateAst(token.location, AstSpec{ .String = .{ .value = token.data.text } });
                    return ast;
                },
                .Int => {
                    self.skipToken();
                    var ast = try self.allocateAst(token.location, AstSpec{ .Int = .{ .value = token.data.int } });
                    return ast;
                },
                .Float => {
                    self.skipToken();
                    var ast = try self.allocateAst(token.location, AstSpec{ .Float = .{ .value = token.data.float } });
                    return ast;
                },
                .BraceLeft => return self.parseBlock(),
                else => |kind| {
                    self.reportError(&token.location, "Unexpected token: {any}", .{kind});
                    return null;
                },
            }
        }
        return null;
    }
};
