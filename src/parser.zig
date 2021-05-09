const std = @import("std");

usingnamespace @import("location.zig");
usingnamespace @import("lexer.zig");
usingnamespace @import("ast.zig");
usingnamespace @import("types.zig");
usingnamespace @import("error_handler.zig");

pub const Parser = struct {
    allocator: std.heap.ArenaAllocator,
    lexer: Lexer,
    errorReporter: *ErrorReporter,
    errorMsgBuffer: std.ArrayList(u8),
    nextId: usize = 0,

    const Self = @This();

    const Context = struct {
        allowLambda: bool = true,
        allowTuple: bool = true,

        pub fn with(self: Context, comptime field: []const u8, value: anytype) Context {
            var result = self;
            @field(result, field) = value;
            return result;
        }
    };

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
            .id = self.nextId,
            .location = location,
            .typ = UnknownType,
            .spec = spec,
        };
        self.nextId += 1;
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

    fn checkExpression(self: *Self, ctx: Context) bool {
        if (self.lexer.peek()) |token| {
            switch (token.kind) {
                .Identifier,
                .String,
                .Int,
                .Float,
                .ParenLeft,
                .BraceLeft,
                .BracketLeft,
                .Ampersand,
                .Plus,
                .Minus,
                => return true,
                .Bar => return ctx.allowTuple,
                else => return false,
            }
        }
        return false;
    }

    fn consume(self: *Self, kind: TokenKind, report: bool) ?Token {
        if (self.lexer.peek()) |token| {
            if (token.kind == kind) {
                _ = self.lexer.read();
                return token;
            } else {
                if (report) {
                    self.reportError(&token.location, "Expected {any}, found {any}", .{ kind, token.kind });
                    if (@errorReturnTrace()) |trace| {
                        std.debug.dumpStackTrace(trace.*);
                    } else {
                        @panic("consume");
                    }
                }
                return null;
            }
        }
        return null;
    }

    fn skipNewline(self: *Self) bool {
        if (self.lexer.peek()) |token| {
            if (token.kind == .Newline) {
                _ = self.lexer.read();
                return true;
            } else {
                return false;
            }
        }
        return false;
    }

    pub fn parseTopLevelExpression(self: *Self) anyerror!?*Ast {
        if (try self.parseExpression(.{})) |expr| {
            _ = self.consume(.Newline, true);
            return expr;
        }
        return null;
    }

    fn parseExpression(self: *Self, ctx: Context) anyerror!?*Ast {
        while (self.lexer.peek()) |token| {
            switch (token.kind) {
                else => |kind| {
                    var expr = (try self.parseAssignmentOrDeclOrLess(ctx)) orelse {
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

    fn parseAssignmentOrDeclOrLess(self: *Self, ctx: Context) anyerror!?*Ast {
        var expr = (try self.parseCommaOrLess(ctx)) orelse return null;

        if (self.lexer.peek()) |next| {
            switch (next.kind) {
                // _ =
                .Equal => {
                    self.skipToken();
                    _ = self.skipNewline();
                    var value = (try self.parseCommaOrLess(ctx)) orelse return null;
                    return try self.allocateAst(expr.location, AstSpec{ .Assignment = .{
                        .pattern = expr,
                        .value = value,
                    } });
                },

                // _ :
                .Colon => {
                    self.skipToken();
                    _ = self.skipNewline();

                    if (self.lexer.peek()) |next2| {
                        switch (next2.kind) {
                            // _ ::
                            .Colon => {
                                self.skipToken();
                                const value = (try self.parseCommaOrLess(ctx)) orelse return null;
                                return try self.allocateAst(expr.location, AstSpec{ .ConstDecl = .{
                                    .pattern = expr,
                                    .typ = null,
                                    .value = value,
                                } });
                            },

                            // _ :=
                            .Equal => {
                                self.skipToken();
                                const value = (try self.parseCommaOrLess(ctx)) orelse return null;
                                return try self.allocateAst(expr.location, AstSpec{ .VarDecl = .{
                                    .pattern = expr,
                                    .typ = null,
                                    .value = value,
                                } });
                            },

                            // _ : _
                            else => {
                                const typ = (try self.parseCommaOrLess(ctx)) orelse return null;

                                if (self.lexer.peek()) |next3| {
                                    switch (next3.kind) {
                                        // _ : _ :
                                        .Colon => {
                                            self.skipToken();
                                            _ = self.skipNewline();
                                            const value = (try self.parseCommaOrLess(ctx)) orelse return null;
                                            return try self.allocateAst(expr.location, AstSpec{ .ConstDecl = .{
                                                .pattern = expr,
                                                .typ = typ,
                                                .value = value,
                                            } });
                                        },

                                        // _ : _ =
                                        .Equal => {
                                            self.skipToken();
                                            _ = self.skipNewline();
                                            const value = (try self.parseCommaOrLess(ctx)) orelse return null;
                                            return try self.allocateAst(expr.location, AstSpec{ .VarDecl = .{
                                                .pattern = expr,
                                                .typ = typ,
                                                .value = value,
                                            } });
                                        },

                                        // _ : _
                                        .Newline => {
                                            return try self.allocateAst(expr.location, AstSpec{ .VarDecl = .{
                                                .pattern = expr,
                                                .typ = typ,
                                                .value = null,
                                            } });
                                        },

                                        else => {
                                            self.reportError(&next3.location, "Expected '=' or ':', found {}", .{next3.kind});
                                            return null;
                                        },
                                    }
                                }

                                // _ : _
                                return try self.allocateAst(expr.location, AstSpec{ .VarDecl = .{
                                    .pattern = expr,
                                    .typ = typ,
                                    .value = null,
                                } });
                            },
                        }
                    }
                    var value = (try self.parseCommaOrLess(ctx)) orelse return null;
                    return try self.allocateAst(expr.location, AstSpec{ .Assignment = .{
                        .pattern = expr,
                        .value = value,
                    } });
                },

                else => return expr,
            }
        }
        return expr;
    }

    fn parseCommaOrLess(self: *Self, ctx: Context) anyerror!?*Ast {
        var expr = (try self.parsePipeOrLess(ctx)) orelse return null;
        if (ctx.allowTuple and self.check(.Comma) != null) {
            var args = std.ArrayList(*Ast).init(&self.allocator.allocator);
            try args.append(expr);
            while (self.lexer.peek()) |next| {
                switch (next.kind) {
                    .Comma => {
                        self.skipToken();
                        _ = self.skipNewline();

                        if (self.checkExpression(ctx)) {
                            if (try self.parsePipeOrLess(ctx)) |e| {
                                try args.append(e);
                            }
                        } else {
                            break;
                        }
                    },
                    else => break,
                }
            }
            return try self.allocateAst(expr.location, AstSpec{ .Tuple = .{
                .values = args,
            } });
        }
        return expr;
    }

    fn parsePipeOrLess(self: *Self, ctx: Context) anyerror!?*Ast {
        var expr = (try self.parseCallOrLess(ctx)) orelse return null;
        while (self.lexer.peek()) |next| {
            switch (next.kind) {
                .Arrow => {
                    self.skipToken();
                    _ = self.skipNewline();
                    var right = (try self.parseCallOrLess(ctx)) orelse return null;
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

    pub fn parseCallOrLess(self: *Self, ctx: Context) anyerror!?*Ast {
        var expr = (try self.parseAtomic(ctx)) orelse return null;
        while (self.lexer.peek()) |next| {
            switch (next.kind) {
                .ParenLeft => {
                    expr = (try self.parseCall(expr, ctx)) orelse return expr;
                },
                .Period => {
                    self.skipToken();
                    const right = (try self.parseAtomic(ctx)) orelse return expr;
                    expr = try self.allocateAst(expr.location, AstSpec{ .Access = .{
                        .left = expr,
                        .right = right,
                    } });
                },
                else => return expr,
            }
        }
        return expr;
    }

    pub fn parseBlock(self: *Self, ctx: Context) anyerror!?*Ast {
        const braceLeft = self.consume(.BraceLeft, true) orelse return null;

        var body = std.ArrayList(*Ast).init(&self.allocator.allocator);
        errdefer body.deinit();

        _ = self.skipNewline();
        while (self.lexer.peek()) |token| {
            if (token.kind == .BraceRight) {
                break;
            }

            if (try self.parseExpression(ctx)) |expr| {
                try body.append(expr);

                if (self.check(.BraceRight) == null) {
                    _ = self.consume(.Newline, true);
                }
            } else {
                self.reportError(&braceLeft.location, "Expected expression.", .{});
                self.skipToken();
                _ = self.skipNewline();
            }
        }

        const braceRight = self.consume(.BraceRight, true) orelse return null;

        var ast = try self.allocateAst(braceLeft.location, AstSpec{ .Block = .{
            .body = body,
        } });
        return ast;
    }

    pub fn parseCall(self: *Self, func: *Ast, ctx: Context) anyerror!?*Ast {
        const parenLeft = self.consume(.ParenLeft, true) orelse return null;

        var args = std.ArrayList(*Ast).init(&self.allocator.allocator);
        errdefer args.deinit();

        var state: enum { AfterOpen, AfterExpr, AfterComma } = .AfterOpen;

        const argCtx = ctx.with("allowTuple", false);
        while (self.lexer.peek()) |token| {
            if (token.kind == .ParenRight) {
                break;
            }

            switch (state) {
                .AfterOpen => {
                    _ = self.skipNewline();
                    if (try self.parseExpression(argCtx)) |expr| {
                        try args.append(expr);
                    } else {
                        self.reportError(&parenLeft.location, "Expected argument.", .{});
                        self.skipToken();
                    }
                    state = .AfterExpr;
                },
                .AfterExpr => {
                    if (!self.skipNewline()) {
                        _ = self.consume(.Comma, true);
                    } else if (self.check(.ParenRight) == null) {
                        _ = self.consume(.Comma, false);
                    }
                    state = .AfterComma;
                },
                .AfterComma => {
                    _ = self.skipNewline();
                    if (try self.parseExpression(argCtx)) |expr| {
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
                .Bar,
                => {
                    if (token.kind != .Bar or ctx.allowLambda) {
                        var block = (try self.parsePipeOrLess(ctx)) orelse return null;
                        try args.append(block);
                    }
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

    pub fn parseLambda(self: *Self, ctx: Context) anyerror!?*Ast {
        const parenLeft = self.consume(.Bar, true) orelse return null;

        var args = std.ArrayList(*Ast).init(&self.allocator.allocator);
        errdefer args.deinit();

        var state: enum { AfterOpen, AfterExpr, AfterComma } = .AfterOpen;

        const argCtx = ctx.with("allowLambda", false).with("allowTuple", false);

        _ = self.skipNewline();
        while (self.lexer.peek()) |token| {
            if (token.kind == .Bar) {
                break;
            }

            switch (state) {
                .AfterOpen => {
                    _ = self.skipNewline();
                    if (try self.parseExpression(argCtx)) |expr| {
                        try args.append(expr);
                    } else {
                        self.reportError(&parenLeft.location, "Expected argument.", .{});
                        self.skipToken();
                    }
                    state = .AfterExpr;
                },
                .AfterExpr => {
                    if (!self.skipNewline()) {
                        _ = self.consume(.Comma, true);
                    } else if (self.check(.Bar) == null) {
                        _ = self.consume(.Comma, false);
                    }
                    state = .AfterComma;
                },
                .AfterComma => {
                    _ = self.skipNewline();
                    if (try self.parseExpression(argCtx)) |expr| {
                        try args.append(expr);
                    } else {
                        self.reportError(&parenLeft.location, "Expected argument.", .{});
                        self.skipToken();
                    }
                    state = .AfterExpr;
                },
            }
        }

        const parenRight = self.consume(.Bar, true) orelse return null;
        _ = self.skipNewline();

        var body = (try self.parseCallOrLess(ctx)) orelse return null;

        var ast = try self.allocateAst(parenLeft.location, AstSpec{ .Lambda = .{
            .body = body,
            .args = args,
        } });
        return ast;
    }

    pub fn parseAtomic(self: *Self, ctx: Context) anyerror!?*Ast {
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

                .ParenLeft => {
                    self.skipToken();
                    const expr = self.parseExpression(ctx.with("allowLambda", true).with("allowTuple", true));
                    _ = self.consume(.ParenRight, true);
                    return expr;
                },

                .BraceLeft => return self.parseBlock(ctx),

                .Bar => {
                    if (ctx.allowLambda) {
                        return self.parseLambda(ctx);
                    } else {
                        return null;
                    }
                },

                else => |kind| {
                    self.reportError(&token.location, "Unexpected token: {any}", .{kind});
                    if (@errorReturnTrace()) |trace| {
                        std.debug.dumpStackTrace(trace.*);
                    } else {
                        @panic("parseAtomic");
                    }
                    return null;
                },
            }
        }
        return null;
    }
};

fn parseOne(allocator: *std.mem.Allocator, input: []const u8) !?*Ast {
    var errorReporter = NullErrorReporter{};
    var parser = Parser.init(try Lexer.init(input), allocator, &errorReporter.reporter);
    errdefer parser.deinit();

    if (try parser.parseTopLevelExpression()) |expr| {
        std.testing.expect(parser.lexer.peek() == null);
        return expr;
    }
    return null;
}

////////////////////////////////////////////
//                 Tests                  //
////////////////////////////////////////////

test "identifier" {
    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();
    if (try parseOne(&allocator.allocator, "id")) |ast| {
        std.testing.expect(@as(std.meta.Tag(AstSpec), ast.spec) == .Identifier);
        std.testing.expectEqualStrings("id", ast.spec.Identifier.name);
    } else {
        std.testing.expect(false);
    }
}

test "string" {
    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();
    if (try parseOne(&allocator.allocator, "\"hello world,\nwassup?\"")) |ast| {
        std.testing.expect(@as(std.meta.Tag(AstSpec), ast.spec) == .String);
        std.testing.expectEqualStrings("hello world,\nwassup?", ast.spec.String.value);
    } else {
        std.testing.expect(false);
    }
}

test "number" {
    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();

    // decimal
    if (try parseOne(&allocator.allocator, "123")) |ast| {
        std.testing.expect(@as(std.meta.Tag(AstSpec), ast.spec) == .Int);
        std.testing.expectEqual(@as(u128, 123), ast.spec.Int.value);
    } else {
        std.testing.expect(false);
    }

    // hex
    if (try parseOne(&allocator.allocator, "0x123")) |ast| {
        std.testing.expect(@as(std.meta.Tag(AstSpec), ast.spec) == .Int);
        std.testing.expectEqual(@as(u128, 291), ast.spec.Int.value);
    } else {
        std.testing.expect(false);
    }

    // binary
    if (try parseOne(&allocator.allocator, "0b10110")) |ast| {
        std.testing.expect(@as(std.meta.Tag(AstSpec), ast.spec) == .Int);
        std.testing.expectEqual(@as(u128, 22), ast.spec.Int.value);
    } else {
        std.testing.expect(false);
    }
}

test "block" {
    var allocator = std.heap.ArenaAllocator.init(std.testing.allocator);
    defer allocator.deinit();

    // empty
    if (try parseOne(&allocator.allocator, "{}")) |ast| {
        std.testing.expect(@as(std.meta.Tag(AstSpec), ast.spec) == .Block);
        var block = &ast.spec.Block;
        std.testing.expectEqual(@as(u128, 0), block.body.items.len);
    } else {
        std.testing.expect(false);
    }

    // empty
    if (try parseOne(&allocator.allocator, "{\n}")) |ast| {
        std.testing.expect(@as(std.meta.Tag(AstSpec), ast.spec) == .Block);
        var block = &ast.spec.Block;
        std.testing.expectEqual(@as(u128, 0), block.body.items.len);
    } else {
        std.testing.expect(false);
    }

    // non empty
    if (try parseOne(&allocator.allocator, "{\n5\nalol\n}")) |ast| {
        std.testing.expect(@as(std.meta.Tag(AstSpec), ast.spec) == .Block);
        var block = &ast.spec.Block;
        std.testing.expectEqual(@as(u128, 2), block.body.items.len);

        std.testing.expect(@as(std.meta.Tag(AstSpec), block.body.items[0].spec) == .Int);
        std.testing.expectEqual(@as(u128, 5), block.body.items[0].spec.Int.value);

        std.testing.expect(@as(std.meta.Tag(AstSpec), block.body.items[1].spec) == .Identifier);
        std.testing.expectEqualStrings("lol", block.body.items[1].spec.Identifier.name);
    } else {
        std.testing.expect(false);
    }
}
