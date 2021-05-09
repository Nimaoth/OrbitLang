const std = @import("std");

usingnamespace @import("common.zig");
usingnamespace @import("lexer.zig");
usingnamespace @import("ast.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("error_handler.zig");
usingnamespace @import("types.zig");
usingnamespace @import("code_formatter.zig");
usingnamespace @import("dot_printer.zig");

pub const CodeRunner = struct {
    allocator: *std.mem.Allocator,
    errorReporter: *ErrorReporter,

    // execution
    stack: List(u8),
    stackPointer: usize = 0,

    const Self = @This();

    pub fn init(allocator: *std.mem.Allocator, errorReporter: *ErrorReporter) !Self {
        var stack = List(u8).init(allocator);
        try stack.resize(4 * 1024 * 1024);
        return Self{
            .allocator = allocator,
            .errorReporter = errorReporter,
            .stack = stack,
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
    }

    fn push(self: *Self, value: anytype) !void {
        const size = @sizeOf(@TypeOf(value));
        std.mem.copy(u8, self.stack.items[self.stackPointer..], std.mem.asBytes(&value));
        self.stackPointer += size;
    }

    fn pop(self: *Self, comptime T: type) !T {
        const size = @sizeOf(T);
        if (self.stackPointer < size) {
            return error.StackUnderflow;
        }
        self.stackPointer -= size;
        var value: T = undefined;
        std.mem.copy(u8, std.mem.asBytes(&value), self.stack.items[self.stackPointer..(self.stackPointer + size)]);
        return value;
    }

    fn popBytes(self: *Self, size: usize) !void {
        if (self.stackPointer < size) {
            return error.StackUnderflow;
        }
        self.stackPointer -= size;
    }

    pub fn runAst(self: *Self, ast: *Ast) anyerror!void {
        switch (ast.spec) {
            .Block => try self.runBlock(ast),
            .Call => try self.runCall(ast),
            .Identifier => |*id| {
                if (std.mem.eql(u8, id.name, "true")) {
                    try self.push(true);
                } else if (std.mem.eql(u8, id.name, "false")) {
                    try self.push(false);
                }
            },
            .Int => |*int| {
                try self.push(int.value);
            },
            .Pipe => try self.runPipe(ast),
            else => {
                const UnionTagType = @typeInfo(AstSpec).Union.tag_type.?;
                std.log.debug("runAst({s}) Not implemented", .{@tagName(@as(UnionTagType, ast.spec))});
                return error.NotImplemented;
            },
        }
    }

    fn runBlock(self: *Self, ast: *Ast) anyerror!void {
        const block = &ast.spec.Block;
        for (block.body.items) |expr, i| {
            try self.runAst(expr);

            if (i < block.body.items.len - 1) {
                try self.popBytes(expr.typ.size);
            }
        }
    }

    fn runCall(self: *Self, ast: *Ast) anyerror!void {
        const call = &ast.spec.Call;
        switch (call.func.spec) {
            .Identifier => |*id| {
                if (id.name[0] == '@') {
                    if (std.mem.eql(u8, id.name, "@print")) {
                        try self.runCallPrint(ast);
                    } else if (std.mem.eql(u8, id.name, "@then")) {
                        try self.runCallThen(ast);
                    } else if (std.mem.eql(u8, id.name, "@repeat")) {
                        try self.runCallRepeat(ast);
                    } else {
                        return error.NotImplemented;
                    }
                }
            },
            else => return error.NotImplemented,
        }
    }

    fn runCallThen(self: *Self, ast: *Ast) anyerror!void {
        //std.log.debug("runCallThen()", .{});
        const call = &ast.spec.Call;

        const condition = call.args.items[1];
        const body = call.args.items[0];

        try self.runAst(condition);
        if (try self.pop(bool)) {
            try self.runAst(body);
        }
    }

    fn runCallRepeat(self: *Self, ast: *Ast) anyerror!void {
        //std.log.debug("runCallRepeat()", .{});
        const call = &ast.spec.Call;

        const condition = call.args.items[1];
        const body = call.args.items[0];

        if (condition.typ.is(.Int)) {
            var i: u128 = 0;
            try self.runAst(condition);
            const count = try self.pop(u128);
            while (i < count) : (i += 1) {
                try self.runAst(body);
            }
        } else if (condition.typ.is(.Bool)) {
            while (true) {
                try self.runAst(condition);
                if (!(try self.pop(bool))) {
                    break;
                }
                try self.runAst(body);
            }
        } else {
            @panic("Not implemented");
        }
    }

    fn runCallPrint(self: *Self, ast: *Ast) anyerror!void {
        //std.log.debug("runCallPrint()", .{});
        const call = &ast.spec.Call;

        for (call.args.items) |arg, i| {
            if (i > 0) {
                std.debug.print(" ", .{});
            }
            try self.runAst(arg);

            switch (arg.typ.kind) {
                .Int => |*int| {
                    std.debug.print("{}", .{try self.pop(u128)});
                },
                .Bool => {
                    std.debug.print("{}", .{try self.pop(bool)});
                },
                else => {
                    std.debug.print("<unknown>", .{});
                },
            }
        }
        std.debug.print("\n", .{});
    }

    fn runPipe(self: *Self, ast: *Ast) anyerror!void {
        const pipe = &ast.spec.Pipe;
        try self.runAst(pipe.right);
    }
};
