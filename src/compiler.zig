const std = @import("std");

usingnamespace @import("lexer.zig");
usingnamespace @import("ast.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("error_handler.zig");
usingnamespace @import("code_formatter.zig");

pub const String = []const u8;
pub const StringBuf = std.ArrayList(u8);
pub fn List(comptime T: type) type {
    return std.ArrayList(T);
}

pub const SourceFile = struct {
    path: String,

    const Self = @This();

    pub fn init(path: String) !Self {
        return Self{
            .path = path,
        };
    }
};

pub const Compiler = struct {
    allocator: *std.mem.Allocator,
    errorReporter: *ErrorReporter,
    files: List(SourceFile),
    stack: List(u8),
    stackPointer: usize = 0,

    const Self = @This();

    pub fn init(allocator: *std.mem.Allocator, errorReporter: *ErrorReporter) !Self {
        var stack = List(u8).init(allocator);
        try stack.resize(4048 * 1024);
        return Self{
            .allocator = allocator,
            .errorReporter = errorReporter,
            .files = List(SourceFile).init(allocator),
            .stack = stack,
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
        self.files.deinit();
    }

    pub fn compileAndRunFile(self: *Self, path: String) anyerror!void {
        const fileContent = try std.fs.cwd().readFileAlloc(self.allocator, path, std.math.maxInt(usize));
        defer self.allocator.free(fileContent);

        var lexer = try Lexer.init(fileContent);
        var parser = Parser.init(lexer, self.allocator, self.errorReporter);
        defer parser.deinit();

        while (try parser.parseTopLevelExpression()) |ast| {
            try self.compileAst(ast);
            try self.runAst(ast);
        }
    }

    fn compileAst(self: *Self, _ast: *Ast) anyerror!void {
        switch (_ast.spec) {
            .Int => |*ast| {
                std.log.debug("compileAst(int) {}", .{ast.value});
            },
            .Call => |*ast| {
                switch (ast.func.spec) {
                    .Identifier => |*id| {
                        if (id.name[0] == '@') {
                            if (std.mem.eql(u8, id.name, "@print")) {
                                std.log.debug("compileAst(Call)", .{});
                            }
                        }
                    },
                    else => {},
                }
            },
            else => {
                const UnionTagType = @typeInfo(AstSpec).Union.tag_type.?;
                std.log.debug("compileAst({s}) Not implemented", .{@tagName(@as(UnionTagType, _ast.spec))});
                return error.NotImplemented;
            },
        }
    }

    fn runAst(self: *Self, _ast: *Ast) anyerror!void {
        switch (_ast.spec) {
            .Int => |*ast| {
                try self.push(ast.value);
                std.log.debug("runAst(int) {}", .{ast.value});
            },
            .Call => |*ast| {
                switch (ast.func.spec) {
                    .Identifier => |*id| {
                        if (id.name[0] == '@') {
                            if (std.mem.eql(u8, id.name, "@print")) {
                                try self.runCallPrint(_ast);
                            }
                        }
                    },
                    else => {},
                }
            },
            else => {
                const UnionTagType = @typeInfo(AstSpec).Union.tag_type.?;
                std.log.debug("runAst({s}) Not implemented", .{@tagName(@as(UnionTagType, _ast.spec))});
                return error.NotImplemented;
            },
        }
    }

    fn runCallPrint(self: *Self, ast: *Ast) anyerror!void {
        std.log.debug("runCallPrint(Call)", .{});
        const call = &ast.spec.Call;

        for (call.args.items) |arg| {
            try self.runAst(arg);
            const value = try self.pop(u128);
            std.debug.print("{}\n", .{value});
        }
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
};
