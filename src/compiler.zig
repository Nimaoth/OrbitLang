const std = @import("std");

usingnamespace @import("common.zig");
usingnamespace @import("lexer.zig");
usingnamespace @import("ast.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("error_handler.zig");
usingnamespace @import("types.zig");
usingnamespace @import("code_formatter.zig");
usingnamespace @import("dot_printer.zig");

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
    typeRegistry: TypeRegistry,

    files: List(SourceFile),

    // execution
    stack: List(u8),
    stackPointer: usize = 0,

    const Self = @This();

    pub fn init(allocator: *std.mem.Allocator, errorReporter: *ErrorReporter) !Self {
        var stack = List(u8).init(allocator);
        try stack.resize(4048 * 1024);
        return Self{
            .allocator = allocator,
            .errorReporter = errorReporter,
            .typeRegistry = try TypeRegistry.init(allocator),

            .files = List(SourceFile).init(allocator),

            .stack = stack,
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
        self.files.deinit();
        self.typeRegistry.deinit();
    }

    pub fn compileAndRunFile(self: *Self, path: String) anyerror!void {
        const fileContent = try std.fs.cwd().readFileAlloc(self.allocator, path, std.math.maxInt(usize));
        defer self.allocator.free(fileContent);

        var lexer = try Lexer.init(fileContent);
        var parser = Parser.init(lexer, self.allocator, self.errorReporter);
        defer parser.deinit();

        var newFileName = StringBuf.init(self.allocator);
        try std.fmt.format(newFileName.writer(), "{s}.gv", .{path});
        defer newFileName.deinit();

        var graphFile = try std.fs.cwd().createFile(newFileName.items, .{});
        defer graphFile.close();

        var dotPrinter = try DotPrinter.init(graphFile.writer(), true);
        defer dotPrinter.deinit(graphFile.writer());

        while (try parser.parseTopLevelExpression()) |ast| {
            try self.compileAst(ast);
            try dotPrinter.printGraph(graphFile.writer(), ast);
            try self.runAst(ast);
        }
    }

    fn compileAst(self: *Self, _ast: *Ast) anyerror!void {
        switch (_ast.spec) {
            .Int => |*int| {
                std.log.debug("compileAst(int) {}", .{int.value});
                _ast.typ = try self.typeRegistry.getIntType(8, false, null);
            },
            .Identifier => |*id| {
                std.log.debug("compileAst(id) {s}", .{id.name});

                if (std.mem.eql(u8, id.name, "true")) {
                    _ast.typ = try self.typeRegistry.getBoolType(1);
                } else if (std.mem.eql(u8, id.name, "false")) {
                    _ast.typ = try self.typeRegistry.getBoolType(1);
                } else {
                    _ast.typ = try self.typeRegistry.getVoidType();
                }
            },
            .Call => |*ast| {
                switch (ast.func.spec) {
                    .Identifier => |*id| {
                        if (id.name[0] == '@') {
                            if (std.mem.eql(u8, id.name, "@print")) {
                                std.log.debug("compileAst(Call)", .{});

                                for (ast.args.items) |arg| {
                                    try self.compileAst(arg);
                                }

                                _ast.typ = try self.typeRegistry.getVoidType();
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
            .Int => |*int| {
                try self.push(int.value);
            },
            .Identifier => |*id| {
                if (std.mem.eql(u8, id.name, "true")) {
                    try self.push(true);
                } else if (std.mem.eql(u8, id.name, "false")) {
                    try self.push(false);
                }
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
