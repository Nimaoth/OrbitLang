const std = @import("std");

usingnamespace @import("common.zig");
usingnamespace @import("location.zig");
usingnamespace @import("lexer.zig");
usingnamespace @import("ast.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("error_handler.zig");
usingnamespace @import("types.zig");
usingnamespace @import("code_formatter.zig");
usingnamespace @import("code_runner.zig");
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
    errorMsgBuffer: std.ArrayList(u8),

    typeRegistry: TypeRegistry,

    files: List(SourceFile),

    codeRunner: CodeRunner,

    const Self = @This();

    pub fn init(allocator: *std.mem.Allocator, errorReporter: *ErrorReporter) !Self {
        return Self{
            .allocator = allocator,
            .errorReporter = errorReporter,
            .typeRegistry = try TypeRegistry.init(allocator),

            .files = List(SourceFile).init(allocator),

            .codeRunner = try CodeRunner.init(allocator, errorReporter),

            .errorMsgBuffer = std.ArrayList(u8).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.codeRunner.deinit();
        self.files.deinit();
        self.typeRegistry.deinit();
        self.errorMsgBuffer.deinit();
    }

    fn reportError(self: *Self, location: *const Location, comptime format: []const u8, args: anytype) void {
        self.errorMsgBuffer.resize(0) catch unreachable;
        std.fmt.format(self.errorMsgBuffer.writer(), format, args) catch {};
        self.errorReporter.report(self.errorMsgBuffer.items, location);
    }

    fn wasError(self: *Self, ast: *Ast, check: *Ast) bool {
        if (check.typ.is(.Error)) {
            ast.typ = check.typ;
            return true;
        }
        return false;
    }

    pub fn compileAndRunFile(self: *Self, path: String) anyerror!void {
        const fileContent = try std.fs.cwd().readFileAlloc(self.allocator, path, std.math.maxInt(usize));
        defer self.allocator.free(fileContent);

        const fullPath = try std.fs.cwd().realpathAlloc(self.allocator, path);
        defer self.allocator.free(fullPath);

        var newFileName = StringBuf.init(self.allocator);
        try std.fmt.format(newFileName.writer(), "{s}.gv", .{path});
        defer newFileName.deinit();

        var graphFile = try std.fs.cwd().createFile(newFileName.items, .{});
        defer graphFile.close();

        var dotPrinter = try DotPrinter.init(graphFile.writer(), true);
        defer dotPrinter.deinit(graphFile.writer());

        var lexer = try Lexer.init(fullPath, fileContent);
        var parser = Parser.init(lexer, self.allocator, self.errorReporter);
        defer parser.deinit();
        while (try parser.parseTopLevelExpression()) |ast| {
            try self.compileAst(ast, null);
            try dotPrinter.printGraph(graphFile.writer(), ast);

            if (ast.typ.is(.Error) or ast.typ.is(.Unknown)) {
                const location = &ast.location;
                std.log.debug("{s}:{}:{}: Failed to compile top level expr", .{ location.file, location.line, location.column });
            } else {
                try self.codeRunner.runAst(ast);
            }
        }
    }

    fn compileAst(self: *Self, _ast: *Ast, injected: ?*Ast) anyerror!void {
        switch (_ast.spec) {
            .Block => try self.compileBlock(_ast, injected),
            .Call => try self.compileCall(_ast, injected),
            .Identifier => try self.compileIdentifier(_ast, injected),
            .Int => try self.compileInt(_ast, injected),
            .Pipe => try self.compilePipe(_ast, injected),
            else => {
                const UnionTagType = @typeInfo(AstSpec).Union.tag_type.?;
                std.log.debug("compileAst({s}) Not implemented", .{@tagName(@as(UnionTagType, _ast.spec))});
                return error.NotImplemented;
            },
        }
    }

    fn compileBlock(self: *Self, ast: *Ast, injected: ?*Ast) anyerror!void {
        const block = &ast.spec.Block;
        std.log.debug("compileBlock()", .{});

        ast.typ = try self.typeRegistry.getVoidType();

        for (block.body.items) |expr| {
            try self.compileAst(expr, null);
            if (self.wasError(ast, expr)) {
                return;
            }
            ast.typ = expr.typ;
        }
    }

    fn compileCall(self: *Self, ast: *Ast, injected: ?*Ast) anyerror!void {
        const call = &ast.spec.Call;
        std.log.debug("compileCall()", .{});
        switch (call.func.spec) {
            .Identifier => |*id| {
                if (id.name[0] == '@') {
                    if (std.mem.eql(u8, id.name, "@print")) {
                        try self.compileCallPrint(ast, injected);
                    } else if (std.mem.eql(u8, id.name, "@then")) {
                        try self.compileCallThen(ast, injected);
                    } else if (std.mem.eql(u8, id.name, "@repeat")) {
                        try self.compileCallRepeat(ast, injected);
                    } else {
                        self.reportError(&ast.location, "Unknown compiler function '{s}'", .{id.name});
                        ast.typ = try self.typeRegistry.getErrorType();
                    }
                }
            },
            else => {
                self.reportError(&ast.location, "not implemented", .{});
                ast.typ = try self.typeRegistry.getErrorType();
                return error.NotImplemented;
            },
        }
    }

    fn compileCallPrint(self: *Self, ast: *Ast, injected: ?*Ast) anyerror!void {
        const call = &ast.spec.Call;
        std.log.debug("compileCallPrint()", .{});

        for (call.args.items) |arg| {
            try self.compileAst(arg, null);
        }

        ast.typ = try self.typeRegistry.getVoidType();
    }

    fn compileCallThen(self: *Self, ast: *Ast, injected: ?*Ast) anyerror!void {
        const call = &ast.spec.Call;
        std.log.debug("compileCallThen()", .{});

        if (injected == null) {
            self.reportError(&ast.location, "Missing condition for @then(). Hint: Use the pipe operator (->)", .{});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        // compile and check condition
        try self.compileAst(injected.?, null);
        if (self.wasError(ast, injected.?)) {
            return;
        }

        if (!injected.?.typ.is(.Bool)) {
            self.reportError(&ast.location, "Condition for @then() is not a bool but '{}'", .{injected.?.typ});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        // check number of arguments
        if (call.args.items.len != 1) {
            self.reportError(&ast.location, "Wrong number of arguments for @then(): expected exactly one argument but found {}", .{call.args.items.len});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        // compile body
        try self.compileAst(call.args.items[0], null);
        if (self.wasError(ast, call.args.items[0])) {
            return;
        }

        // Add injected expression as last argument so we have a pointer for running this ast.
        try call.args.append(injected.?);

        ast.typ = try self.typeRegistry.getVoidType();
    }

    fn compileCallRepeat(self: *Self, ast: *Ast, injected: ?*Ast) anyerror!void {
        const call = &ast.spec.Call;
        std.log.debug("compileCallRepeat()", .{});

        if (injected == null) {
            self.reportError(&ast.location, "Missing condition for @repeat(). Hint: Use the pipe operator (->)", .{});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        // compile and check condition
        try self.compileAst(injected.?, null);
        if (self.wasError(ast, injected.?)) {
            return;
        }

        if (injected.?.typ.is(.Bool)) {
            // ok
        } else if (injected.?.typ.is(.Int)) {
            // ok
        } else {
            self.reportError(&ast.location, "Condition for @repeat() is not a bool or int but '{}'", .{injected.?.typ});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        // check number of arguments
        if (call.args.items.len != 1) {
            self.reportError(&ast.location, "Wrong number of arguments for @repeat(): expected exactly one argument but found {}", .{call.args.items.len});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        // compile body
        try self.compileAst(call.args.items[0], null);
        if (self.wasError(ast, call.args.items[0])) {
            return;
        }

        // Add injected expression as last argument so we have a pointer for running this ast.
        try call.args.append(injected.?);

        ast.typ = try self.typeRegistry.getVoidType();
    }

    fn compileIdentifier(self: *Self, ast: *Ast, injected: ?*Ast) anyerror!void {
        const id = &ast.spec.Identifier;
        std.log.debug("compileIdentifier(id) {s}", .{id.name});

        if (std.mem.eql(u8, id.name, "true")) {
            ast.typ = try self.typeRegistry.getBoolType(1);
        } else if (std.mem.eql(u8, id.name, "false")) {
            ast.typ = try self.typeRegistry.getBoolType(1);
        } else {
            ast.typ = try self.typeRegistry.getVoidType();
        }
    }

    fn compileInt(self: *Self, ast: *Ast, injected: ?*Ast) anyerror!void {
        const int = &ast.spec.Int;
        std.log.debug("compileInt() {}", .{int.value});
        ast.typ = try self.typeRegistry.getIntType(8, false, null);
    }

    fn compilePipe(self: *Self, ast: *Ast, injected: ?*Ast) anyerror!void {
        const pipe = &ast.spec.Pipe;
        std.log.debug("compilePipe()", .{});

        if (injected) |_| {
            self.reportError(&ast.location, "Can't inject expressions into the pipe operator (->)", .{});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        try self.compileAst(pipe.right, pipe.left);
        ast.typ = pipe.right.typ;
    }
};
