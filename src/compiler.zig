const std = @import("std");

usingnamespace @import("common.zig");
usingnamespace @import("job.zig");
usingnamespace @import("location.zig");
usingnamespace @import("lexer.zig");
usingnamespace @import("ast.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("error_handler.zig");
usingnamespace @import("types.zig");
usingnamespace @import("symbol.zig");
usingnamespace @import("code_formatter.zig");
usingnamespace @import("code_runner.zig");
usingnamespace @import("dot_printer.zig");
usingnamespace @import("ring_buffer.zig");

pub const SourceFile = struct {
    path: StringBuf,
    content: StringBuf,

    const Self = @This();

    pub fn init(path: StringBuf, content: StringBuf) Self {
        return Self{
            .path = path,
            .content = content,
        };
    }

    pub fn deinit(self: *const Self) void {
        self.path.deinit();
        self.content.deinit();
    }
};

pub const Compiler = struct {
    allocator: *std.mem.Allocator,
    constantsAllocator: std.heap.ArenaAllocator,
    astAllocator: std.heap.ArenaAllocator,

    errorReporter: *ErrorReporter,
    errorMsgBuffer: std.ArrayList(u8),

    typeRegistry: TypeRegistry,

    files: List(*SourceFile),

    codeRunner: CodeRunner,
    globalScope: *SymbolTable,
    currentScope: *SymbolTable,

    allFibers: List(*FiberContext),
    fiberPool: List(*FiberContext),
    unqueuedJobs: RingBuffer(*Job),
    fiberQueue1: RingBuffer(*FiberContext),
    fiberQueue2: RingBuffer(*FiberContext),
    readyFibers: *RingBuffer(*FiberContext) = undefined,
    waitingFibers: *RingBuffer(*FiberContext) = undefined,

    const Self = @This();

    pub fn init(allocator: *std.mem.Allocator, errorReporter: *ErrorReporter) !*Self {
        var self = try allocator.create(Self);
        var globalScope = try allocator.create(SymbolTable);

        self.* = Self{
            .allocator = allocator,
            .constantsAllocator = std.heap.ArenaAllocator.init(allocator),
            .astAllocator = std.heap.ArenaAllocator.init(allocator),

            .errorReporter = errorReporter,
            .typeRegistry = try TypeRegistry.init(allocator),

            .files = List(*SourceFile).init(allocator),

            .codeRunner = try CodeRunner.init(allocator, errorReporter),
            .globalScope = globalScope,
            .currentScope = globalScope,

            .allFibers = List(*FiberContext).init(allocator),
            .fiberPool = List(*FiberContext).init(allocator),
            .unqueuedJobs = try RingBuffer(*Job).init(allocator),
            .fiberQueue1 = try RingBuffer(*FiberContext).init(allocator),
            .fiberQueue2 = try RingBuffer(*FiberContext).init(allocator),

            .errorMsgBuffer = std.ArrayList(u8).init(allocator),
        };

        globalScope.* = SymbolTable.init(null, &self.constantsAllocator.allocator, allocator);
        self.readyFibers = &self.fiberQueue1;
        self.waitingFibers = &self.fiberQueue2;

        return self;
    }

    pub fn deinit(self: *Self) void {
        self.codeRunner.deinit();
        self.typeRegistry.deinit();
        self.currentScope.deinit();
        self.allocator.destroy(self.globalScope);
        self.constantsAllocator.deinit();
        self.astAllocator.deinit();

        self.errorMsgBuffer.deinit();

        // deinit fibers
        for (self.allFibers.items) |fiber| {
            fiber.deinit();
        }
        self.allFibers.deinit();
        self.fiberPool.deinit();
        self.fiberQueue1.deinit();
        self.fiberQueue2.deinit();
        self.unqueuedJobs.deinit();

        for (self.files.items) |file| {
            file.deinit();
            self.allocator.destroy(file);
        }
        self.files.deinit();

        self.allocator.destroy(self);
    }

    pub fn reportError(self: *Self, location: *const Location, comptime format: []const u8, args: anytype) void {
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

    pub fn allocateSourceFile(self: *Self, path: StringBuf, content: StringBuf) !*SourceFile {
        var file = try self.allocator.create(SourceFile);
        file.* = SourceFile.init(path, content);
        try self.files.append(file);
        return file;
    }

    fn swapReadyAndWaitingQueue(self: *Self) void {
        const temp = self.readyFibers;
        self.readyFibers = self.waitingFibers;
        self.waitingFibers = temp;
    }

    fn getFreeFiber(self: *Self) !*FiberContext {
        if (self.fiberPool.items.len == 0) {
            const fiber = try FiberContext.init(self.allFibers.items.len, self.allocator);
            try self.allFibers.append(fiber);
            return fiber;
        } else {
            return self.fiberPool.pop();
        }
    }

    pub fn allocateAndAddJob(self: *Self, _job: anytype) !*Job {
        var job = try self.allocator.create(@TypeOf(_job));
        job.* = _job;
        try self.addJob(&job.job);
        return &job.job;
    }

    pub fn addJob(self: *Self, job: *Job) !void {
        try self.unqueuedJobs.push(job);
        std.log.debug("[addJob] {}, {}, {}", .{ self.unqueuedJobs.len(), self.readyFibers.len(), self.waitingFibers.len() });
    }

    pub fn run(self: *Self) !void {
        var madeProgress = false;

        while (true) {
            std.log.debug("[Main Loop] {}, {}, {}", .{ self.unqueuedJobs.len(), self.readyFibers.len(), self.waitingFibers.len() });
            var fiber: ?*FiberContext = null;
            if (self.readyFibers.len() == 0 and self.waitingFibers.len() > 0 and madeProgress) {
                std.log.debug("Move jobs from waiting queue to ready queue.", .{});
                self.swapReadyAndWaitingQueue();
                madeProgress = false;
            }
            if (self.readyFibers.pop()) |f| {
                std.log.debug("Take job from ready queue.", .{});
                fiber = f;
            } else if (self.unqueuedJobs.pop()) |job| {
                std.log.debug("Start next job.", .{});
                job.compiler = self;
                fiber = try self.getFreeFiber();
                fiber.?.job = job;
                fiber.?.madeProgress = true;
                fiber.?.wasCancelled = false;
                fiber.?.done = false;
                fiber.?.state = .Suspended;
            } else {
                std.log.debug("No more jobs left.", .{});
                break;
            }

            if (fiber) |f| {
                try f.step();
                madeProgress = madeProgress or f.madeProgress;
                if (f.done) {
                    f.job.?.free(self.allocator);
                    f.job = null;
                    try self.fiberPool.append(f);
                } else {
                    try self.waitingFibers.push(f);
                }
            }
        }

        std.debug.assert(self.unqueuedJobs.len() == 0);
        std.debug.assert(self.readyFibers.len() == 0);

        // Cancel all waiting jobs.
        std.log.debug("There are {} fibers still waiting.", .{self.waitingFibers.len()});
        var it = self.waitingFibers.iterator();
        while (it.next()) |f| {
            std.log.debug("Cancelling job.", .{});
            f.wasCancelled = true;
            try f.step();

            if (f.job) |j| {
                j.free(self.allocator);
            }
        }

        var it2 = self.unqueuedJobs.iterator();
        while (it2.next()) |job| {
            job.free(self.allocator);
        }
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

    pub fn compileAst(self: *Self, _ast: *Ast, injected: ?*Ast) anyerror!void {
        switch (_ast.spec) {
            .Block => try self.compileBlock(_ast, injected),
            .Call => try self.compileCall(_ast, injected),
            .Identifier => try self.compileIdentifier(_ast, injected),
            .Int => try self.compileInt(_ast, injected),
            .Pipe => try self.compilePipe(_ast, injected),
            .ConstDecl => try self.compileConstDecl(_ast, injected),
            .VarDecl => try self.compileVarDecl(_ast, injected),
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

        var subScope = SymbolTable.init(self.currentScope, &self.constantsAllocator.allocator, self.allocator);
        defer subScope.deinit();
        self.currentScope = &subScope;
        defer self.currentScope = subScope.parent.?;

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
            if (self.wasError(ast, arg)) {
                return;
            }
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
            return;
        } else if (std.mem.eql(u8, id.name, "false")) {
            ast.typ = try self.typeRegistry.getBoolType(1);
            return;
        }

        if (try self.currentScope.get(id.name, &ast.location)) |sym| {
            id.symbol = sym;

            switch (sym.kind) {
                .Constant => |*gv| {
                    ast.typ = gv.typ;
                },
                .GlobalVariable => |*gv| {
                    ast.typ = gv.typ;
                },
                //else => {
                //    return error.NotImplemented;
                //},
            }
        } else {
            self.reportError(&ast.location, "Unknown symbol '{s}'", .{id.name});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }
    }

    fn compileInt(self: *Self, ast: *Ast, injected: ?*Ast) anyerror!void {
        const int = &ast.spec.Int;
        std.log.debug("compileInt() {}", .{int.value});
        ast.typ = try self.typeRegistry.getIntType(16, false, null);
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

    fn compileConstDecl(self: *Self, ast: *Ast, injected: ?*Ast) anyerror!void {
        const decl = &ast.spec.ConstDecl;
        std.log.debug("compileConstDecl()", .{});

        if (injected) |_| {
            self.reportError(&ast.location, "Can't inject expressions into constant declaration", .{});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        var varType: ?*const Type = null;
        var varName: String = "";

        switch (decl.pattern.spec) {
            .Identifier => |*id| {
                varName = id.name;
            },
            else => {
                self.reportError(&decl.pattern.location, "Unsupported pattern in variable declaration.", .{});
                ast.typ = try self.typeRegistry.getErrorType();
                return;
            },
        }

        if (decl.typ) |typ| {
            try self.compileAst(typ, null);
            if (self.wasError(ast, typ)) {
                return;
            }
            if (!typ.typ.is(.Type)) {
                self.reportError(&typ.location, "Expected type, found '{any}'", .{typ.typ});
                ast.typ = try self.typeRegistry.getErrorType();
                return;
            }

            // @todo: use this as the type
        }

        try self.compileAst(decl.value, null);
        if (self.wasError(ast, decl.value)) {
            return;
        }
        varType = decl.value.typ;

        std.debug.assert(varType != null);

        std.log.debug("{any}", .{varType});
        var sym = self.currentScope.define(varName) catch |err| switch (err) {
            error.SymbolAlreadyExists => {
                self.reportError(&decl.pattern.location, "A symbol with name '{s}' already exists in the current scope", .{varName});
                ast.typ = try self.typeRegistry.getErrorType();
                return;
            },
            else => return err,
        };

        try self.codeRunner.runAst(decl.value);
        var value = try self.constantsAllocator.allocator.alloc(u8, varType.?.size);
        try self.codeRunner.popInto(value);

        sym.* = Symbol.init(SymbolKind{ .Constant = .{
            .decl = ast,
            .typ = varType.?,
            .value = value,
        } });

        decl.symbol = sym;
        ast.typ = try self.typeRegistry.getVoidType();
    }

    fn compileVarDecl(self: *Self, ast: *Ast, injected: ?*Ast) anyerror!void {
        const decl = &ast.spec.VarDecl;
        std.log.debug("compileVarDecl()", .{});

        if (injected) |_| {
            self.reportError(&ast.location, "Can't inject expressions into variable declaration", .{});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        var varType: ?*const Type = null;
        var varName: String = "";

        switch (decl.pattern.spec) {
            .Identifier => |*id| {
                varName = id.name;
            },
            else => {
                self.reportError(&decl.pattern.location, "Unsupported pattern in variable declaration.", .{});
                ast.typ = try self.typeRegistry.getErrorType();
                return;
            },
        }

        if (decl.typ) |typ| {
            try self.compileAst(typ, null);
            if (self.wasError(ast, typ)) {
                return;
            }
            if (!typ.typ.is(.Type)) {
                self.reportError(&typ.location, "Expected type, found '{any}'", .{typ.typ});
                ast.typ = try self.typeRegistry.getErrorType();
                return;
            }

            // @todo: use this as the type
        }

        if (decl.value) |value| {
            try self.compileAst(value, null);
            if (self.wasError(ast, value)) {
                return;
            }
            varType = value.typ;
        }

        std.debug.assert(varType != null);

        std.log.debug("{any}", .{varType});
        var sym = self.currentScope.define(varName) catch |err| switch (err) {
            error.SymbolAlreadyExists => {
                self.reportError(&decl.pattern.location, "A symbol with name '{s}' already exists in the current scope", .{varName});
                ast.typ = try self.typeRegistry.getErrorType();
                return;
            },
            else => return err,
        };
        sym.* = Symbol.init(SymbolKind{ .GlobalVariable = .{
            .decl = ast,
            .typ = varType.?,
        } });

        decl.symbol = sym;
        ast.typ = try self.typeRegistry.getVoidType();
    }
};
