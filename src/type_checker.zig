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
usingnamespace @import("compiler.zig");
usingnamespace @import("code_runner.zig");
usingnamespace @import("dot_printer.zig");
usingnamespace @import("ring_buffer.zig");

pub const TypeListener = struct {
    const Self = @This();

    notifyFn: fn (self: *Self, typ: *const Type) void,

    pub fn notify(self: *Self, typ: *const Type) void {
        return self.notifyFn(self, typ);
    }
};

pub const TypeChecker = struct {
    compiler: *Compiler,
    typeRegistry: *TypeRegistry,

    errorReporter: *ErrorReporter,
    errorMsgBuffer: std.ArrayList(u8),

    codeRunner: *CodeRunner,
    globalScope: *SymbolTable,
    currentScope: *SymbolTable,

    const Context = struct {
        injected: ?*Ast = null,
        expected: ?*const Type = null,
        typeListener: ?*TypeListener = null,
    };

    const Self = @This();

    pub fn init(compiler: *Compiler, codeRunner: *CodeRunner) !Self {
        return Self{
            .compiler = compiler,
            .typeRegistry = &compiler.typeRegistry,
            .codeRunner = codeRunner,
            .globalScope = compiler.globalScope,
            .currentScope = compiler.globalScope,
            .errorReporter = compiler.errorReporter,

            .errorMsgBuffer = std.ArrayList(u8).init(compiler.allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.errorMsgBuffer.deinit();
    }

    pub fn reportError(self: *Self, location: *const Location, comptime format: []const u8, args: anytype) void {
        self.errorMsgBuffer.resize(0) catch unreachable;
        std.fmt.format(self.errorMsgBuffer.writer(), format, args) catch {};
        self.compiler.errorReporter.report(self.errorMsgBuffer.items, location);
    }

    fn wasError(self: *Self, ast: *Ast, check: *Ast) bool {
        if (check.typ.is(.Error)) {
            ast.typ = check.typ;
            return true;
        }
        return false;
    }

    pub fn pushEnv(self: *Self) anyerror!*SymbolTable {
        var env = try self.compiler.constantsAllocator.allocator.create(SymbolTable);
        env.* = SymbolTable.init(
            self.currentScope,
            &self.compiler.constantsAllocator.allocator,
            &self.compiler.constantsAllocator.allocator,
        );
        self.currentScope = env;
        return env;
    }

    pub fn popEnv(self: *Self) void {
        self.currentScope = self.currentScope.parent.?;
    }

    pub fn compileAst(self: *Self, _ast: *Ast, ctx: Context) anyerror!void {
        switch (_ast.spec) {
            .Block => try self.compileBlock(_ast, ctx),
            .Call => try self.compileCall(_ast, ctx),
            .Identifier => try self.compileIdentifier(_ast, ctx),
            .Int => try self.compileInt(_ast, ctx),
            .Pipe => try self.compilePipe(_ast, ctx),
            .ConstDecl => try self.compileConstDecl(_ast, ctx),
            .VarDecl => try self.compileVarDecl(_ast, ctx),
            else => {
                const UnionTagType = @typeInfo(AstSpec).Union.tag_type.?;
                std.log.debug("compileAst({s}) Not implemented", .{@tagName(@as(UnionTagType, _ast.spec))});
                return error.NotImplemented;
            },
        }
    }

    fn compileBlock(self: *Self, ast: *Ast, ctx: Context) anyerror!void {
        const block = &ast.spec.Block;
        //std.log.debug("compileBlock()", .{});

        ast.typ = try self.typeRegistry.getVoidType();

        var subEnv = try self.pushEnv();
        defer self.popEnv();

        for (block.body.items) |expr| {
            try self.compileAst(expr, .{});
            if (self.wasError(ast, expr)) {
                return;
            }
            ast.typ = expr.typ;
        }
    }

    fn compileCall(self: *Self, ast: *Ast, ctx: Context) anyerror!void {
        //std.log.debug("compileCall()", .{});
        const call = &ast.spec.Call;
        switch (call.func.spec) {
            .Identifier => |*id| {
                if (id.name[0] == '@') {
                    if (std.mem.eql(u8, id.name, "@print")) {
                        try self.compileCallPrint(ast, ctx);
                        return;
                    } else if (std.mem.eql(u8, id.name, "@then")) {
                        try self.compileCallThen(ast, ctx);
                        return;
                    } else if (std.mem.eql(u8, id.name, "@repeat")) {
                        try self.compileCallRepeat(ast, ctx);
                        return;
                    } else if (std.mem.eql(u8, id.name, "@fn")) {
                        try self.compileCallFn(ast, ctx);
                        return;
                    } else if (id.name[0] == '@') {
                        self.reportError(&ast.location, "Unknown compiler function '{s}'", .{id.name});
                        ast.typ = try self.typeRegistry.getErrorType();
                        return;
                    }
                }
            },
            else => {},
        }

        try self.compileAst(call.func, .{});
        if (self.wasError(ast, call.func)) {
            return;
        }

        switch (call.func.typ.kind) {
            .Function => |func| try self.compileRegularFunctionCall(ast, call.func.typ, ctx),

            else => {
                self.reportError(&call.func.location, "Invalid type for call exression: {}", .{call.func.typ});
                ast.typ = try self.typeRegistry.getErrorType();
                return error.NotImplemented;
            },
        }
    }

    fn compileRegularFunctionCall(self: *Self, ast: *Ast, funcType: *const Type, ctx: Context) anyerror!void {
        var call = &ast.spec.Call;
        var func = &funcType.kind.Function;
        ast.typ = func.returnType;

        // Check arguments.
        if (call.args.items.len != func.params.items.len) {
            self.reportError(&ast.location, "Wrong number of arguments. Expected {}, got {}.", .{ func.params.items.len, call.args.items.len });
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        var i: usize = 0;
        while (i < call.args.items.len) : (i += 1) {
            const paramType = func.params.items[i];
            const arg = call.args.items[i];

            try self.compileAst(arg, .{ .expected = paramType });
            if (self.wasError(ast, arg)) {
                return;
            }

            // @todo: Check if arg has correct type.
        }
    }

    fn compileCallPrint(self: *Self, ast: *Ast, ctx: Context) anyerror!void {
        const call = &ast.spec.Call;
        //std.log.debug("compileCallPrint()", .{});

        for (call.args.items) |arg| {
            try self.compileAst(arg, .{});
            if (self.wasError(ast, arg)) {
                return;
            }
        }

        ast.typ = try self.typeRegistry.getVoidType();
    }

    fn compileCallThen(self: *Self, ast: *Ast, ctx: Context) anyerror!void {
        const call = &ast.spec.Call;
        //std.log.debug("compileCallThen()", .{});

        if (ctx.injected == null) {
            self.reportError(&ast.location, "Missing condition for @then(). Hint: Use the pipe operator (->)", .{});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        // compile and check condition
        try self.compileAst(ctx.injected.?, .{});
        if (self.wasError(ast, ctx.injected.?)) {
            return;
        }

        if (!ctx.injected.?.typ.is(.Bool)) {
            self.reportError(&ast.location, "Condition for @then() is not a bool but '{}'", .{ctx.injected.?.typ});
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
        try self.compileAst(call.args.items[0], .{});
        if (self.wasError(ast, call.args.items[0])) {
            return;
        }

        // Add injected expression as last argument so we have a pointer for running this ast.
        try call.args.append(ctx.injected.?);

        ast.typ = try self.typeRegistry.getVoidType();
    }

    fn compileCallRepeat(self: *Self, ast: *Ast, ctx: Context) anyerror!void {
        const call = &ast.spec.Call;
        //std.log.debug("compileCallRepeat()", .{});

        if (ctx.injected == null) {
            self.reportError(&ast.location, "Missing condition for @repeat(). Hint: Use the pipe operator (->)", .{});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        // compile and check condition
        try self.compileAst(ctx.injected.?, .{});
        if (self.wasError(ast, ctx.injected.?)) {
            return;
        }

        if (ctx.injected.?.typ.is(.Bool)) {
            // ok
        } else if (ctx.injected.?.typ.is(.Int)) {
            // ok
        } else {
            self.reportError(&ast.location, "Condition for @repeat() is not a bool or int but '{}'", .{ctx.injected.?.typ});
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
        try self.compileAst(call.args.items[0], .{});
        if (self.wasError(ast, call.args.items[0])) {
            return;
        }

        // Add injected expression as last argument so we have a pointer for running this ast.
        try call.args.append(ctx.injected.?);

        ast.typ = try self.typeRegistry.getVoidType();
    }

    fn compileCallFn(self: *Self, ast: *Ast, ctx: Context) anyerror!void {
        const call = &ast.spec.Call;
        //std.log.debug("compileCallFn()", .{});

        if (ctx.injected != null) {
            self.reportError(&ast.location, "Can't inject into function.", .{});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        var signature: ?*Ast = null;
        var body: ?*Ast = null;

        if (call.args.items.len > 2) {
            self.reportError(&ast.location, "Too many arguments for function declaration.", .{});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        if (call.args.items.len == 1) {
            var temp = call.args.items[0];
            if (temp.is(.Pipe)) {
                signature = call.args.pop();
            } else {
                body = call.args.pop();
            }
        } else if (call.args.items.len == 2) {
            body = call.args.pop();
            signature = call.args.pop();
        }

        if (body == null) {
            self.reportError(&ast.location, "Extern function declarations not implemented yet.", .{});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        var returnType = try self.typeRegistry.getVoidType();
        var paramTypes = try self.typeRegistry.allocTypeArray(0);

        var argsScope = try self.pushEnv();
        defer self.popEnv();

        if (signature) |sig| {
            if (!sig.is(.Pipe)) {
                self.reportError(&sig.location, "Function parameters must be of this pattern: (arg1: T1, arg2: T2, ...) -> T0", .{});
                ast.typ = try self.typeRegistry.getErrorType();
                return;
            }

            var paramsAst = sig.spec.Pipe.left;
            var returnTypeAst = sig.spec.Pipe.right;

            if (!paramsAst.is(.Tuple)) {
                self.reportError(&sig.location, "Function parameters must be of this pattern: (arg1: T1, arg2: T2, ...) -> T0", .{});
                ast.typ = try self.typeRegistry.getErrorType();
                return;
            }

            var paramsTuple = &paramsAst.spec.Tuple;

            // Compile parameters.
            var argOffset: usize = 0;
            for (paramsTuple.values.items) |param| {
                // Make sure param is a declaration.
                if (!param.is(.VarDecl)) {
                    self.reportError(&param.location, "Function parameter must be a declaration.", .{});
                    ast.typ = try self.typeRegistry.getErrorType();
                    return;
                }
                var paramDecl = &param.spec.VarDecl;

                // Make sure pattern is an identifier.
                if (!paramDecl.pattern.is(.Identifier)) {
                    self.reportError(&param.location, "Parameter pattern must be an identifier.", .{});
                    ast.typ = try self.typeRegistry.getErrorType();
                    return;
                }
                var paramName = &paramDecl.pattern.spec.Identifier;

                // Get type of parameter.
                try self.compileAst(paramDecl.typ.?, .{});
                if (self.wasError(ast, paramDecl.typ.?)) {
                    return;
                }
                if (!paramDecl.typ.?.typ.is(.Type)) {
                    self.reportError(&param.location, "Function parameter type is not a type: {}", .{paramDecl.typ.?.typ});
                    ast.typ = try self.typeRegistry.getErrorType();
                    return;
                }
                try self.codeRunner.runAst(paramDecl.typ.?);
                const paramType = try self.codeRunner.pop(*const Type);
                try paramTypes.append(paramType);

                if (try argsScope.define(paramName.name)) |sym| sym.kind = .{ .Argument = .{
                    .decl = param,
                    .typ = paramType,
                    .offset = argOffset,
                } } else {
                    self.reportError(&sig.location, "Parameter with name '{s}' already exists.", .{paramName.name});
                    ast.typ = try self.typeRegistry.getErrorType();
                    return;
                }

                argOffset = std.mem.alignForward(argOffset + paramType.size, paramType.alignment);
            }

            // Compile return type.
            try self.compileAst(returnTypeAst, .{});
            if (self.wasError(ast, returnTypeAst)) {
                return;
            }
            if (!returnTypeAst.typ.is(.Type)) {
                self.reportError(&sig.location, "Function return type is not a type: {}", .{returnTypeAst.typ});
                ast.typ = try self.typeRegistry.getErrorType();
                return;
            }
            try self.codeRunner.runAst(returnTypeAst);
            returnType = try self.codeRunner.pop(*const Type);
        }

        ast.typ = try self.typeRegistry.getFunctionType(paramTypes, returnType);
        if (ctx.typeListener) |listener| {
            listener.notify(ast.typ);
        }

        // call.args change type of ast to Function and use call.args for the actual args.
        var args = call.args;
        ast.spec = .{ .Function = .{
            .args = args,
            .body = body.?,
        } };
        try self.compileAst(body.?, .{});
    }

    fn compileIdentifier(self: *Self, ast: *Ast, ctx: Context) anyerror!void {
        const id = &ast.spec.Identifier;
        //std.log.debug("compileIdentifier() {s}", .{id.name});

        if (std.mem.eql(u8, id.name, "true")) {
            ast.typ = try self.typeRegistry.getBoolType(1);
            return;
        } else if (std.mem.eql(u8, id.name, "false")) {
            ast.typ = try self.typeRegistry.getBoolType(1);
            return;
        }

        if (try self.currentScope.get(id.name, &ast.location)) |sym| {
            id.symbol = sym;

            // Wait until symbol kind is set.
            {
                var condition = struct {
                    condition: FiberWaitCondition = .{
                        .evalFn = eval,
                        .reportErrorFn = reportError,
                    },
                    symbol: *Symbol,
                    location: *Location,

                    pub fn eval(condition: *FiberWaitCondition) bool {
                        const s = @fieldParentPtr(@This(), "condition", condition);
                        return !s.symbol.is(.NotSet);
                    }

                    pub fn reportError(condition: *FiberWaitCondition, compiler: *Compiler) void {
                        const s = @fieldParentPtr(@This(), "condition", condition);
                        compiler.reportError(s.location, "Unknown kind of symbol: {s}", .{s.symbol.name});
                    }
                }{
                    .symbol = sym,
                    .location = &ast.location,
                };
                try Coroutine.current().getUserData().?.waitUntil(&condition.condition);
            }

            switch (sym.kind) {
                .Argument => |*arg| {
                    ast.typ = arg.typ;
                },
                .Constant => |*gv| {
                    ast.typ = gv.typ;
                },
                .GlobalVariable => |*gv| {
                    ast.typ = gv.typ;
                },
                .NativeFunction => |*nf| {
                    ast.typ = nf.typ;
                },
                .Type => ast.typ = try self.typeRegistry.getTypeType(),
                else => return error.NotImplemented,
            }
        } else {
            self.reportError(&ast.location, "Unknown symbol '{s}'", .{id.name});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }
    }

    fn compileInt(self: *Self, ast: *Ast, ctx: Context) anyerror!void {
        const int = &ast.spec.Int;
        //std.log.debug("compileInt() {}", .{int.value});
        if (ctx.expected) |expected| switch (expected.kind) {
            .Int => {
                // @todo: Make sure the value fits in this size.
                ast.typ = expected;
            },
            else => ast.typ = try self.typeRegistry.getIntType(8, false, null),
        } else {
            ast.typ = try self.typeRegistry.getIntType(8, false, null);
        }
    }

    fn compilePipe(self: *Self, ast: *Ast, ctx: Context) anyerror!void {
        const pipe = &ast.spec.Pipe;
        //std.log.debug("compilePipe()", .{});

        if (ctx.injected) |_| {
            self.reportError(&ast.location, "Can't inject expressions into the pipe operator (->)", .{});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        }

        try self.compileAst(pipe.right, .{ .injected = pipe.left });
        ast.typ = pipe.right.typ;
    }

    fn compileConstDecl(self: *Self, ast: *Ast, ctx: Context) anyerror!void {
        const decl = &ast.spec.ConstDecl;
        //std.log.debug("compileConstDecl()", .{});

        if (ctx.injected) |_| {
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
            try self.compileAst(typ, .{});
            if (self.wasError(ast, typ)) {
                return;
            }
            if (!typ.typ.is(.Type)) {
                self.reportError(&typ.location, "Expected type, found '{any}'", .{typ.typ});
                ast.typ = try self.typeRegistry.getErrorType();
                return;
            }

            try self.codeRunner.runAst(typ);
            varType = try self.codeRunner.pop(*const Type);
        }

        var sym = if (try self.currentScope.define(varName)) |sym| sym else {
            self.reportError(&decl.pattern.location, "A symbol with name '{s}' already exists in the current scope", .{varName});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        };

        // Register type set listener.
        var onTypeSet = struct {
            listener: TypeListener = .{
                .notifyFn = notify,
            },
            ast: *Ast,
            symbol: *Symbol,

            pub fn notify(listener: *TypeListener, typ: *const Type) void {
                const s = @fieldParentPtr(@This(), "listener", listener);
                s.symbol.kind = .{ .Constant = .{
                    .decl = s.ast,
                    .typ = typ,
                    .value = null,
                } };
            }
        }{
            .ast = ast,
            .symbol = sym,
        };
        try self.compileAst(decl.value, .{ .expected = varType, .typeListener = &onTypeSet.listener });
        if (self.wasError(ast, decl.value)) {
            return;
        }
        varType = decl.value.typ;

        std.debug.assert(varType != null);

        try self.codeRunner.runAst(decl.value);
        var value = try self.compiler.constantsAllocator.allocator.alloc(u8, varType.?.size);
        try self.codeRunner.popInto(value);

        if (sym.is(.NotSet)) {
            sym.kind = .{ .Constant = .{
                .decl = ast,
                .typ = varType.?,
                .value = value,
            } };
        } else {
            sym.kind.Constant.value = value;
        }

        decl.symbol = sym;
        ast.typ = try self.typeRegistry.getVoidType();
    }

    fn compileVarDecl(self: *Self, ast: *Ast, ctx: Context) anyerror!void {
        const decl = &ast.spec.VarDecl;
        //std.log.debug("compileVarDecl()", .{});

        if (ctx.injected) |_| {
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
            try self.compileAst(typ, .{});
            if (self.wasError(ast, typ)) {
                return;
            }
            if (!typ.typ.is(.Type)) {
                self.reportError(&typ.location, "Expected type, found '{any}'", .{typ.typ});
                ast.typ = try self.typeRegistry.getErrorType();
                return;
            }

            try self.codeRunner.runAst(typ);
            varType = try self.codeRunner.pop(*const Type);
        }

        if (decl.value) |value| {
            try self.compileAst(value, .{});
            if (self.wasError(ast, value)) {
                return;
            }
            varType = value.typ;
        }

        std.debug.assert(varType != null);

        var sym = if (try self.currentScope.define(varName)) |sym| sym else {
            self.reportError(&decl.pattern.location, "A symbol with name '{s}' already exists in the current scope", .{varName});
            ast.typ = try self.typeRegistry.getErrorType();
            return;
        };
        sym.kind = .{ .GlobalVariable = .{
            .decl = ast,
            .typ = varType.?,
        } };

        decl.symbol = sym;
        ast.typ = try self.typeRegistry.getVoidType();
    }
};
