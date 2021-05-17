const std = @import("std");

usingnamespace @import("ast.zig");
usingnamespace @import("code_formatter.zig");
usingnamespace @import("common.zig");
usingnamespace @import("compiler.zig");
usingnamespace @import("dot_printer.zig");
usingnamespace @import("error_handler.zig");
usingnamespace @import("lexer.zig");
usingnamespace @import("location.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("job.zig");
usingnamespace @import("types.zig");
usingnamespace @import("symbol.zig");

pub const CodeRunner = struct {
    allocator: *std.mem.Allocator,

    errorReporter: *ErrorReporter,
    errorMsgBuffer: std.ArrayList(u8),

    // execution
    globalVariables: *std.mem.Allocator,
    stack: List(u8),
    stackPointer: usize = 0,
    basePointer: usize = 0,

    const Self = @This();

    pub fn init(compiler: *Compiler) !Self {
        var stack = List(u8).init(&compiler.stackAllocator.allocator);
        try stack.resize(4 * 1024 * 1024);
        return Self{
            .allocator = compiler.allocator,
            .errorReporter = compiler.errorReporter,
            .stack = stack,
            .globalVariables = &compiler.constantsAllocator.allocator,
            .errorMsgBuffer = std.ArrayList(u8).init(&compiler.stackAllocator.allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
        self.errorMsgBuffer.deinit();
    }

    fn reportError(self: *Self, location: *const Location, comptime format: []const u8, args: anytype) void {
        self.errorMsgBuffer.resize(0) catch unreachable;
        std.fmt.format(self.errorMsgBuffer.writer(), format, args) catch {};
        self.errorReporter.report(self.errorMsgBuffer.items, location);
    }

    pub fn push(self: *Self, value: anytype) !void {
        const size = @sizeOf(@TypeOf(value));
        std.mem.copy(u8, self.stack.items[self.stackPointer..], std.mem.asBytes(&value));
        self.stackPointer += size;
    }

    pub fn pushArg(self: *Self, offset: usize, size: usize) !void {
        const src = self.basePointer + offset;
        std.mem.copy(u8, self.stack.items[self.stackPointer..], self.stack.items[src..(src + size)]);
        self.stackPointer += size;
    }

    pub fn pushSlice(self: *Self, value: []u8) !void {
        std.mem.copy(u8, self.stack.items[self.stackPointer..], value);
        self.stackPointer += value.len;
    }

    pub fn pop(self: *Self, comptime T: type) !T {
        const size = @sizeOf(T);
        if (self.stackPointer < size) {
            return error.StackUnderflow;
        }
        self.stackPointer -= size;
        var value: T = undefined;
        std.mem.copy(u8, std.mem.asBytes(&value), self.stack.items[self.stackPointer..(self.stackPointer + size)]);
        return value;
    }

    pub fn popInto(self: *Self, dest: []u8) !void {
        const size = dest.len;
        if (self.stackPointer < size) {
            return error.StackUnderflow;
        }
        self.stackPointer -= size;
        std.mem.copy(u8, dest, self.stack.items[self.stackPointer..(self.stackPointer + size)]);
    }

    pub fn popBytes(self: *Self, size: usize) !void {
        if (self.stackPointer < size) {
            return error.StackUnderflow;
        }
        self.stackPointer -= size;
    }

    pub fn runAst(self: *Self, ast: *Ast) anyerror!void {
        switch (ast.spec) {
            .Block => try self.runBlock(ast),
            .Call => try self.runCall(ast),
            .Identifier => try self.runIdentifier(ast),
            .Int => |*int| {
                try self.push(int.value);
            },
            .Function => try self.runFunction(ast),
            .Pipe => try self.runPipe(ast),
            .ConstDecl => {},
            .VarDecl => try self.runVarDecl(ast),
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
                        return;
                    } else if (std.mem.eql(u8, id.name, "@then")) {
                        try self.runCallThen(ast);
                        return;
                    } else if (std.mem.eql(u8, id.name, "@repeat")) {
                        try self.runCallRepeat(ast);
                        return;
                    } else if (id.name[0] == '@') {
                        std.log.debug("runCall({s}) Not implemented", .{id.name});
                        return error.NotImplemented;
                    }
                }
            },
            else => {},
        }

        // regular call

        // Get the function ast.
        std.log.debug("Get the function.", .{});
        try self.runAst(call.func);
        const func = try self.pop(*Ast);
        std.debug.assert(func.is(.Function));

        // Stack frame + arguments.
        std.log.debug("Stack frame + arguments.", .{});
        try self.push(self.basePointer);
        self.basePointer = self.stackPointer;
        const stackPointerOld = self.stackPointer;
        for (call.args.items) |arg| {
            try self.runAst(arg);
        }

        std.log.debug("Run the function.", .{});
        try self.runAst(func.spec.Function.body);

        std.log.debug("After call, restore base pointer.", .{});
        self.stackPointer = stackPointerOld;
        self.basePointer = try self.pop(@TypeOf(self.basePointer));
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

    fn runFunction(self: *Self, ast: *Ast) anyerror!void {
        //std.log.debug("runFunction()", .{});
        const call = &ast.spec.Function;
        try self.push(ast);
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

    fn runIdentifier(self: *Self, ast: *Ast) anyerror!void {
        //std.log.debug("runIdentifier()", .{});
        const id = &ast.spec.Identifier;
        if (std.mem.eql(u8, id.name, "true")) {
            try self.push(true);
        } else if (std.mem.eql(u8, id.name, "false")) {
            try self.push(false);
        } else {
            std.debug.assert(id.symbol != null);

            switch (id.symbol.?.kind) {
                .Argument => |*arg| {
                    const offset = arg.offset;
                    std.log.debug("Loading argument at index {}", .{offset});
                    //try self.pushSlice(gv.value.?);
                    try self.pushArg(offset, arg.typ.size);
                },
                .Constant => |*constant| {
                    // Wait until value is known.
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
                                return s.symbol.kind.Constant.value != null;
                            }

                            pub fn reportError(condition: *FiberWaitCondition, compiler: *Compiler) void {
                                const s = @fieldParentPtr(@This(), "condition", condition);
                                compiler.reportError(s.location, "Value of symbol not known yet: {s}", .{s.symbol.name});
                            }
                        }{
                            .symbol = id.symbol.?,
                            .location = &ast.location,
                        };
                        try Coroutine.current().getUserData().?.waitUntil(&condition.condition);
                    }
                    try self.pushSlice(constant.value.?);
                },
                .GlobalVariable => |*gv| {
                    if (gv.value == null) {
                        // @todo: wait
                        self.reportError(&ast.location, "Trying to evaluate an identifier at compile time but the value is not known yet.", .{});
                        self.reportError(&gv.decl.location, "Variable defined here.", .{});
                        return error.FailedToRunCode;
                    }
                    try self.pushSlice(gv.value.?);
                },
                .Type => |*typ| {
                    try self.push(typ.typ);
                },
                else => return error.NotImplemented,
            }
        }
    }

    fn runPipe(self: *Self, ast: *Ast) anyerror!void {
        //std.log.debug("runPipe()", .{});
        const pipe = &ast.spec.Pipe;
        try self.runAst(pipe.right);
    }

    fn runVarDecl(self: *Self, ast: *Ast) anyerror!void {
        //std.log.debug("runVarDecl()", .{});
        const decl = &ast.spec.VarDecl;
        std.debug.assert(decl.symbol != null);

        const symbol = decl.symbol.?;
        const globalVariable = &symbol.kind.GlobalVariable;

        // @todo: alignment
        globalVariable.value = try self.globalVariables.alloc(u8, globalVariable.typ.size);

        if (decl.value) |value| {
            try self.runAst(value);
            try self.popInto(globalVariable.value.?);
        }
    }
};
