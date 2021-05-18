const std = @import("std");
const term = @import("ansi-term");

usingnamespace @import("ast.zig");
usingnamespace @import("code_formatter.zig");
usingnamespace @import("common.zig");
usingnamespace @import("compiler.zig");
usingnamespace @import("dot_printer.zig");
usingnamespace @import("error_handler.zig");
usingnamespace @import("job.zig");
usingnamespace @import("lexer.zig");
usingnamespace @import("location.zig");
usingnamespace @import("native_function.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("symbol.zig");
usingnamespace @import("types.zig");

const DEBUG_LOG_STACK = false;

const NativeOrAstFunction = struct {
    ptr: usize,

    const Self = @This();
    const mask: usize = 1 << 63;

    pub fn get(self: Self) union(enum) {
        Ast: *Ast,
        Native: *NativeFunctionWrapper,
    } {
        // Check if leftmost bit is set.
        if ((self.ptr & mask) != 0) {
            return .{ .Ast = @intToPtr(*Ast, self.ptr & (~mask)) };
        } else {
            return .{ .Native = @intToPtr(*NativeFunctionWrapper, self.ptr) };
        }
    }

    pub fn fromAst(ast: *Ast) Self {
        std.debug.assert(@ptrToInt(ast) & mask == 0);
        return .{ .ptr = @ptrToInt(ast) | mask };
    }

    pub fn fromNative(nfw: *NativeFunctionWrapper) Self {
        std.debug.assert(@ptrToInt(nfw) & mask == 0);
        return .{ .ptr = @ptrToInt(nfw) };
    }
};

pub const CodeRunner = struct {
    allocator: *std.mem.Allocator,

    errorReporter: *ErrorReporter,
    errorMsgBuffer: std.ArrayList(u8),
    printBuffer: std.ArrayList(u8),

    // execution
    globalVariables: *std.mem.Allocator,
    stack: std.ArrayListAligned(u8, 8),
    stackPointer: usize = 0,
    basePointer: usize = 0,

    const Self = @This();

    pub fn init(compiler: *Compiler) !Self {
        var stack = std.ArrayListAligned(u8, 8).init(&compiler.stackAllocator.allocator);
        try stack.resize(4 * 1024 * 1024);
        return Self{
            .allocator = compiler.allocator,
            .errorReporter = compiler.errorReporter,
            .stack = stack,
            .globalVariables = &compiler.constantsAllocator.allocator,
            .errorMsgBuffer = std.ArrayList(u8).init(&compiler.stackAllocator.allocator),
            .printBuffer = std.ArrayList(u8).init(&compiler.stackAllocator.allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.stack.deinit();
        self.errorMsgBuffer.deinit();
        self.printBuffer.deinit();
    }

    fn reportError(self: *Self, location: ?*const Location, comptime format: []const u8, args: anytype) void {
        self.errorMsgBuffer.resize(0) catch unreachable;
        std.fmt.format(self.errorMsgBuffer.writer(), format, args) catch {};
        self.errorReporter.report(self.errorMsgBuffer.items, location);
    }

    pub fn printStack(self: *Self) !void {
        if (DEBUG_LOG_STACK) {
            var stdOut = std.io.getStdOut().writer();
            const style = term.Style{ .foreground = .{ .RGB = .{ .r = 0xd6, .g = 0x9a, .b = 0x9a } } };
            term.updateStyle(stdOut, style, null) catch {};
            defer term.updateStyle(stdOut, .{}, style) catch {};

            var i: usize = 0;
            try stdOut.writeAll("stack: [");
            while (i < self.stackPointer) : (i += 8) {
                if (i > 0) {
                    try stdOut.writeAll(", ");
                }
                try std.fmt.format(stdOut, "{}", .{bytesAsValue(u64, self.stack.items[i..(i + 8)])});
            }
            try stdOut.writeAll("]\n");
        }
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

    pub fn copyArgInto(self: *Self, offset: usize, dest: []u8) !void {
        const src = self.basePointer + offset;
        std.mem.copy(u8, dest, self.stack.items[src..(src + dest.len)]);
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
        //if (self.stackPointer > 50) {
        //    self.reportError(null, "Max stack size reached", .{});
        //    return error.StackOverflow;
        //}
        switch (ast.spec) {
            .Block => try self.runBlock(ast),
            .Call => try self.runCall(ast),
            .Identifier => try self.runIdentifier(ast),
            .Int => try self.runInt(ast),
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
        const nativeOrAstFunction = try self.pop(NativeOrAstFunction);

        // Stack frame
        std.log.debug("Stack frame.", .{});
        try self.push(self.basePointer);
        const newBasePointer = self.stackPointer;

        // Arguments.
        std.log.debug("Arguments.", .{});
        for (call.args.items) |arg, i| {
            std.log.debug("Running argument {} at {}", .{ i, self.stackPointer });
            try self.runAst(arg);
        }

        // Finish stack frame.
        std.log.debug("Base pointer: {}", .{self.basePointer});
        self.basePointer = newBasePointer;

        const returnType = call.func.typ.kind.Function.returnType;

        switch (nativeOrAstFunction.get()) {
            .Ast => |func| {
                std.debug.assert(func.is(.Function));
                try self.runAst(func.spec.Function.body);
            },
            .Native => |func| {
                try func.invoke(self, call.func.typ);
            },
        }
        var returnValuePointer = self.stackPointer - returnType.size;

        std.log.debug("After call, restore base pointer.", .{});
        self.stackPointer = newBasePointer;
        self.basePointer = try self.pop(@TypeOf(self.basePointer));
        std.log.debug("Resetting base pointer: {}", .{self.basePointer});

        if (returnType.size > 0) {
            std.log.debug("Copying return value from {} to {}", .{ returnValuePointer, self.stackPointer });
            try self.pushSlice(self.stack.items[returnValuePointer..(returnValuePointer + returnType.size)]);
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

    fn runFunction(self: *Self, ast: *Ast) anyerror!void {
        //std.log.debug("runFunction()", .{});
        const call = &ast.spec.Function;
        std.log.info("Push ast function. {}", .{ast});
        try self.push(NativeOrAstFunction.fromAst(ast));
    }

    fn bytesAsValue(comptime T: type, bytes: []const u8) T {
        // std.log.debug("{}, {x}, {}", .{ @alignOf(T), @ptrToInt(bytes.ptr), @ptrToInt(bytes.ptr) % @alignOf(T) });
        return @ptrCast(*const T, @alignCast(@alignOf(T), bytes.ptr)).*;
    }

    fn printGenericValue(writer: anytype, memory: []const u8, typ: *const Type) anyerror!void {
        switch (typ.kind) {
            .Int => |*int| {
                if (typ.kind.Int.signed) {
                    switch (typ.size) {
                        1 => try std.fmt.format(writer, "{}", .{bytesAsValue(i8, memory)}),
                        2 => try std.fmt.format(writer, "{}", .{bytesAsValue(i16, memory)}),
                        4 => try std.fmt.format(writer, "{}", .{bytesAsValue(i32, memory)}),
                        8 => try std.fmt.format(writer, "{}", .{bytesAsValue(i64, memory)}),
                        16 => try std.fmt.format(writer, "{}", .{bytesAsValue(i128, memory)}),
                        else => unreachable,
                    }
                } else {
                    switch (typ.size) {
                        1 => try std.fmt.format(writer, "{}", .{bytesAsValue(u8, memory)}),
                        2 => try std.fmt.format(writer, "{}", .{bytesAsValue(u16, memory)}),
                        4 => try std.fmt.format(writer, "{}", .{bytesAsValue(u32, memory)}),
                        8 => try std.fmt.format(writer, "{}", .{bytesAsValue(u64, memory)}),
                        16 => try std.fmt.format(writer, "{}", .{bytesAsValue(u128, memory)}),
                        else => unreachable,
                    }
                }
            },
            .Bool => {
                try writer.print("{}", .{bytesAsValue(bool, memory)});
            },
            else => {
                try writer.print("<unknown>", .{});
            },
        }
    }

    fn runCallPrint(self: *Self, ast: *Ast) anyerror!void {
        //std.log.debug("runCallPrint()", .{});
        const call = &ast.spec.Call;

        try self.printBuffer.resize(0);

        for (call.args.items) |arg, i| {
            if (i > 0) {
                try self.printBuffer.writer().print(" ", .{});
            }
            try self.runAst(arg);
            std.log.debug("sp: {}, size: {}", .{ self.stackPointer, arg.typ.size });
            try printGenericValue(self.printBuffer.writer(), self.stack.items[(self.stackPointer - arg.typ.size)..(self.stackPointer)], arg.typ);
            try self.popBytes(arg.typ.size);
        }
        try self.printBuffer.writer().print("\n", .{});

        // Print stuff in different color.
        var stdOut = std.io.getStdOut().writer();
        const style = term.Style{ .foreground = .{ .RGB = .{ .r = 0x9a, .g = 0xd6, .b = 0xd6 } } };
        term.updateStyle(stdOut, style, null) catch {};
        defer term.updateStyle(stdOut, .{}, style) catch {};

        try stdOut.writeAll(self.printBuffer.items);
    }

    fn runIdentifier(self: *Self, ast: *Ast) anyerror!void {
        const id = &ast.spec.Identifier;
        std.log.debug("runIdentifier({s})", .{id.name});
        if (std.mem.eql(u8, id.name, "true")) {
            try self.push(true);
        } else if (std.mem.eql(u8, id.name, "false")) {
            try self.push(false);
        } else {
            std.debug.assert(id.symbol != null);

            switch (id.symbol.?.kind) {
                .Argument => |*arg| {
                    const offset = arg.offset;
                    std.log.debug("Loading argument at index {} to {}", .{ offset, self.stackPointer });
                    //try self.pushSlice(gv.value.?);
                    try self.pushArg(offset, arg.typ.size);
                    try self.printStack();
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
                .NativeFunction => |*func| {
                    std.log.info("Push native function. {}", .{func.wrapper});
                    try self.push(NativeOrAstFunction.fromNative(func.wrapper));
                },
                .Type => |*typ| {
                    try self.push(typ.typ);
                },
                else => return error.NotImplemented,
            }
        }
    }

    fn runInt(self: *Self, ast: *Ast) anyerror!void {
        //std.log.debug("runInt()", .{});
        const int = &ast.spec.Int;
        const size = ast.typ.size;
        const sign = ast.typ.kind.Int.signed;

        if (sign) {
            const value = @bitCast(i128, int.value);
            switch (size) {
                1 => try self.push(@intCast(i8, value)),
                2 => try self.push(@intCast(i16, value)),
                4 => try self.push(@intCast(i32, value)),
                8 => try self.push(@intCast(i64, value)),
                16 => try self.push(@intCast(i128, value)),
                else => unreachable,
            }
        } else {
            switch (size) {
                1 => try self.push(@intCast(u8, int.value)),
                2 => try self.push(@intCast(u16, int.value)),
                4 => try self.push(@intCast(u32, int.value)),
                8 => try self.push(@intCast(u64, int.value)),
                16 => try self.push(@intCast(u128, int.value)),
                else => unreachable,
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
