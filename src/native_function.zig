const std = @import("std");

usingnamespace @import("code_runner.zig");
usingnamespace @import("common.zig");
usingnamespace @import("types.zig");

pub const NativeFunctionWrapper = struct {
    const Self = @This();

    invokeFn: fn (self: *Self, codeRunner: *CodeRunner, callFuncType: *const Type) anyerror!void,
    deinitFn: fn (self: *Self) void,

    pub fn invoke(self: *Self, codeRunner: *CodeRunner, callFuncType: *const Type) anyerror!void {
        try self.invokeFn(self, codeRunner, callFuncType);
    }

    pub fn deinit(self: *Self) void {
        self.deinitFn(self);
    }

    pub fn init(functionToWrap: anytype, allocator: *std.mem.Allocator) !*NativeFunctionWrapper {
        const Wrapper = struct {
            const FunctionType = @TypeOf(functionToWrap);

            wrapper: NativeFunctionWrapper = .{ .invokeFn = invoke, .deinitFn = deinit },
            func: FunctionType,
            allocator: *std.mem.Allocator,

            pub fn deinit(wrapper: *NativeFunctionWrapper) void {
                const self = @fieldParentPtr(@This(), "wrapper", wrapper);
                self.allocator.destroy(self);
            }

            pub fn invoke(wrapper: *NativeFunctionWrapper, codeRunner: *CodeRunner, callFuncType: *const Type) !void {
                const self = @fieldParentPtr(@This(), "wrapper", wrapper);

                const ArgsType = std.meta.ArgsTuple(FunctionType);
                const info = @typeInfo(ArgsType).Struct;

                if (info.fields.len != callFuncType.kind.Function.params.items.len) {
                    //self.reportError(null, "Wrong number of arguments. Expected {}, got {}.", .{ info.fields.len, callFuncType.kind.Function.params.len });
                    return error.WrongNumberOfArguments;
                }

                var args: ArgsType = undefined;

                var currentOffset: usize = 0;
                inline for (info.fields) |field, i| {
                    const fieldPtr = &@field(args, field.name);

                    std.log.debug("Loading argument from {}", .{codeRunner.basePointer + currentOffset});
                    try codeRunner.copyArgInto(currentOffset, std.mem.asBytes(fieldPtr));
                    currentOffset += callFuncType.kind.Function.params.items[i].size;
                    try codeRunner.printStack();
                }

                const ReturnType = @typeInfo(FunctionType).Fn.return_type;
                if (ReturnType == null or ReturnType == void) {
                    @call(.{}, self.func, args);
                } else {
                    const result = @call(.{}, self.func, args);
                    std.log.debug("After native call, pushing {any}: {s} to {}", .{ result, @typeName(ReturnType.?), codeRunner.stackPointer });
                    try codeRunner.push(result);
                }
            }
        };

        var wrapper = try allocator.create(Wrapper);
        wrapper.* = .{
            .func = functionToWrap,
            .allocator = allocator,
        };
        return &wrapper.wrapper;
    }
};
