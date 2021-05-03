const std = @import("std");

var gCurrentCoroutine: ?*StacklessCoroutine(void) = undefined;

pub fn StacklessCoroutine(comptime T: type) type {
    return struct {
        const Self = @This();
        pub const UserFunctionType = fn () callconv(.Async) void;

        // members
        allocator: *std.mem.Allocator,
        func: UserFunctionType,
        frame: ?anyframe = null,
        frame_buffer: []align(16) u8 = &.{},
        user_data: *T,

        // functions
        pub fn init(func: UserFunctionType, user_data: *T, allocator: *std.mem.Allocator) !Self {
            return Self{
                .allocator = allocator,
                .func = func,
                .user_data = user_data,
            };
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.frame_buffer);
        }

        pub fn call(self: *Self) !void {
            gCurrentCoroutine = @ptrCast(@TypeOf(gCurrentCoroutine), self);
            if (self.frame) |frame| {
                resume frame;
            } else {
                self.frame_buffer = try self.allocator.allocAdvanced(u8, 16, 4096 * 4, .at_least);
                _ = @asyncCall(self.frame_buffer, {}, self.func, .{});
            }
            gCurrentCoroutine = null;
        }

        pub fn yield() !void {
            suspend gCurrentCoroutine.?.frame = @frame();
        }

        pub fn getUserData(self: *Self) ?*T {
            return self.user_data;
        }

        pub fn current() *Self {
            return @ptrCast(*Self, gCurrentCoroutine);
        }

        pub fn callAsync(func: anytype, args: anytype) callconv(.Async) returnType(@TypeOf(func)) {
            const ReturnType = returnType(@TypeOf(func));

            var curr = gCurrentCoroutine orelse @panic("gCurrentCoroutine shouldn't be null.");
            var buff = curr.allocator.allocAdvanced(u8, 16, @frameSize(func), .at_least) catch unreachable;
            defer curr.allocator.free(buff);
            curr.frame = null;
            if (ReturnType != void) {
                var result: ReturnType = undefined;
                _ = @asyncCall(buff, &result, func, args);
                while (curr.frame) |f| {
                    suspend curr.frame = @frame();
                    curr.frame = null;
                    resume f;
                }
                return result;
            } else {
                _ = @asyncCall(buff, {}, func, args);
                while (curr.frame) |f| {
                    suspend curr.frame = @frame();
                    curr.frame = null;
                    resume f;
                }
            }
        }
    };
}

fn returnType(comptime func: type) type {
    return @typeInfo(func).Fn.return_type.?;
}

fn frameToPointer(frame: ?anyframe) ?*i64 {
    if (frame) |f| {
        return @ptrCast(*const *i64, &f).*;
    } else {
        return null;
    }
}
fn frameCast(comptime T: type, frame: anyframe) *(anyframe->T) {
    return @ptrCast(*const (anyframe->T), &frame).*;
}
