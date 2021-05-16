const std = @import("std");

usingnamespace @import("coro.zig");

pub fn StackfullCoroutine(comptime T: type) type {
    return struct {
        const Self = @This();
        pub const UserFunctionType = fn () void;

        // members
        coro: *mco_coro,

        // functions
        pub fn init(func: UserFunctionType, user_data: *T, allocator: *std.mem.Allocator) !Self {
            var desc = mco_desc_init(Self.wrapper, 1024 * 1024 * 10);
            desc.malloc_cb = customAlloc;
            desc.free_cb = customFree;

            desc.allocator_data = @ptrCast(*c_void, allocator);
            desc.user_data = user_data;

            var coro: *mco_coro = undefined;
            const result = mco_create(&coro, &desc);
            if (result != .Success) {
                return error.FailedToCreateCoroutine;
            }
            var fiber = Self{
                .coro = coro,
            };
            errdefer fiber.deinit();

            try fiber.push(UserFunctionType, func);
            return fiber;
        }

        pub fn deinit(self: *Self) void {
            _ = mco_destroy(self.coro);
        }

        pub fn call(self: *Self) !mco_state {
            if (mco_resume(self.coro) != .Success) {
                return error.FailedToResumeCoroutine;
            }

            const state = mco_status(self.coro);
            if (state == .Dead and mco_get_bytes_stored(self.coro) > 0) blk: {
                const err = self.pop(anyerror) catch |err| {
                    if (err != error.FailedToPopFromCoroutine) {
                        return err;
                    }
                    break :blk;
                };
                return err;
            }

            return state;
        }

        pub fn yield() !void {
            if (mco_yield(mco_running()) != .Success) {
                return error.FailedToYieldCoroutine;
            }
        }

        pub fn getUserData(self: *Self) ?*T {
            if (mco_get_user_data(mco_running())) |user_data| {
                return @ptrCast(*T, @alignCast(@alignOf(T), user_data));
            } else {
                return null;
            }
        }

        pub fn current() Self {
            return Self{ .coro = mco_running() };
        }

        pub fn push(self: Self, comptime V: type, value: V) !void {
            if (mco_push(self.coro, @ptrCast(*const c_void, &value), @sizeOf(V)) != .Success) {
                return error.FailedToPushToCoroutine;
            }
        }

        pub fn pop(self: Self, comptime V: type) !V {
            var result: V = undefined;
            if (mco_pop(self.coro, &result, @sizeOf(V)) != .Success) {
                return error.FailedToPopFromCoroutine;
            }
            return result;
        }

        fn wrapper(coro: *mco_coro) callconv(.C) void {
            var fiber = Self{ .coro = coro };
            var func = fiber.pop(UserFunctionType) catch |err| {
                std.log.err("Failed to get user function from coroutine data: {any}", .{err});
                return;
            };
            func();
        }

        fn customAlloc(size: usize, allocator_data: ?*c_void) callconv(.C) ?*c_void {
            var allocator = @ptrCast(*std.mem.Allocator, @alignCast(@alignOf(std.mem.Allocator), allocator_data.?));
            var mem = allocator.allocAdvanced(u8, @alignOf(usize), size + @sizeOf(usize), .exact) catch {
                std.log.err("myMalloc failed", .{});
                return null;
            };
            var sizePtr = @ptrCast(*usize, mem.ptr);
            sizePtr.* = size;
            return mem.ptr + @sizeOf(usize);
        }

        fn customFree(ptr: ?*c_void, allocator_data: ?*c_void) callconv(.C) void {
            if (ptr == null) {
                return;
            }
            var allocator = @ptrCast(*std.mem.Allocator, @alignCast(@alignOf(std.mem.Allocator), allocator_data.?));
            const sizePtr = @intToPtr(*usize, @ptrToInt(ptr.?) - @sizeOf(usize));
            const size = sizePtr.* + @sizeOf(usize);
            allocator.free(@ptrCast([*]const u8, sizePtr)[0..size]);
        }
    };
}
