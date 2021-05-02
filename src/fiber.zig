const std = @import("std");

usingnamespace @import("coro.zig");

const UserFunctionType = fn (Fiber) anyerror!void;

pub const Fiber = struct {
    coro: *mco_coro,

    const Self = @This();

    pub fn init(func: UserFunctionType, allocator: *std.mem.Allocator) !Fiber {
        var desc = mco_desc_init(Self.wrapper, 0);
        desc.malloc_cb = customAlloc;
        desc.free_cb = customFree;

        //@ptrCast(*UserFunctionType, &desc.user_data).* = func;
        desc.allocator_data = @ptrCast(*c_void, allocator);

        var coro: *mco_coro = undefined;
        const result = mco_create(&coro, &desc);
        if (result != .Success) {
            return error.FailedToCreateCoroutine;
        }
        var fiber = Fiber{
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

    pub fn running() Fiber {
        return Fiber{ .coro = mco_running() };
    }

    pub fn push(self: Self, comptime T: type, value: T) !void {
        if (mco_push(self.coro, @ptrCast(*const c_void, &value), @sizeOf(T)) != .Success) {
            return error.FailedToPushToCoroutine;
        }
    }

    pub fn pop(self: Self, comptime T: type) !T {
        var result: T = undefined;
        if (mco_pop(self.coro, &result, @sizeOf(T)) != .Success) {
            return error.FailedToPopFromCoroutine;
        }
        return result;
    }

    fn wrapper(coro: *mco_coro) callconv(.C) void {
        var fiber = Fiber{ .coro = coro };
        var func = fiber.pop(UserFunctionType) catch |err| {
            std.log.err("Failed to get user function from coroutine data: {any}", .{err});
            return;
        };
        func(fiber) catch |err| {
            fiber.push(anyerror, err) catch {
                std.log.err("Failed to push error from user function to fiber: {any}", .{err});
            };
        };
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
