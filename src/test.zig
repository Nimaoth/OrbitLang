const std = @import("std");
usingnamespace @import("coro.zig");
usingnamespace @import("fiber.zig");

const Context = struct {
    id: u64,
    done: bool = false,
    fiber: Fiber,

    const Self = @This();

    fn init(id: u64, func: fn (Fiber) anyerror!void, allocator: *std.mem.Allocator) !Context {
        const fiber = try Fiber.init(func, allocator);
        return Context{
            .id = id,
            .fiber = fiber,
        };
    }

    fn deinit(self: *Self) void {
        self.fiber.deinit();
    }

    fn call(self: *Self) !void {
        currentContext = self;
        _ = try self.fiber.call();
        currentContext = null;
    }
};

var currentContext: ?*Context = null;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    std.log.info("test fiber", .{});

    var calls = std.ArrayList(Context).init(allocator);
    defer calls.deinit();

    for ([_]u8{0} ** 3) |_, i| {
        try calls.append(try Context.init(i, foo, allocator));
    }
    defer for (calls.items) |*context| {
        context.deinit();
    };

    while (calls.items.len > 0) {
        var i: usize = 0;
        while (i < calls.items.len) {
            try calls.items[i].call();
            if (calls.items[i].done) {
                var context = calls.swapRemove(i);
                context.deinit();
            } else {
                i += 1;
            }
        }
    }
}

fn yield() void {
    Fiber.yield() catch unreachable;
}

fn bar(b: bool) void {
    if (b) {
        baz();
    } else {
        bar(!b);
    }
}

fn baz() void {
    std.log.info("baz 1", .{});
    //yield();
    std.log.info("baz 2", .{});
}

fn foo(fiber: Fiber) !void {
    var i: i64 = 0;
    while (i < 2) : (i += 1) {
        std.log.info("[{}:{}] {any}", .{ @ptrToInt(currentContext), currentContext.?.id, i });
        bar(false);
        std.log.info("[{}:{}] after suspend", .{ @ptrToInt(currentContext), currentContext.?.id });
    }

    std.log.info("[{}:{}] after loop", .{ @ptrToInt(currentContext), currentContext.?.id });
    currentContext.?.done = true;
}
