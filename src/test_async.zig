const std = @import("std");

const Context = struct {
    id: u64,
    frame_buffer: []align(16) u8,
    allocator: *std.mem.Allocator,
    func: fn () callconv(.Async) void,
    frame: ?anyframe = null,
    done: bool = false,

    const Self = @This();

    fn init(id: u64, func: fn () callconv(.Async) void, allocator: *std.mem.Allocator) !Context {
        var frame_buffer = try allocator.allocAdvanced(u8, 16, 4096 * 4, .at_least);
        var context = Context{
            .id = id,
            .frame_buffer = frame_buffer,
            .allocator = allocator,
            .func = func,
        };

        return context;
    }

    fn deinit(self: *Self) void {
        self.allocator.free(self.frame_buffer);
    }

    fn call(self: *Self) !void {
        currentContext = self;
        if (self.frame) |frame| {
            resume frame;
        } else {
            _ = @asyncCall(self.frame_buffer, {}, self.func, .{});
        }
        currentContext = null;
    }
};

var currentContext: ?*Context = null;
var gAllocator: *std.mem.Allocator = undefined;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;
    gAllocator = allocator;

    std.log.info("test async", .{});

    var calls = std.ArrayList(Context).init(allocator);
    defer calls.deinit();

    for ([_]u8{0} ** 3) |_, i| {
        try calls.append(try Context.init(i, foo, allocator));
    }

    while (calls.items.len > 0) {
        var i: usize = 0;
        while (i < calls.items.len) {
            std.debug.assert(currentContext == null);
            try calls.items[i].call();
            std.debug.assert(currentContext == null);
            if (calls.items[i].done) {
                var context = calls.swapRemove(i);
                context.deinit();
            } else {
                i += 1;
            }
        }
    }
}

fn callAsync(func: anytype, args: anytype) callconv(.Async) void {
    var buff = gAllocator.allocAdvanced(u8, 16, @frameSize(func), .at_least) catch unreachable;
    defer gAllocator.free(buff);
    _ = @asyncCall(buff, {}, func, args);
    var frame = currentContext.?.frame.?;
    suspend currentContext.?.frame = @frame();
    resume frame;
}

fn yield() void {
    suspend currentContext.?.frame = @frame();
}

fn bar(b: bool) callconv(.Async) void {
    if (b) {
        baz();
    } else {
        callAsync(bar, .{!b});
    }
}

fn baz() void {
    std.log.info("baz 1", .{});
    yield();
    std.log.info("baz 2", .{});
}

fn foo() void {
    var i: i64 = 0;
    while (i < 2) : (i += 1) {
        std.log.info("[{}:{}] {any}", .{ @ptrToInt(currentContext), currentContext.?.id, i });
        bar(false);
        std.log.info("[{}:{}] after suspend", .{ @ptrToInt(currentContext), currentContext.?.id });
    }

    std.log.info("[{}:{}] after loop", .{ @ptrToInt(currentContext), currentContext.?.id });
    currentContext.?.done = true;
}
