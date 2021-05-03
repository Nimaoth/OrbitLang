const std = @import("std");
const opt = @import("build_options");

const use_fibers = opt.use_fibers;
const fib_of = opt.fib_of orelse 25;
const n_coros = opt.n_coros orelse 5;
const n_iter = opt.n_iter orelse 5;
const logging = opt.log;
const use_arena = opt.arena;

const Coroutine = if (use_fibers) @import("stackfull_coroutine.zig").StackfullCoroutine(Context) else @import("stackless_coroutine.zig").StacklessCoroutine(Context);

const Context = struct {
    id: u64,
    done: bool = false,
    allocator: *std.mem.Allocator,
    coro: Coroutine,

    const Self = @This();

    fn init(id: u64, func: Coroutine.UserFunctionType, allocator: *std.mem.Allocator) !*Context {
        var context = try allocator.create(Context);
        const coro = try Coroutine.init(func, context, allocator);
        context.* = Context{
            .id = id,
            .allocator = allocator,
            .coro = coro,
        };
        return context;
    }

    fn deinit(self: *Self) void {
        self.coro.deinit();
        self.allocator.destroy(self);
    }

    fn call(self: *Self) !void {
        _ = try self.coro.call();
    }
};

fn log(comptime format: []const u8, args: anytype) void {
    var writer = std.io.getStdOut().writer();
    std.fmt.format(writer, format, args) catch unreachable;
    writer.writeAll("\n") catch unreachable;
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    var arena = std.heap.ArenaAllocator.init(&gpa.allocator);
    defer arena.deinit();
    if (use_arena) {
        allocator = &arena.allocator;
    }

    log("test {s}", .{if (use_fibers) "fiber" else "async"});

    var calls = std.ArrayList(*Context).init(allocator);
    defer calls.deinit();

    for ([_]u8{0} ** 5) |_, i| {
        try calls.append(try Context.init(i, foo, allocator));
    }
    defer for (calls.items) |context| {
        context.deinit();
    };

    var timer = std.time.Timer.start() catch unreachable;
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
    const elapsed = timer.read() / std.time.ns_per_ms;
    log("{}ms", .{elapsed});
}

fn div2(i: u64) callconv(if (use_fibers) .C else .Async) void {
    var currentCoroutine = Coroutine.current();
    if (currentCoroutine.getUserData()) |context| {
        log("[{}] div2: {}", .{ context.id, i });
    }

    const v = i / 2;
    Coroutine.yield() catch unreachable;

    if (v == 1) {
        baz();
    } else if (v % 2 == 0) {
        if (use_fibers)
            div2(v)
        else
            Coroutine.callAsync(div2, .{v});
    } else {
        if (use_fibers)
            add3(v)
        else
            Coroutine.callAsync(add3, .{v});
    }
}

fn add3(i: u64) callconv(if (use_fibers) .C else .Async) void {
    var currentCoroutine = Coroutine.current();
    if (currentCoroutine.getUserData()) |context| {
        log("[{}] add3: {}", .{ context.id, i });
    }

    const v = i + 3;
    Coroutine.yield() catch unreachable;

    if (v == 1) {
        baz();
    } else if (v % 2 == 0) {
        if (use_fibers)
            div2(v)
        else
            Coroutine.callAsync(div2, .{v});
    } else {
        if (use_fibers)
            add3(v)
        else
            Coroutine.callAsync(add3, .{v});
    }
}

fn fib(b: u64) callconv(if (use_fibers) .C else .Async) u64 {
    if (logging) {
        log("fib({})", .{b});
    }
    Coroutine.yield() catch unreachable;
    if (b <= 1) {
        return 1;
    } else {
        if (use_fibers) {
            return fib(b - 1) + fib(b - 2);
        } else {
            return Coroutine.callAsync(fib, .{b - 1}) + Coroutine.callAsync(fib, .{b - 2});
        }
    }
}

fn foo() void {
    var currentCoroutine = Coroutine.current();
    var currentContext = currentCoroutine.getUserData();

    var i: i64 = 0;
    if (currentContext) |context| {
        while (i < n_iter) : (i += 1) {
            log("{}", .{fib(fib_of)});
        }

        context.done = true;
    } else {
        log("failed to get context", .{});
    }
}
