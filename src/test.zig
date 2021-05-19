const std = @import("std");

fn fib(n: u64) u64 {
    if (n <= 1) return 1;
    return fib(n - 1) + fib(n - 2);
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    var i: u64 = 0;
    while (i <= 42) : (i += 1) {
        std.log.debug("{}, {}", .{ i, fib(i) });
    }
}
