const std = @import("std");
const testing = std.testing;
const assert = std.debug.assert;

pub fn RingBuffer(comptime T: type) type {
    return RingBufferAligned(T, null);
}

pub fn RingBufferAligned(comptime T: type, comptime alignment: ?u29) type {
    if (alignment) |a| {
        if (a == @alignOf(T)) {
            return RingBufferAligned(T, null);
        }
    }
    return struct {
        pub const Self = @This();

        items: Slice,
        first: usize,
        last: usize,
        allocator: *std.mem.Allocator,

        pub const Slice = if (alignment) |a| ([]align(a) T) else []T;
        pub const SliceConst = if (alignment) |a| ([]align(a) const T) else []const T;

        /// Deinitialize with `deinit` or use `toOwnedSlice`.
        pub fn init(allocator: *std.mem.Allocator) !Self {
            return Self{
                .items = try allocator.alloc(T, 2),
                .first = 0,
                .last = 0,
                .allocator = allocator,
            };
        }

        /// Initialize with capacity to hold at least `num` elements.
        /// Deinitialize with `deinit` or use `toOwnedSlice`.
        pub fn initCapacity(allocator: *std.mem.Allocator, num: usize) !Self {
            var self = Self.init(allocator);
            self.items = try self.allocator.allocAdvanced(T, alignment, num, .at_least);
            return self;
        }

        /// Release all allocated memory.
        pub fn deinit(self: Self) void {
            self.allocator.free(self.items);
        }

        /// Modify the array so that it can hold at least `new_capacity` items.
        /// Invalidates pointers if additional memory is needed.
        pub fn ensureCapacity(self: *Self, new_capacity: usize) !void {
            var better_capacity = self.items.len;
            if (better_capacity > new_capacity) return;

            while (true) {
                better_capacity += better_capacity / 2 + 8;
                if (better_capacity >= new_capacity) break;
            }

            // TODO This can be optimized to avoid needlessly copying undefined memory.
            const new_memory = try self.allocator.allocAdvanced(T, alignment, better_capacity, .at_least);
            if (self.last > self.first) {
                std.mem.copy(T, new_memory, self.items[self.first..self.last]);
                self.allocator.free(self.items);
                self.items = new_memory;
                self.last -= self.first;
                self.first = 0;
            } else if (self.last < self.first) {
                std.mem.copy(T, new_memory, self.items[self.first..]);
                std.mem.copy(T, new_memory[self.items.len - self.first ..], self.items[0..self.last]);
                self.allocator.free(self.items);
                self.items = new_memory;
                self.last += self.items.len - self.first;
                self.first = 0;
            } else {
                self.last = 0;
                self.first = 0;
            }
        }

        pub fn len(self: *Self) usize {
            if (self.last >= self.first) {
                return self.last - self.first;
            } else {
                return self.last + self.items.len - self.first;
            }
        }

        /// Increase length by 1, returning pointer to the new item.
        /// The returned pointer becomes invalid when the list resized.
        pub fn addOne(self: *Self) !*T {
            const newlen = self.len() + 1;
            try self.ensureCapacity(newlen);
            return self.addOneAssumeCapacity();
        }

        /// Increase length by 1, returning pointer to the new item.
        /// Asserts that there is already space for the new item without allocating more.
        /// **Does not** invalidate pointers.
        /// The returned pointer becomes invalid when the list resized.
        pub fn addOneAssumeCapacity(self: *Self) *T {
            assert(self.len() < self.items.len);
            const result = &self.items[self.last];
            self.last = (self.last + 1) % self.items.len;
            return result;
        }

        pub fn push(self: *Self, value: T) !void {
            const ptr = try self.addOne();
            ptr.* = value;
        }

        pub fn pop(self: *Self) ?T {
            if (self.first == self.last)
                return null;

            const result = self.items[self.first];
            self.first = (self.first + 1) % self.items.len;
            return result;
        }
    };
}

test "RingBuffer.len" {
    const a = testing.allocator;
    var buff = try RingBuffer(usize).init(a);
    defer buff.deinit();

    testing.expectEqual(@as(usize, 0), buff.len());
    _ = try buff.addOne();
    testing.expectEqual(@as(usize, 1), buff.len());
    _ = try buff.addOne();
    testing.expectEqual(@as(usize, 2), buff.len());
    _ = try buff.addOne();
    testing.expectEqual(@as(usize, 3), buff.len());
    _ = try buff.addOne();
    testing.expectEqual(@as(usize, 4), buff.len());
    _ = try buff.addOne();
    testing.expectEqual(@as(usize, 5), buff.len());
    _ = try buff.addOne();
    testing.expectEqual(@as(usize, 6), buff.len());

    _ = buff.pop();
    testing.expectEqual(@as(usize, 5), buff.len());
    _ = buff.pop();
    testing.expectEqual(@as(usize, 4), buff.len());
    _ = buff.pop();
    testing.expectEqual(@as(usize, 3), buff.len());
    _ = buff.pop();
    testing.expectEqual(@as(usize, 2), buff.len());
    _ = buff.pop();
    testing.expectEqual(@as(usize, 1), buff.len());
    _ = buff.pop();
    testing.expectEqual(@as(usize, 0), buff.len());
    _ = buff.pop();
    testing.expectEqual(@as(usize, 0), buff.len());
}

test "RingBuffer" {
    const a = testing.allocator;
    var buff = try RingBuffer(usize).init(a);
    defer buff.deinit();

    var i: usize = 0;
    while (i < 100) : (i += 1) {
        testing.expectEqual(@as(usize, i), buff.len());
        try buff.push(i);
    }

    i = 0;
    while (i < 100) : (i += 1) {
        testing.expectEqual(@as(usize, 100 - i), buff.len());
        testing.expectEqual(@as(?usize, i), buff.pop());
    }

    testing.expectEqual(@as(usize, 0), buff.len());
    testing.expectEqual(@as(?usize, null), buff.pop());
}
