const std = @import("std");

pub const String = []const u8;
pub const StringBuf = std.ArrayList(u8);
pub fn List(comptime T: type) type {
    return std.ArrayList(T);
}
