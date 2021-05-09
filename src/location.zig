usingnamespace @import("common.zig");

pub const Location = struct {
    file: String,
    index: u64 = 0,
    line: u64 = 0,
    column: u64 = 0,
};
