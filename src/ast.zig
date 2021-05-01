const std = @import("std");

usingnamespace @import("location.zig");

pub const Name = []const u8;

pub const AstSpec = union(enum) {
    Identifier: struct {
        name: Name,
    },
    Number: struct {
        value: i64,
    },
    Call: struct {
        func: *Ast,
        args: std.ArrayList(*Ast),
    },
};

pub const Ast = struct {
    location: Location,
    spec: AstSpec,
};
