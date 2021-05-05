const std = @import("std");

usingnamespace @import("location.zig");

pub const Name = []const u8;

pub const AstSpec = union(enum) {
    Block: struct {
        body: std.ArrayList(*Ast),
    },
    Call: struct {
        func: *Ast,
        args: std.ArrayList(*Ast),
    },
    Float: struct {
        value: f128,
    },
    Identifier: struct {
        name: Name,
    },
    Int: struct {
        value: u128,
    },
    Pipe: struct {
        left: *Ast,
        right: *Ast,
    },
    String: struct {
        value: []const u8,
    },
};

pub const Ast = struct {
    location: Location,
    spec: AstSpec,
};
