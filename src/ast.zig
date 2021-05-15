const std = @import("std");

usingnamespace @import("location.zig");
usingnamespace @import("types.zig");
usingnamespace @import("symbol.zig");

pub const Name = []const u8;

pub const AstSpec = union(enum) {
    Access: struct {
        left: *Ast,
        right: *Ast,
    },
    Assignment: struct {
        pattern: *Ast,
        value: *Ast,
    },
    Block: struct {
        body: std.ArrayList(*Ast),
    },
    Call: struct {
        func: *Ast,
        args: std.ArrayList(*Ast),
    },
    ConstDecl: struct {
        pattern: *Ast,
        typ: ?*Ast,
        value: *Ast,
        symbol: ?*Symbol = null,
    },
    VarDecl: struct {
        pattern: *Ast,
        typ: ?*Ast,
        value: ?*Ast,
        symbol: ?*Symbol = null,
    },
    Float: struct {
        value: f128,
    },
    Identifier: struct {
        name: Name,
        symbol: ?*Symbol = null,
    },
    Int: struct {
        value: u128,
    },
    Lambda: struct {
        body: *Ast,
        args: std.ArrayList(*Ast),
    },
    Pipe: struct {
        left: *Ast,
        right: *Ast,
    },
    String: struct {
        value: []const u8,
    },
    Tuple: struct {
        values: std.ArrayList(*Ast),
    },
};

pub const Ast = struct {
    const Self = @This();

    id: usize,
    location: Location,
    typ: *const Type,
    spec: AstSpec,

    pub fn is(self: *const Self, tag: std.meta.Tag(AstSpec)) bool {
        return @as(std.meta.Tag(AstSpec), self.spec) == tag;
    }
};
