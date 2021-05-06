const std = @import("std");

usingnamespace @import("location.zig");

pub const Name = []const u8;

pub const AstSpec = union(enum) {
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
    VarDecl: struct {
        pattern: *Ast,
        typ: ?*Ast,
        value: ?*Ast,
    },
    ConstDecl: struct {
        pattern: *Ast,
        typ: ?*Ast,
        value: *Ast,
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
};

pub const Ast = struct {
    location: Location,
    spec: AstSpec,
};
