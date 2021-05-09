const std = @import("std");

usingnamespace @import("common.zig");
usingnamespace @import("lexer.zig");
usingnamespace @import("ast.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("error_handler.zig");
usingnamespace @import("code_formatter.zig");

pub const TypeFlags = packed struct {
    ready: bool = false,
    size_set: bool = false,
    generic: bool = false,
    generic_set: bool = false,
};

pub const UnknownType: *const Type = &Type{ .kind = .Unknown };

pub const StructMember = struct {
    name: String,
    typ: *Type,
};

pub const TypeKind = union(enum) {
    Unknown: void,
    Void: void,
    Unreachable: void,
    Bool: void,
    Float: void,
    Int: struct {
        signed: bool,
    },
    Pointer: struct {
        child: *Type,
    },
    Slice: struct {
        child: *Type,
    },
    Array: struct {
        child: *Type,
        len: TypeSize,
    },
    Struct: struct {
        members: List(StructMember),
    },

    pub fn is(self: *Self, tag: std.meta.Tag(@This())) bool {
        return @as(std.meta.Tag(@This()), self.*) == tag;
    }
};

pub const TypeSize = union(enum) {
    Known: u64,
    Unknown: void,
    Generic: String,

    pub fn is(self: *Self, tag: std.meta.Tag(@This())) bool {
        return @as(std.meta.Tag(@This()), self.*) == tag;
    }
};

pub const Type = struct {
    flags: TypeFlags = TypeFlags{},
    size: usize = 0,
    kind: TypeKind,

    const Self = @This();

    pub fn isGeneric(self: *Self) !bool {
        switch (self.kind) {
            .Pointer => |*typ| {
                self.flags.generic = typ.child.isGeneric();
                self.flags.generic_set = true;
                return self.flags.generic;
            },
            .Slice => |*typ| {
                self.flags.generic = typ.child.isGeneric();
                self.flags.generic_set = true;
                return self.flags.generic;
            },
            .Array => |*typ| {
                const childIsGeneric = typ.child.isGeneric();
                const lenIsGeneric = typ.len.is(.Generic);
                self.flags.generic = childIsGeneric or lenIsGeneric;
                self.flags.generic_set = true;
                return self.flags.generic;
            },
            else => {
                self.flags.generic = false;
                self.flags.generic_set = true;
                return false;
            },
        }
    }
};

pub const TypeRegistry = struct {
    allocator: std.heap.ArenaAllocator,

    voidType: ?*Type = null,

    const Self = @This();

    pub fn init(allocator: *std.mem.Allocator) !Self {
        return Self{
            .allocator = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.allocator.deinit();
    }

    pub fn getBoolType(self: *Self, size: usize) !*Type {
        var typ = try self.allocator.allocator.create(Type);
        typ.* = Type{
            .flags = .{
                .ready = true,
                .size_set = true,
                .generic = false,
                .generic_set = true,
            },
            .size = size,
            .kind = .Bool,
        };
        return typ;
    }

    pub fn getIntType(self: *Self, size: usize, signed: bool, alignment: ?usize) !*Type {
        var typ = try self.allocator.allocator.create(Type);
        typ.* = Type{
            .flags = .{
                .ready = true,
                .size_set = true,
                .generic = false,
                .generic_set = true,
            },
            .size = size,
            //.alignment = alignment orelse size,
            .kind = TypeKind{ .Int = .{
                .signed = signed,
            } },
        };
        return typ;
    }

    pub fn getVoidType(self: *Self) !*const Type {
        if (self.voidType == null) {
            self.voidType = try self.allocator.allocator.create(Type);
            self.voidType.?.* = Type{
                .flags = .{
                    .ready = true,
                    .size_set = true,
                    .generic = false,
                    .generic_set = true,
                },
                .size = 0,
                .kind = .Void,
            };
        }
        return self.voidType.?;
    }
};
