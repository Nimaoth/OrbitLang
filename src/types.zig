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
    typ: *const Type,
};

pub const TypeKind = union(enum) {
    Unknown: void,
    Error: void,
    Type: void,
    Void: void,
    Unreachable: void,
    Bool: void,
    Float: void,
    Int: struct {
        signed: bool,
    },
    Pointer: struct {
        child: *const Type,
    },
    Slice: struct {
        child: *const Type,
    },
    Array: struct {
        child: *const Type,
        len: TypeSize,
    },
    Struct: struct {
        members: List(StructMember),
    },
    Function: struct {
        params: List(*const Type),
        returnType: *const Type,
    },
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

    pub fn is(self: *const Self, tag: std.meta.Tag(TypeKind)) bool {
        return @as(std.meta.Tag(TypeKind), self.kind) == tag;
    }

    pub fn format(
        self: *const Self,
        fmt: String,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        switch (self.kind) {
            .Error => try writer.writeAll("<Error>"),
            .Unknown => try writer.writeAll("<Unknown>"),
            .Type => try writer.writeAll("type"),
            .Void => try writer.writeAll("void"),
            .Unreachable => try writer.writeAll("unreachable"),
            .Bool => try std.fmt.format(writer, "b{}", .{self.size * 8}),
            .Float => try std.fmt.format(writer, "f{}", .{self.size * 8}),
            .Int => |*int| try std.fmt.format(writer, "{s}{}", .{
                (if (int.signed) "i" else "u"),
                self.size * 8,
            }),
            .Pointer => |*ptr| {
                try std.fmt.format(writer, "^", .{});
                try std.fmt.format(writer, "{}", .{ptr.child});
            },
            .Slice => |*slc| {
                try std.fmt.format(writer, "[]", .{});
                try std.fmt.format(writer, "{}", .{slc.child});
            },
            .Array => |*arr| {
                switch (arr.len) {
                    .Known => |len| try std.fmt.format(writer, "[{}]", .{len}),
                    .Unknown => try std.fmt.format(writer, "[?]", .{}),
                    .Generic => |name| try std.fmt.format(writer, "[{s}]", .{name}),
                }
                try std.fmt.format(writer, "{}", .{arr.child});
            },
            .Struct => |*str| {
                try std.fmt.format(writer, "struct", .{});
            },
            .Function => |*func| {
                try writer.writeAll("fn(");
                var i: usize = 0;
                while (i < func.params.items.len) : (i += 1) {
                    if (i > 0) {
                        try writer.writeAll(", ");
                    }
                    try std.fmt.format(writer, "{}", .{func.params.items[i]});
                }
                try std.fmt.format(writer, ") -> {}", .{func.returnType});
            },

            //else => try writer.writeAll("<Unknown>"),
        }
    }
};

pub const TypeRegistry = struct {
    allocator: std.heap.ArenaAllocator,

    errorType: ?*Type = null,
    typeType: ?*Type = null,
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

    pub fn allocTypeArray(self: *Self, size: usize) !List(*const Type) {
        return List(*const Type).init(&self.allocator.allocator);
    }

    pub fn getErrorType(self: *Self) !*const Type {
        if (self.errorType == null) {
            self.errorType = try self.allocator.allocator.create(Type);
            self.errorType.?.* = Type{
                .flags = .{
                    .ready = true,
                    .size_set = true,
                    .generic = false,
                    .generic_set = true,
                },
                .size = 0,
                .kind = .Error,
            };
        }
        return self.errorType.?;
    }

    pub fn getTypeType(self: *Self) !*const Type {
        if (self.typeType == null) {
            self.typeType = try self.allocator.allocator.create(Type);
            self.typeType.?.* = Type{
                .flags = .{
                    .ready = true,
                    .size_set = true,
                    .generic = false,
                    .generic_set = true,
                },
                .size = 0,
                .kind = .Type,
            };
        }
        return self.typeType.?;
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

    pub fn getFunctionType(
        self: *Self,
        params: List(*const Type),
        returnType: *const Type,
    ) !*const Type {
        var typ = try self.allocator.allocator.create(Type);
        typ.* = Type{
            .flags = .{
                .ready = true,
                .size_set = true,
                .generic = false,
                .generic_set = true,
            },
            .size = 8, // @todo: size of pointer
            .kind = .{ .Function = .{
                .params = params,
                .returnType = returnType,
            } },
        };
        return typ;
    }

    pub fn createFromNativeType(self: *Self, comptime T: type) !*const Type {
        const typeInfo = @typeInfo(T);

        var size: usize = 0;
        var kind: TypeKind = .Unknown;

        switch (typeInfo) {
            .Void => return self.getVoidType(),
            .Bool => return self.getBoolType(1),
            .Int => |*info| return self.getIntType(info.bits / 8, if (info.signedness == .signed) true else false, null),
            .Fn => |*info| {
                var params = List(*const Type).init(&self.allocator.allocator);

                inline for (info.args) |arg, i| {
                    try params.append(try self.createFromNativeType(arg.arg_type.?));
                }

                size = 8;
                kind = .{ .Function = .{
                    .params = params,
                    .returnType = if (info.return_type) |typ| try self.createFromNativeType(typ) else try self.getVoidType(),
                } };
            },

            else => @compileError("Not implemented: createFromNativeType(" ++ @typeName(T) ++ ")"),
        }

        var typ = try self.allocator.allocator.create(Type);
        typ.* = Type{
            .flags = .{
                .ready = true,
                .size_set = true,
                .generic = false,
                .generic_set = true,
            },
            .size = size,
            .kind = kind,
        };

        return typ;
    }
};
