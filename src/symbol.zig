const std = @import("std");

usingnamespace @import("common.zig");
usingnamespace @import("location.zig");
usingnamespace @import("ast.zig");
usingnamespace @import("types.zig");

pub const SymbolKind = union(enum) {
    Constant: struct {
        decl: ?*const Ast = null,
        typ: *const Type,
        value: []u8,
    },
    GlobalVariable: struct {
        decl: *const Ast,
        typ: *const Type,
        value: ?[]u8 = null,
    },
};

pub const Symbol = struct {
    kind: SymbolKind,

    const Self = @This();

    pub fn init(kind: SymbolKind) Self {
        return Self{
            .kind = kind,
        };
    }
};

pub const SymbolTable = struct {
    allocator: *std.mem.Allocator,
    parent: ?*SymbolTable,
    symbols: std.StringHashMap(*Symbol),

    const Self = @This();

    pub fn init(parent: ?*Self, symbolAllocator: *std.mem.Allocator, mapAllocator: *std.mem.Allocator) Self {
        return Self{
            .allocator = symbolAllocator,
            .parent = parent,
            .symbols = std.StringHashMap(*Symbol).init(mapAllocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.symbols.deinit();
    }

    pub fn define(self: *Self, name: String) !*Symbol {
        if (self.symbols.contains(name)) {
            return error.SymbolAlreadyExists;
        }

        var symbol = try self.allocator.create(Symbol);
        try self.symbols.put(name, symbol);
        return symbol;
    }

    pub fn get(self: *Self, name: String) ?*Symbol {
        if (self.symbols.get(name)) |symbol| {
            return symbol;
        }
        if (self.parent) |p| {
            return p.get(name);
        }
        return null;
    }
};
