const std = @import("std");

usingnamespace @import("common.zig");
usingnamespace @import("location.zig");
usingnamespace @import("ast.zig");
usingnamespace @import("types.zig");

pub const SymbolKind = union(enum) {
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

    pub fn init(parent: ?*Self, allocator: *std.mem.Allocator) !*Self {
        var result = try allocator.create(Self);
        result.* = Self{
            .allocator = allocator,
            .parent = parent,
            .symbols = std.StringHashMap(*Symbol).init(allocator),
        };
        return result;
    }

    pub fn deinit(self: *Self) void {
        var it = self.symbols.iterator();
        while (it.next()) |kv| {
            self.allocator.destroy(kv.value);
        }
        self.symbols.deinit();
        self.allocator.destroy(self);
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
