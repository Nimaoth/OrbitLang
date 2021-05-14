const std = @import("std");

usingnamespace @import("common.zig");
usingnamespace @import("location.zig");
usingnamespace @import("compiler.zig");
usingnamespace @import("ast.zig");
usingnamespace @import("types.zig");
usingnamespace @import("job.zig");

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

    pub fn get(self: *Self, name: String, location: *Location) !?*Symbol {
        if (self.parent == null) {
            // Top level symbol table.
            while (true) {
                if (self.symbols.get(name)) |symbol| {
                    return symbol;
                }

                var ctx = Coroutine.current().getUserData() orelse unreachable;

                var condition = struct {
                    condition: FiberWaitCondition = .{
                        .evalFn = eval,
                        .reportErrorFn = reportError,
                    },
                    scope: *SymbolTable,
                    name: String,
                    location: *Location,

                    pub fn eval(condition: *FiberWaitCondition) bool {
                        const s = @fieldParentPtr(@This(), "condition", condition);
                        return s.scope.symbols.contains(s.name);
                    }

                    pub fn reportError(condition: *FiberWaitCondition, compiler: *Compiler) void {
                        const s = @fieldParentPtr(@This(), "condition", condition);
                        compiler.reportError(s.location, "Symbol not found: {s}", .{s.name});
                    }
                }{
                    .scope = self,
                    .name = name,
                    .location = location,
                };
                try ctx.waitUntil(&condition.condition);
            }
        }

        if (self.symbols.get(name)) |symbol| {
            return symbol;
        }
        if (self.parent) |p| {
            return p.get(name, location);
        }
        return null;
    }
};
