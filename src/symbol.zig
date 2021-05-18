const std = @import("std");

usingnamespace @import("ast.zig");
usingnamespace @import("common.zig");
usingnamespace @import("compiler.zig");
usingnamespace @import("job.zig");
usingnamespace @import("location.zig");
usingnamespace @import("native_function.zig");
usingnamespace @import("types.zig");

pub const SymbolKind = union(enum) {
    NotSet,
    Argument: struct {
        decl: *const Ast,
        typ: *const Type,
        offset: usize,
    },
    Constant: struct {
        decl: ?*const Ast = null,
        typ: *const Type,
        value: ?[]u8, // Null if not set yet.
    },
    GlobalVariable: struct {
        decl: *const Ast,
        typ: *const Type,
        value: ?[]u8 = null,
    },
    NativeFunction: struct {
        typ: *const Type,
        wrapper: *NativeFunctionWrapper,
    },
    Type: struct {
        typ: *const Type,
    },
};

pub const Symbol = struct {
    name: String,
    kind: SymbolKind,

    const Self = @This();

    pub fn is(self: *const Self, tag: std.meta.Tag(SymbolKind)) bool {
        return @as(std.meta.Tag(SymbolKind), self.kind) == tag;
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

    pub fn define(self: *Self, name: String) !?*Symbol {
        if (self.symbols.contains(name)) {
            return null;
        }

        var symbol = try self.allocator.create(Symbol);
        symbol.* = Symbol{
            .name = name,
            .kind = .NotSet,
        };
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
                var ctx = Coroutine.current().getUserData() orelse unreachable;
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

    pub fn format(
        self: *const Self,
        fmt: String,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        try std.fmt.format(writer, "scope", .{});
        var it = self.symbols.iterator();
        while (it.next()) |sym| {
            try std.fmt.format(writer, "\n  {s} -> {any}", .{ sym.key, sym.value });
        }
    }
};
