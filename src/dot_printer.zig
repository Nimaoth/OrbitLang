const std = @import("std");

usingnamespace @import("ast.zig");
usingnamespace @import("types.zig");

//digraph graphname {
//    "A" -> {B C}
//    "A" -> X
//    X -> " lol hi"
//}
pub const DotPrinter = struct {
    printTypes: bool,

    const Self = @This();

    pub fn init(writer: anytype, printTypes: bool) !Self {
        try writer.writeAll("digraph Ast {\n");
        return Self{
            .printTypes = printTypes,
        };
    }

    pub fn deinit(self: *Self, writer: anytype) void {
        writer.writeAll("}") catch {};
    }

    fn newLine(self: *Self, writer: anytype) anyerror!void {
        try writer.writeAll("\n    ");
    }

    fn printNode(self: *Self, writer: anytype, ast: *Ast) anyerror!void {
        const UnionTagType = @typeInfo(AstSpec).Union.tag_type.?;
        try std.fmt.format(writer, "\"{s} #{}", .{ @tagName(@as(UnionTagType, ast.spec)), ast.id });
        if (self.printTypes) {
            try std.fmt.format(writer, "\\n", .{});
            try self.printType(writer, ast.typ);
        }
        try self.printNodeArg(writer, ast);
        try std.fmt.format(writer, "\"", .{});
    }

    fn printConnection(self: *Self, writer: anytype, from: *Ast, to: *Ast) anyerror!void {
        try writer.writeAll("    ");
        try self.printNode(writer, from);
        try writer.writeAll(" -> ");
        try self.printNode(writer, to);
        try writer.writeAll("\n");
    }

    pub fn printGraph(self: *Self, writer: anytype, _ast: *Ast) anyerror!void {
        switch (_ast.spec) {

            //
            .Access => |*ast| {
                try self.printConnection(writer, _ast, ast.left);
                try self.printConnection(writer, _ast, ast.right);
                try self.printGraph(writer, ast.left);
                try self.printGraph(writer, ast.right);
            },

            .Assignment => |*ast| {
                try self.printConnection(writer, _ast, ast.pattern);
                try self.printConnection(writer, _ast, ast.value);
                try self.printGraph(writer, ast.pattern);
                try self.printGraph(writer, ast.value);
            },

            .Block => |*ast| {
                for (ast.body.items) |expr| {
                    try self.printConnection(writer, _ast, expr);
                    try self.printGraph(writer, expr);
                }
            },

            .Call => |*ast| {
                try self.printConnection(writer, _ast, ast.func);
                try self.printGraph(writer, ast.func);
                for (ast.args.items) |expr| {
                    try self.printConnection(writer, _ast, expr);
                    try self.printGraph(writer, expr);
                }
            },

            .ConstDecl => |*ast| {
                try self.printConnection(writer, _ast, ast.pattern);
                try self.printGraph(writer, ast.pattern);
                if (ast.typ) |typ| {
                    try self.printConnection(writer, _ast, typ);
                    try self.printGraph(writer, typ);
                }
                try self.printConnection(writer, _ast, ast.value);
                try self.printGraph(writer, ast.value);
            },

            .VarDecl => |*ast| {
                try self.printConnection(writer, _ast, ast.pattern);
                try self.printGraph(writer, ast.pattern);
                if (ast.typ) |typ| {
                    try self.printConnection(writer, _ast, typ);
                    try self.printGraph(writer, typ);
                }
                if (ast.value) |val| {
                    try self.printConnection(writer, _ast, val);
                    try self.printGraph(writer, val);
                }
            },

            .Lambda => |*ast| {
                for (ast.args.items) |expr| {
                    try self.printConnection(writer, _ast, expr);
                    try self.printGraph(writer, expr);
                }
                try self.printConnection(writer, _ast, ast.body);
                try self.printGraph(writer, ast.body);
            },

            .Pipe => |*ast| {
                try self.printConnection(writer, _ast, ast.left);
                try self.printConnection(writer, _ast, ast.right);
                try self.printGraph(writer, ast.left);
                try self.printGraph(writer, ast.right);
            },

            .Tuple => |*ast| {
                for (ast.values.items) |expr| {
                    try self.printConnection(writer, _ast, expr);
                    try self.printGraph(writer, expr);
                }
            },

            else => {},
        }
    }

    pub fn printNodeArg(self: *Self, writer: anytype, ast: *Ast) anyerror!void {
        switch (ast.spec) {
            .Access => |*acc| try std.fmt.format(writer, "\\n.", .{}),
            .Assignment => |*ass| try std.fmt.format(writer, "\\n=", .{}),
            .Block => |*block| try std.fmt.format(writer, "\\n{{}}", .{}),
            .Call => |*call| try std.fmt.format(writer, "\\n()", .{}),
            .ConstDecl => |*decl| try std.fmt.format(writer, "\\n::", .{}),
            .VarDecl => |*decl| try std.fmt.format(writer, "\\n:=", .{}),
            .Float => |float| try std.fmt.format(writer, "\\n{}", .{float.value}),
            .Identifier => |id| try std.fmt.format(writer, "\\n{s}", .{id.name}),
            .Int => |int| try std.fmt.format(writer, "\\n{}", .{int.value}),
            .Lambda => |*lambda| try std.fmt.format(writer, "\\n||", .{}),
            .Pipe => |*pipe| try std.fmt.format(writer, "\\n->", .{}),
            .String => |text| try std.fmt.format(writer, "\\n{s}", .{text.value}),
            .Tuple => |*tuple| try std.fmt.format(writer, "\\n()", .{}),

            //else => try writer.writeAll("<Unknown>"),
        }
    }

    fn printType(self: *Self, writer: anytype, typ: *const Type) anyerror!void {
        switch (typ.kind) {
            .Unknown => try std.fmt.format(writer, "<Unknown>", .{}),
            .Void => try std.fmt.format(writer, "void", .{}),
            .Unreachable => try std.fmt.format(writer, "unreachable", .{}),
            .Bool => try std.fmt.format(writer, "b{}", .{typ.size * 8}),
            .Float => try std.fmt.format(writer, "f{}", .{typ.size * 8}),
            .Int => |*int| try std.fmt.format(writer, "{s}{}", .{
                (if (int.signed) "i" else "u"),
                typ.size * 8,
            }),
            .Pointer => |*ptr| {
                try std.fmt.format(writer, "^", .{});
                try self.printType(writer, ptr.child);
            },
            .Slice => |*slc| {
                try std.fmt.format(writer, "[]", .{});
                try self.printType(writer, slc.child);
            },
            .Array => |*arr| {
                switch (arr.len) {
                    .Known => |len| try std.fmt.format(writer, "[{}]", .{len}),
                    .Unknown => try std.fmt.format(writer, "[?]", .{}),
                    .Generic => |name| try std.fmt.format(writer, "[{s}]", .{name}),
                }
                try self.printType(writer, arr.child);
            },
            .Struct => |*str| {
                try std.fmt.format(writer, "struct", .{});
            },

            //else => try writer.writeAll("<Unknown>"),
        }
    }
};
