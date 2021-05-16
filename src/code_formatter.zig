const std = @import("std");

usingnamespace @import("ast.zig");

pub const AstFormatter = struct {
    indentSize: usize = 4,

    const Self = @This();

    pub fn init() Self {
        return Self{};
    }

    fn newLine(self: *AstFormatter, writer: anytype, indent: u64) anyerror!void {
        var i: usize = 0;
        try writer.writeAll("\n");
        while (i < indent * self.indentSize) : (i += 1) {
            try writer.writeAll(" ");
        }
    }

    pub fn format(self: *AstFormatter, writer: anytype, ast: *Ast, indent: u64) anyerror!void {
        switch (ast.spec) {

            //
            .Access => |*acc| {
                try writer.writeAll("[");
                try self.format(writer, acc.left, indent);
                try writer.writeAll("].[");
                try self.format(writer, acc.right, indent);
                try writer.writeAll("]");
            },

            .Assignment => |*ass| {
                try self.format(writer, ass.pattern, indent);
                try writer.writeAll(" = ");
                try self.format(writer, ass.value, indent);
            },

            .Block => |*block| {
                try writer.writeAll("{");
                for (block.body.items) |arg, i| {
                    try self.newLine(writer, indent + 1);
                    try self.format(writer, arg, indent + 1);
                }
                if (block.body.items.len > 0) {
                    try self.newLine(writer, indent);
                }
                try writer.writeAll("}");
            },

            .Call => |*call| {
                try self.format(writer, call.func, indent);
                try writer.writeAll("(");
                for (call.args.items) |arg, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try self.format(writer, arg, indent);
                }
                try writer.writeAll(")");
            },

            .ConstDecl => |*decl| {
                try self.format(writer, decl.pattern, indent);
                try writer.writeAll(" :");
                if (decl.typ) |typ| {
                    try writer.writeAll(" ");
                    try self.format(writer, typ, indent);
                    try writer.writeAll(" ");
                }
                try writer.writeAll(": ");
                try self.format(writer, decl.value, indent);
            },

            .VarDecl => |*decl| {
                try self.format(writer, decl.pattern, indent);
                try writer.writeAll(" :");
                if (decl.typ) |typ| {
                    try writer.writeAll(" ");
                    try self.format(writer, typ, indent);
                    try writer.writeAll(" ");
                }
                if (decl.value) |value| {
                    try writer.writeAll("= ");
                    try self.format(writer, value, indent);
                }
            },

            .Float => |float| try std.fmt.format(writer, "{}", .{float.value}),
            .Function => |float| try std.fmt.format(writer, "@fn", .{}),

            .Identifier => |id| try writer.writeAll(id.name),

            .Int => |int| try std.fmt.format(writer, "{}", .{int.value}),

            .Lambda => |*lambda| {
                try writer.writeAll("|");
                for (lambda.args.items) |arg, i| {
                    if (i > 0) {
                        try writer.writeAll(", ");
                    }
                    try self.format(writer, arg, indent);
                }
                try writer.writeAll("| ");
                try self.format(writer, lambda.body, indent);
            },

            .Pipe => |*pipe| {
                try self.format(writer, pipe.left, indent);
                try writer.writeAll(" -> ");
                try self.format(writer, pipe.right, indent);
            },

            .String => |text| try std.fmt.format(writer, "\"{s}\"", .{text.value}),

            .Tuple => |*tuple| {
                try writer.writeAll(".(");
                for (tuple.values.items) |value, i| {
                    if (i > 0) {
                        try writer.writeAll(", ");
                    }
                    try self.format(writer, value, indent);
                }
                try writer.writeAll(")");
            },

            //else => try writer.writeAll("<Unknown>"),
        }
    }
};
