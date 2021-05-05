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
            .Block => |block| {
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

            .Float => |float| try std.fmt.format(writer, "{}", .{float.value}),

            .Identifier => |id| try writer.writeAll(id.name),

            .Int => |int| try std.fmt.format(writer, "{}", .{int.value}),

            .Pipe => |*pipe| {
                try self.format(writer, pipe.left, indent);
                try writer.writeAll(" -> ");
                try self.format(writer, pipe.right, indent);
            },

            .String => |text| try std.fmt.format(writer, "\"{s}\"", .{text.value}),

            //else => try writer.writeAll("<Unknown>"),
        }
    }
};
