const std = @import("std");

usingnamespace @import("lexer.zig");

const Name = []const u8;

const AstSpec = union(enum) {
    Identifier: struct {
        name: Name,
    },
    Number: struct {
        value: i64,
    },
    Call: struct {
        func: *Ast,
        args: std.ArrayList(*Ast),
    },
};

const Ast = struct {
    spec: AstSpec,
};

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    const examples = try std.fs.cwd().openDir("examples", .{});
    const fileContent = try examples.readFileAlloc(allocator, "ideas.orb", std.math.maxInt(usize));
    defer allocator.free(fileContent);

    var lexer = try Lexer.init(fileContent);

    while (lexer.read()) |token| {
        switch (token.kind) {
            .Identifier => |name| {
                std.log.info("{s}", .{name});
            },
            .String => |name| {
                std.log.info("\"{s}\"", .{name});
            },
            .Char => |c| {
                var buff: [4]u8 = undefined;
                const bytes = try std.unicode.utf8Encode(c, buff[0..buff.len]);
                std.log.info("'{s}'", .{buff[0..bytes]});
            },
            else => {
                std.log.info("{any}", .{token});
            },
        }
    }
}
