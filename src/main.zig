const std = @import("std");

usingnamespace @import("lexer.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("error_handler.zig");

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    const examples = try std.fs.cwd().openDir("examples", .{});
    const fileContent = try examples.readFileAlloc(allocator, "test.orb", std.math.maxInt(usize));
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
    std.debug.print("\n", .{});

    lexer = try Lexer.init(fileContent);
    var errorReporter = ConsoleErrorReporter{};
    var parser = Parser.init(lexer, allocator, &errorReporter.reporter);
    defer parser.deinit();

    while (parser.parseExpression()) |expr| {
        std.log.info("{any}", .{expr});
    }
}
