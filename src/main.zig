const std = @import("std");

const clap = @import("clap");

usingnamespace @import("lexer.zig");
usingnamespace @import("parser.zig");
usingnamespace @import("error_handler.zig");
usingnamespace @import("code_formatter.zig");

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator = &gpa.allocator;

    const params = comptime [_]clap.Param(clap.Help){
        clap.parseParam("-h, --help             Display this help and exit.") catch unreachable,
        clap.parseParam("-c, --compile          Compile input files.") catch unreachable,
        clap.parseParam("-l, --lex              Only lex input files.") catch unreachable,
        clap.parseParam("-p, --parse            Only parse input files.") catch unreachable,
        clap.parseParam("<POS>...") catch unreachable,
    };

    var diag = clap.Diagnostic{};
    var args = clap.parse(clap.Help, &params, allocator, &diag) catch |err| {
        diag.report(std.io.getStdErr().writer(), err) catch {};
        return err;
    };
    defer args.deinit();

    var files = std.ArrayList([]const u8).init(allocator);
    defer files.deinit();

    for (args.positionals()) |pos| {
        std.log.info("{s}", .{pos});
        try files.append(pos);
    }

    if (args.flag("--lex")) {
        try lexFiles(files.items, allocator);
        return;
    }

    if (args.flag("--parse")) {
        try parseFiles(files.items, allocator);
        return;
    }

    if (args.flag("--compile")) {
        try compileFiles(files.items, allocator);
        return;
    }
}

pub fn compileFiles(files: [][]const u8, allocator: *std.mem.Allocator) anyerror!void {
    return error.NotImplemented;
}

pub fn parseFiles(files: [][]const u8, _allocator: *std.mem.Allocator) anyerror!void {
    const T = struct {
        pub fn parseFile(file: []const u8, allocator: *std.mem.Allocator) anyerror!void {
            const fileContent = try std.fs.cwd().readFileAlloc(allocator, file, std.math.maxInt(usize));
            defer allocator.free(fileContent);

            var lexer = try Lexer.init(fileContent);
            var errorReporter = ConsoleErrorReporter{};
            var parser = Parser.init(lexer, allocator, &errorReporter.reporter);
            defer parser.deinit();

            var astFormatter = AstFormatter.init();
            while (try parser.parseExpression()) |expr| {
                try astFormatter.format(std.io.getStdOut().writer(), expr, 0);
                try std.io.getStdOut().writer().writeAll("\n");
            }

            std.debug.print("\n", .{});
        }
    };

    if (files.len == 0) {
        T.parseFile("./examples/test.orb", _allocator) catch |err| {
            std.log.err("Failed to parse file {s}: {any}", .{ "./examples/test.orb", err });
        };
    } else {
        for (files) |file| {
            T.parseFile(file, _allocator) catch |err| {
                std.log.err("Failed to parse file {s}: {any}", .{ file, err });
            };
        }
    }
}

pub fn lexFiles(files: [][]const u8, _allocator: *std.mem.Allocator) anyerror!void {
    const T = struct {
        pub fn lexFile(file: []const u8, allocator: *std.mem.Allocator) anyerror!void {
            const fileContent = try std.fs.cwd().readFileAlloc(allocator, file, std.math.maxInt(usize));
            defer allocator.free(fileContent);

            var lexer = try Lexer.init(fileContent);

            while (lexer.read()) |token| {
                switch (token.kind) {
                    .Identifier => {
                        std.log.info("{s}", .{token.data.text});
                    },
                    .String => {
                        std.log.info("\"{s}\"", .{token.data.text});
                    },
                    .Char => {
                        var buff: [4]u8 = undefined;
                        const bytes = try std.unicode.utf8Encode(token.data.char, buff[0..buff.len]);
                        std.log.info("'{s}'", .{buff[0..bytes]});
                    },
                    else => {
                        std.log.info("{any}", .{token});
                    },
                }
            }
            std.debug.print("\n", .{});
        }
    };

    if (files.len == 0) {
        T.lexFile("./examples/test.orb", _allocator) catch |err| {
            std.log.err("Failed to lex file {s}: {any}", .{ "./examples/test.orb", err });
        };
    } else {
        for (files) |file| {
            T.lexFile(file, _allocator) catch |err| {
                std.log.err("Failed to lex file {s}: {any}", .{ file, err });
            };
        }
    }
}
