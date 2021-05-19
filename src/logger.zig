const std = @import("std");
const term = @import("ansi-term");

pub fn log(
    comptime message_level: std.log.Level,
    comptime scope: @Type(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    var style = term.Style{ .foreground = .Red };
    style.foreground = switch (message_level) {
        .emerg, .alert, .crit => .White,

        .err => .Red,
        .warn => .Yellow,
        .notice => .Blue,
        .info => .White,
        .debug => .Green,
    };
    style.background = switch (message_level) {
        .emerg, .alert, .crit => .Red,
        else => .Black,
    };

    const stderr = std.io.getStdErr().writer();
    term.updateStyle(stderr, style, null) catch {};
    defer term.updateStyle(stderr, .{}, style) catch {};

    const level_txt = switch (message_level) {
        .emerg => "emergency",
        .alert => "alert",
        .crit => "critical",
        .err => "error",
        .warn => "warning",
        .notice => "notice",
        .info => "info",
        .debug => "debug",
    };
    const prefix2 = if (scope == .default) "" else ": " ++ @tagName(scope);
    const held = std.debug.getStderrMutex().acquire();
    defer held.release();
    nosuspend stderr.print("[" ++ level_txt ++ prefix2 ++ "] " ++ format ++ "\n", args) catch return;
}
