const std = @import("std");

usingnamespace @import("common.zig");
usingnamespace @import("location.zig");

pub const TokenKind = enum {
    Unknown,
    Newline,
    Colon,
    Comma,
    Period,
    Range,
    RangeInclusive,
    Spread,
    Bar,
    Ampersand,
    Bang,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Percent,
    PlusEqual,
    MinusEqual,
    AsteriskEqual,
    SlashEqual,
    PercentEqual,
    BraceLeft,
    BraceRight,
    ParenLeft,
    ParenRight,
    BracketLeft,
    BracketRight,
    GreaterEqual,
    Greater,
    LessEqual,
    Less,
    EqualEqual,
    Equal,
    NotEqual,
    Arrow,
    Underscore,
    KwLoop,
    KwFor,
    KwMatch,
    KwThen,
    KwElse,
    KwBreak,
    KwContinue,
    KwStruct,
    KwEnum,
    KwFn,
    Identifier,
    Char,
    String,
    Int,
    Float,
    DocComment,
};

pub const TokenData = union {
    none: void,
    char: u21,
    text: String,
    int: u128,
    float: f128,
};

pub const Token = struct {
    location: Location,
    kind: TokenKind,
    data: TokenData,

    const Self = @This();
};

pub const Lexer = struct {
    input: String,
    peeked: ?Token,
    location: Location,

    const Self = @This();

    pub fn init(filename: String, input: String) !Lexer {
        if (!std.unicode.utf8ValidateSlice(input)) {
            return error.InvalidUtf8;
        }
        return Lexer{
            .input = input,
            .peeked = null,
            .location = .{ .file = filename, .index = 0, .line = 1, .column = 1 },
        };
    }

    fn peekChar(self: *Self, offset: usize) ?u8 {
        const i = self.location.index + offset;
        if (i >= self.input.len) {
            return null;
        } else {
            return self.input[i];
        }
    }

    fn readChar(self: *Self) ?u8 {
        if (self.location.index >= self.input.len) {
            return null;
        } else {
            defer self.nextChar();
            return self.input[self.location.index];
        }
    }

    pub fn peek(self: *Self) ?Token {
        if (self.peeked == null) {
            self.peeked = self.parseNextToken();
        }
        return self.peeked;
    }

    pub fn read(self: *Self) ?Token {
        if (self.peeked) |token| {
            self.peeked = null;
            return token;
        }
        return self.parseNextToken();
    }

    fn nextChar(self: *Self) void {
        if (self.location.index < self.input.len and self.input[self.location.index] == '\n') {
            self.location.line += 1;
            self.location.column = 0;
        }
        self.location.index += 1;
        self.location.column += 1;
    }

    fn parseNextToken(self: *Self) ?Token {
        // skip whitespace and comments
        var newlineLocation: ?Location = null;
        while (self.location.index < self.input.len) {
            switch (self.input[self.location.index]) {
                ' ' => self.nextChar(),
                '\t' => self.nextChar(),
                '\r' => self.nextChar(),
                '\n' => {
                    newlineLocation = newlineLocation orelse self.location;
                    self.nextChar();
                },
                '#' => {
                    while (self.location.index < self.input.len) {
                        self.nextChar();
                        if (self.input[self.location.index - 1] == '\n') {
                            newlineLocation = newlineLocation orelse self.location;
                            break;
                        }
                    }
                },
                else => break,
            }
        }

        if (newlineLocation) |loc| {
            return Token{
                .location = loc,
                .kind = .Newline,
                .data = TokenData{ .none = {} },
            };
        }

        if (self.location.index >= self.input.len) {
            return null;
        }

        var token = Token{
            .location = self.location,
            .kind = .Unknown,
            .data = TokenData{ .none = {} },
        };

        switch (self.readChar().?) {
            ':' => token.kind = .Colon,
            ',' => token.kind = .Comma,
            '.' => if (self.peekChar(0)) |c1| {
                if (c1 == '.') {
                    if (self.peekChar(1)) |c2| {
                        switch (c2) {
                            '.' => {
                                self.nextChar();
                                self.nextChar();
                                token.kind = .Spread;
                            },
                            '=' => {
                                self.nextChar();
                                self.nextChar();
                                token.kind = .RangeInclusive;
                            },
                            else => {
                                self.nextChar();
                                token.kind = .Range;
                            },
                        }
                    } else {
                        self.nextChar();
                        token.kind = .Range;
                    }
                } else {
                    token.kind = .Period;
                }
            } else {
                token.kind = .Period;
            },
            '|' => token.kind = .Bar,
            '&' => token.kind = .Ampersand,
            '+' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.nextChar();
                    token.kind = .PlusEqual;
                } else {
                    token.kind = .Plus;
                }
            } else {
                token.kind = .Plus;
            },
            '-' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.nextChar();
                    token.kind = .MinusEqual;
                } else if (c1 == '>') {
                    self.nextChar();
                    token.kind = .Arrow;
                } else {
                    token.kind = .Minus;
                }
            } else {
                token.kind = .Minus;
            },
            '*' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.nextChar();
                    token.kind = .AsteriskEqual;
                } else {
                    token.kind = .Asterisk;
                }
            } else {
                token.kind = .Asterisk;
            },
            '/' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.nextChar();
                    token.kind = .SlashEqual;
                } else {
                    token.kind = .Slash;
                }
            } else {
                token.kind = .Slash;
            },
            '%' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.nextChar();
                    token.kind = .PercentEqual;
                } else {
                    token.kind = .Percent;
                }
            } else {
                token.kind = .Percent;
            },
            '{' => token.kind = .BraceLeft,
            '}' => token.kind = .BraceRight,
            '(' => token.kind = .ParenLeft,
            ')' => token.kind = .ParenRight,
            '[' => token.kind = .BracketLeft,
            ']' => token.kind = .BracketRight,
            '>' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.nextChar();
                    token.kind = .GreaterEqual;
                } else {
                    token.kind = .Greater;
                }
            } else {
                token.kind = .Greater;
            },
            '<' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.nextChar();
                    token.kind = .LessEqual;
                } else {
                    token.kind = .Less;
                }
            } else {
                token.kind = .Less;
            },
            '=' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.nextChar();
                    token.kind = .EqualEqual;
                } else {
                    token.kind = .Equal;
                }
            } else {
                token.kind = .Equal;
            },
            '!' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.nextChar();
                    token.kind = .NotEqual;
                } else {
                    token.kind = .Bang;
                }
            } else {
                token.kind = .Bang;
            },
            '_' => token.kind = .Underscore,
            '"' => if (self.parseString('"')) |text| {
                token.kind = .String;
                token.data = TokenData{ .text = text };
            } else {
                token.kind = .Unknown;
            },
            '\'' => if (self.parseString('\'')) |text| {
                var utf8View = std.unicode.Utf8View.initUnchecked(text).iterator();
                if (utf8View.nextCodepoint()) |c| {
                    if (utf8View.nextCodepoint()) |_| {
                        // text longer than one char
                    } else {
                        token.kind = .Char;
                        token.data = TokenData{ .char = c };
                    }
                } else {
                    // empty char literal
                    token.kind = .Unknown;
                }
            } else {
                token.kind = .Unknown;
            },
            //' ' => token.kind = .DocComment: []const u8,
            //' ' => token.kind = .Int: u128,
            //' ' => token.kind = .Float: f128,
            else => |c| {
                token.kind = .Unknown;

                if (isValidFirstIdentifierChar(c)) {
                    const start = self.location.index - 1; // index pointing to second char
                    while (self.location.index < self.input.len and isValidIdentifierChar(self.input[self.location.index])) {
                        self.nextChar();
                    }
                    token.kind = .Identifier;
                    token.data = TokenData{ .text = self.input[start..self.location.index] };
                } else if (c >= '0' and c <= '9') {
                    self.location.index -= 1;
                    self.parseNumber(&token);
                }
            },
        }

        return token;
    }

    fn parseNumber(self: *Self, token: *Token) void {
        var radix: u8 = 10;
        const startRaw = self.location.index;

        if (self.location.index + 1 < self.input.len and
            self.input[self.location.index] == '0')
        {
            switch (self.input[self.location.index + 1]) {
                'x' => {
                    radix = 16;
                    self.nextChar();
                    self.nextChar();
                },
                'b' => {
                    radix = 2;
                    self.nextChar();
                    self.nextChar();
                },
                else => {},
            }
        }

        const start = self.location.index;
        while (self.location.index < self.input.len and isDigit(self.input[self.location.index], radix)) {
            self.nextChar();
        }
        const end = self.location.index;

        // check if empty
        if (start == end) {
            std.log.err("Failed to parse int literal: '{s}'", .{self.input[startRaw..end]});
            return;
        }

        const number = std.fmt.parseInt(u128, self.input[start..end], radix) catch {
            std.log.err("Failed to parse int literal: '{s}'", .{self.input[startRaw..end]});
            return;
        };
        token.kind = .Int;
        token.data = TokenData{ .int = number };
    }

    fn parseString(self: *Self, end: u8) ?[]const u8 {
        // self.location.index already points to first byte of content
        const start = self.location.index;
        while (self.location.index < self.input.len) : (self.nextChar()) {
            if (self.input[self.location.index] == end) {
                self.nextChar();
                return self.input[start..(self.location.index - 1)];
            }
        }
        return null;
    }
};

fn isValidFirstIdentifierChar(c: u8) bool {
    return c == '$' or c == '@' or c == '_' or (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z');
}

fn isValidIdentifierChar(c: u8) bool {
    return c == '_' or (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or (c >= '0' and c <= '9');
}

fn isDigit(c: u8, radix: u8) bool {
    if (radix <= 10) {
        return c >= '0' and c < ('0' + radix);
    } else {
        return (c >= '0' and c <= '9') or (c >= 'a' and c < ('a' + radix - 10)) or (c >= 'A' and c < ('A' + radix - 10));
    }
}
