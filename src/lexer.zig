const std = @import("std");

pub const TokenKind = union(enum) {
    Unknown,
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
    Char: u21,
    Identifier: []const u8,
    String: []const u8,
    DocComment: []const u8,
    IntLiteral: u128,
    FloatLiteral: f128,
};

pub const Token = struct {
    kind: TokenKind,

    const Self = @This();
};

pub const Lexer = struct {
    input: []const u8,
    index: usize,
    peeked: ?Token,

    const Self = @This();

    pub fn init(input: []const u8) !Lexer {
        if (!std.unicode.utf8ValidateSlice(input)) {
            return error.InvalidUtf8;
        }
        return Lexer{
            .input = input,
            .index = 0,
            .peeked = null,
        };
    }

    fn peekChar(self: *Self, offset: usize) ?u8 {
        const i = self.index + offset;
        if (i >= self.input.len) {
            return null;
        } else {
            return self.input[i];
        }
    }

    fn readChar(self: *Self) ?u8 {
        if (self.index >= self.input.len) {
            return null;
        } else {
            defer self.index += 1;
            return self.input[self.index];
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

    fn parseNextToken(self: *Self) ?Token {
        // skip whitespace and comments
        while (self.index < self.input.len) {
            switch (self.input[self.index]) {
                ' ' => self.index += 1,
                '\t' => self.index += 1,
                '\r' => self.index += 1,
                '\n' => self.index += 1,
                '#' => {
                    while (self.index < self.input.len) {
                        self.index += 1;
                        if (self.input[self.index - 1] == '\n') {
                            break;
                        }
                    }
                },
                else => break,
            }
        }

        if (self.index >= self.input.len) {
            return null;
        }

        var token = Token{ .kind = .Unknown };

        switch (self.readChar().?) {
            ':' => token.kind = .Colon,
            ',' => token.kind = .Comma,
            '.' => if (self.peekChar(0)) |c1| {
                if (c1 == '.') {
                    if (self.peekChar(1)) |c2| {
                        switch (c2) {
                            '.' => {
                                self.index += 2;
                                token.kind = .Spread;
                            },
                            '=' => {
                                self.index += 2;
                                token.kind = .RangeInclusive;
                            },
                            else => {
                                self.index += 1;
                                token.kind = .Range;
                            },
                        }
                    } else {
                        self.index += 1;
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
                    self.index += 1;
                    token.kind = .PlusEqual;
                } else {
                    token.kind = .Plus;
                }
            } else {
                token.kind = .Plus;
            },
            '-' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.index += 1;
                    token.kind = .MinusEqual;
                } else if (c1 == '>') {
                    self.index += 1;
                    token.kind = .Arrow;
                } else {
                    token.kind = .Minus;
                }
            } else {
                token.kind = .Minus;
            },
            '*' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.index += 1;
                    token.kind = .AsteriskEqual;
                } else {
                    token.kind = .Asterisk;
                }
            } else {
                token.kind = .Asterisk;
            },
            '/' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.index += 1;
                    token.kind = .SlashEqual;
                } else {
                    token.kind = .Slash;
                }
            } else {
                token.kind = .Slash;
            },
            '%' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.index += 1;
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
                    self.index += 1;
                    token.kind = .GreaterEqual;
                } else {
                    token.kind = .Greater;
                }
            } else {
                token.kind = .Greater;
            },
            '<' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.index += 1;
                    token.kind = .LessEqual;
                } else {
                    token.kind = .Less;
                }
            } else {
                token.kind = .Less;
            },
            '=' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.index += 1;
                    token.kind = .EqualEqual;
                } else {
                    token.kind = .Equal;
                }
            } else {
                token.kind = .Equal;
            },
            '!' => if (self.peekChar(0)) |c1| {
                if (c1 == '=') {
                    self.index += 1;
                    token.kind = .NotEqual;
                } else {
                    token.kind = .Bang;
                }
            } else {
                token.kind = .Bang;
            },
            '_' => token.kind = .Underscore,
            '"' => if (self.parseString('"')) |text| {
                token.kind = TokenKind{ .String = text };
            } else {
                token.kind = .Unknown;
            },
            '\'' => if (self.parseString('\'')) |text| {
                var utf8View = std.unicode.Utf8View.initUnchecked(text).iterator();
                if (utf8View.nextCodepoint()) |c| {
                    if (utf8View.nextCodepoint()) |_| {
                        // text longer than one char
                    } else {
                        token.kind = TokenKind{ .Char = c };
                    }
                } else {
                    // empty char literal
                    token.kind = .Unknown;
                }
            } else {
                token.kind = .Unknown;
            },
            //' ' => token.kind = .DocComment: []const u8,
            //' ' => token.kind = .IntLiteral: u128,
            //' ' => token.kind = .FloatLiteral: f128,
            else => |c| {
                token.kind = .Unknown;

                if (isValidFirstIdentifierChar(c)) {
                    const start = self.index - 1; // index pointing to second char
                    while (self.index < self.input.len and isValidIdentifierChar(self.input[self.index])) {
                        self.index += 1;
                    }
                    token.kind = TokenKind{ .Identifier = self.input[start..self.index] };
                } else {}
            },
        }

        return token;
    }

    fn parseString(self: *Self, end: u8) ?[]const u8 {
        // self.index already points to first byte of content
        const start = self.index;
        while (self.index < self.input.len) : (self.index += 1) {
            if (self.input[self.index] == end) {
                self.index += 1;
                return self.input[start..(self.index - 1)];
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
