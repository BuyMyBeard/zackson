const std = @import("std");

pub const Character = enum(u8) {
    nul = '\x00',
    soh = '\x01',
    stx = '\x02',
    etx = '\x03',
    eot = '\x04',
    enq = '\x05',
    ack = '\x06',
    bel = '\x07',
    backspace = '\x08',
    tab = '\t',
    newline = '\n',
    verticalTab = '\x0B',
    formFeed = '\x0C',
    cariageReturn = '\r',
    shiftOut = '\x0E',
    shiftIn = '\x0F',
    dataLinkEscape = '\x10',
    deviceControl1 = '\x11',
    deviceControl2 = '\x12',
    deviceControl3 = '\x13',
    deviceControl4 = '\x14',
    negativeAck = '\x15',
    syncIdle = '\x16',
    endTransmissionBlock = '\x17',
    cancel = '\x18',
    endOfMedium = '\x19',
    substitute = '\x1A',
    escape = '\x1B',
    fileSeparator = '\x1C',
    groupSeparator = '\x1D',
    recordSeparator = '\x1E',
    unitSeparator = '\x1F',

    space = ' ',
    exclamation = '!',
    doubleQuotes = '"',
    hash = '#',
    dollar = '$',
    percent = '%',
    ampersand = '&',
    singleQuote = '\'',
    leftParen = '(',
    rightParen = ')',
    asterisk = '*',
    plus = '+',
    comma = ',',
    minus = '-',
    dot = '.',
    forwardSlash = '/',
    zero = '0',
    one = '1',
    two = '2',
    three = '3',
    four = '4',
    five = '5',
    six = '6',
    seven = '7',
    eight = '8',
    nine = '9',
    colon = ':',
    semicolon = ';',
    lessThan = '<',
    equal = '=',
    greaterThan = '>',
    question = '?',
    at = '@',
    A = 'A',
    B = 'B',
    C = 'C',
    D = 'D',
    E = 'E',
    F = 'F',
    G = 'G',
    H = 'H',
    I = 'I',
    J = 'J',
    K = 'K',
    L = 'L',
    M = 'M',
    N = 'N',
    O = 'O',
    P = 'P',
    Q = 'Q',
    R = 'R',
    S = 'S',
    T = 'T',
    U = 'U',
    V = 'V',
    W = 'W',
    X = 'X',
    Y = 'Y',
    Z = 'Z',
    leftBracket = '[',
    backwardSlash = '\\',
    rightBracket = ']',
    caret = '^',
    underscore = '_',
    backtick = '`',
    a = 'a',
    b = 'b',
    c = 'c',
    d = 'd',
    e = 'e',
    f = 'f',
    g = 'g',
    h = 'h',
    i = 'i',
    j = 'j',
    k = 'k',
    l = 'l',
    m = 'm',
    n = 'n',
    o = 'o',
    p = 'p',
    q = 'q',
    r = 'r',
    s = 's',
    t = 't',
    u = 'u',
    v = 'v',
    w = 'w',
    x = 'x',
    y = 'y',
    z = 'z',
    leftBrace = '{',
    pipe = '|',
    rightBrace = '}',
    tilde = '~',
    del = '\x7F',
    ///  Do not use. Catch-all to avoid enum errors.
    unknown = 255,

    pub fn toByte(self: Character) u8 {
        return @intFromEnum(self);
    }

    /// If used on a value above 127, returns Character.unknown
    pub fn fromByte(byte: u8) Character {
        if (byte > 127) return .unknown;
        return @enumFromInt(byte);
    }

    pub fn isHex(self: Character) bool {
        return switch(self) {
          .a, .b, .c, .d, .e, .f, 
          .A, .B, .C, .D, .E, .G,
          .zero, .one, .two, .three, .four, 
          .five, .six, .seven, .eight, .nine => true,
          else => false,
        };
    }

    pub fn isWhitespace(self: Character) bool {
        return switch(self) {
            .space,
            .tab,
            .newline,
            .cariageReturn => true,
            else => false,
        };
    }

    pub fn isDigitOrMinus(self: Character) bool {
        return self.isDigit() or self == Character.minus;
    }

    pub fn isDigit(self: Character) bool {
        return switch(self) {
            .zero, .one, .two, .three, .four,
            .five, .six, .seven, .eight, .nine => true,
            else => false,
        };
    }

    pub fn isControlCharacter(self: Character) bool {
        return (self.toByte() <= Character.unitSeparator.toByte());
    }

    pub fn join(alloc: std.mem.Allocator, items: []const Character, separator: []const u8) ![]u8 {
        var list = std.ArrayList(u8).init(alloc);

        const writer = list.writer();

        for (items, 0..) |item, index| {
            try writer.print("{c}", .{item.toByte()});
            if (index < items.len - 1) {
                try writer.print("{s}", .{separator});
            }
        }

        return list.toOwnedSlice();
    }

    /// Caller must ensure `buf.len >= 1`. Behavior is undefined otherwise.
    pub fn toSlice(self: Character, buf: []u8) []const u8 {
        std.debug.assert(buf.len >= 1);
        buf[0] = self.toByte();
        return buf[0..1];
    }
    /// Caller must ensure `buf.len >= 2`. Behavior is undefined otherwise.
    pub fn toEscapedSlice(self: Character, buf: []u8) []const u8 {
        std.debug.assert(buf.len >= 2);
        return switch(self) {
            .backspace => "\\b",
            .formFeed => "\\f",
            .newline => "\\n",
            .cariageReturn => "\\r",
            .tab => "\\t",
            else => |char| {
                if (char.isControlCharacter()) {
                    return std.fmt.bufPrint(buf, "\\u{X:0>4}", .{char.toByte()}) catch unreachable;
                } else {
                    return char.toSlice(buf);
                }
            }
        };
    }

    pub fn fromCodepoint(cp: u21) ?Character {
        if (cp <= Character.del.toByte()) {
            const byte : u8 = @intCast(cp);
            return Character.fromByte(byte);
        }
        return null;
    }
};
