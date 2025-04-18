const std = @import("std");

const math = @import("math.zig");
const value = @import("value.zig");
const Character = @import("character.zig").Character;
const jerror = @import("error.zig");
const ParseError = jerror.ParseError;
const ParseOrAllocError = jerror.ParseOrAllocError;

pub const JsonParseResult = union(enum) {
    success: struct {
        value: value.Value,
        arena: std.heap.ArenaAllocator,
    },
    failure: struct {
        errorInfo: jerror.ParsingErrorInfo,
        allocator: std.mem.Allocator,
    },

    pub fn deinit(self: *JsonParseResult) void {
        switch (self.*) {
            .success => |*s| {
                s.arena.deinit();
            },
            .failure => |*f| {
                f.allocator.free(f.errorInfo);
            },
        }
    }
};

/// Options for parsing.
pub const ParseOptions = struct {
    /// The max parsing depth.
    ///
    /// If Reached, a ParseError.MaxDepthReached is thrown.
    maxDepth: ?u16 = 64,
};

/// Parses input into a Json value tree.
pub fn parse(input: []const u8, options: ParseOptions) error{OutOfMemory}!JsonParseResult {
    var ctx = ParseContext.init(input, options.maxDepth);
    const jsonData = parseValue(&ctx, 0) catch |e| {
        switch(e) {
            error.OutOfMemory => |outOfMemoryErr| return outOfMemoryErr,
            else => {
                ctx.arenaAlloc.deinit();
                return JsonParseResult{ .failure = .{
                    .errorInfo = ctx.err.?,
                    .allocator = ctx.errorMsgAllocator,
                } };
            }
        }
    };

    return JsonParseResult{ .success = .{
        .arena = ctx.arenaAlloc,
        .value = jsonData,
    } };
}

const Cursor = struct {
    input: []const u8,
    pos: usize = 0,

    fn peek(self: *Cursor) ?u8 {
        if (self.pos >= self.input.len) return null;
        const byte = self.input[self.pos];
        return byte;
    }

    fn peekOffset(self: *Cursor, n: usize) ?u8 {
        if (self.pos + n >= self.input.len) return null;
        return self.input[self.pos + n];
    }

    fn next(self: *Cursor) ?u8 {
        const index = self.pos + 1;
        if (index >= self.input.len) return null;
        return self.input[index];
    }

    fn skip(self: *Cursor, n: usize) void {
        self.pos += n;
    }
};

const ParseContext = struct {
    arenaAlloc: std.heap.ArenaAllocator,
    errorMsgAllocator: std.mem.Allocator,
    gpa: std.heap.DebugAllocator(.{}),
    cursor: Cursor,
    input: []const u8,
    maxDepth: ?u16,
    err: ?jerror.ParsingErrorInfo = null,

    pub fn init(input: []const u8, maxDepth: ?u16) ParseContext {
        var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
        return ParseContext{
            .gpa = gpa,
            .cursor = Cursor{ .input = input },
            .arenaAlloc = std.heap.ArenaAllocator.init(gpa.allocator()),
            .errorMsgAllocator = gpa.allocator(),
            .input = input,
            .maxDepth = maxDepth,
        };
    }

    fn throwErr(self: *ParseContext, err: ParseError, message: ?[]u8) ParseError {
        self.err = .{ .errorType = err, .message = message, .input = self.input };
        return err;
    }

    fn nextOrThrow(self: *ParseContext) ParseError!u8 {
        return self.cursor.next() orelse self.throwErr(ParseError.UnexpectedEOF, null);
    }

    fn peekOrThrow(self: *ParseContext) ParseError!u8 {
        return self.cursor.peek() orelse self.throwErr(ParseError.UnexpectedEOF, null);
    }

    fn consumeSequence(self: *ParseContext, seq: []const u8) ParseError!void {
        for (seq) |expectedByte| {
            const byte = try self.nextOrThrow();
            if (byte != expectedByte) {
                return ParseError.InvalidValue;
            }
        }
    }

    fn consumeChar(self: *ParseContext, char: Character) !void {
        const byte = try self.nextOrThrow();
        if (byte != char.toByte()) {
            return self.throwErr(ParseError.UnexpectedToken, try jerror.formatExpectMessage(self.errorMsgAllocator, &[_]Character{char}, byte));
        }
    }

    fn consumeWhitespace(self: *ParseContext) void {
        while (true) {
            const byte = self.cursor.peek() orelse break;
            if (Character.fromByte(byte).isWhitespace()) {
                self.cursor.skip(1);
            } else {
                break;
            }
        }
    }
};

fn guardMaxDepth(ctx: *ParseContext, depth: u16) ParseError!void {
    if (ctx.maxDepth) |maxDepth| {
        if (depth > maxDepth) return ctx.throwErr(ParseError.MaxDepthReached, null);
    }
}

fn parseObject(ctx: *ParseContext, depth: u16) ParseOrAllocError!value.Object {
    try guardMaxDepth(ctx, depth);
    try ctx.consumeChar(Character.leftBrace);
    var object: value.Object = std.StringHashMap(value.Value).init(ctx.arenaAlloc.allocator());
    ctx.consumeWhitespace();
    if (try ctx.peekOrThrow() == Character.rightBrace.toByte()) {
        return object;
    }
    while (true) {
        const key = try parseString(ctx);
        ctx.consumeWhitespace();
        try ctx.consumeChar(Character.colon);
        ctx.consumeWhitespace();
        const val = try parseValue(ctx, depth);
        try object.put(key, val);
        ctx.consumeWhitespace();
        if (try ctx.peekOrThrow() == Character.comma.toByte()) {
            try ctx.consumeChar(Character.comma);
            ctx.consumeWhitespace();
            continue;
        } else {
            ctx.consumeWhitespace();
            try ctx.consumeChar(Character.rightBrace);
            break;
        }
    }

    return object;
}

fn parseArray(ctx: *ParseContext, depth: u16) ParseOrAllocError!value.Array {
    try guardMaxDepth(ctx, depth);
    ctx.consumeWhitespace();
    unreachable;
}

fn parseString(ctx: *ParseContext) ParseOrAllocError!value.String {
    const cursor = &ctx.cursor;
    const byte = cursor.next() orelse return ctx.throwErr(ParseError.UnexpectedEOF, null);

    if (byte != Character.doubleQuotes.toByte()) return ctx.throwErr(ParseError.UnexpectedToken, try jerror.formatExpectMessage(ctx.errorMsgAllocator, &[_]Character{Character.doubleQuotes}, byte));
    const startIndex = cursor.pos + 1;
    while (cursor.peek() != Character.doubleQuotes.toByte()) {
        const stringByte = cursor.next();
        if (stringByte == null) return ctx.throwErr(ParseError.UnexpectedEOF, null);
        if (stringByte == Character.backwardSlash.toByte()) {
            try handleEscapeCharacter(ctx);
        }
    }
    if (cursor.pos == startIndex) {
        return "";
    }
    cursor.skip(1);
    const endIndex = cursor.pos;
    return cursor.input[startIndex..endIndex];
}

fn handleEscapeCharacter(ctx: *ParseContext) ParseOrAllocError!void {
    const cursor = &ctx.cursor;
    const byte = cursor.peek() orelse return ctx.throwErr(ParseError.InvalidUnicodeSequence, null);
    const char = Character.fromByte(byte);
    const expectedChars = &[_]Character{ .doubleQuotes, .forwardSlash, .backwardSlash, .b, .f, .n, .r, .t, .u };
    switch (char) {
        .doubleQuotes, .forwardSlash, .backwardSlash, .b, .f, .n, .r, .t => cursor.skip(1),
        .u => {
            cursor.skip(1);
            for (1..4) |offset| {
                const hexDigit = cursor.peekOffset(offset) orelse return ctx.throwErr(ParseError.UnexpectedEOF, null);
                if (!Character.fromByte(hexDigit).isHex()) {
                    const message = try std.fmt.allocPrint(ctx.errorMsgAllocator, "expected an hex value, but got '{c}'", .{byte});
                    return ctx.throwErr(ParseError.InvalidUnicodeSequence, message);
                }
            }
            cursor.skip(4);
        },
        else => return ctx.throwErr(ParseError.InvalidUnicodeSequence, try jerror.formatExpectMessage(ctx.errorMsgAllocator, expectedChars, byte)),
    }
}

fn validateHexDigit(ctx: *ParseContext, byte: u8) ParseError!void {
    return switch (byte) {
        Character.zero.toByte()...Character.nine.to, Character.a...Character.f, Character.A...Character.F => {},
        else => {
            const message = std.fmt.allocPrint(ctx.errorMsgAllocator, "expected an hex value, but got '{c}'", .{byte});
            return ctx.throwErr(ParseError.InvalidUnicodeSequence, message);
        },
    };
}

fn parseNum(_: *ParseContext) ParseError!value.Value {
    unreachable;
}

fn parseValue(ctx: *ParseContext, depth: u16) ParseOrAllocError!value.Value {
    ctx.consumeWhitespace();
    const expectedChars = &[_]Character{ Character.leftBracket, Character.leftBrace };
    const byte = ctx.cursor.peek() orelse return ctx.throwErr(ParseError.UnexpectedEOF, null);
    const char = Character.fromByte(byte);

    if (char.isDigitOrMinus()) {
        return parseNum(ctx);
    }

    return switch (char) {
        .leftBracket => value.Value{ .array = try parseArray(ctx, depth + 1) },
        .leftBrace => value.Value{ .object = try parseObject(ctx, depth + 1) },
        .t => {
            ctx.consumeSequence("true") catch |err| return ctx.throwErr(err, null);
            return value.Value{ .bool = true };
        },
        .f => {
            ctx.consumeSequence("false") catch |err| return ctx.throwErr(err, null);
            return value.Value{ .bool = false };
        },
        .n => {
            ctx.consumeSequence("null") catch |err| return ctx.throwErr(err, null);
            return value.Value{ .null = {} };
        },
        .doubleQuotes => value.Value{ .string = try parseString(ctx) },
        else => return ctx.throwErr(ParseError.UnexpectedToken, try jerror.formatExpectMessage(ctx.errorMsgAllocator, expectedChars, byte)),
    };
}
