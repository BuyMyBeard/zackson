const std = @import("std");

const math = @import("math.zig");
const value = @import("value.zig");
const Character = @import("character.zig").Character;
const jerror = @import("error.zig");
const ParseError = jerror.ParseError;
const ParseOrAllocError = jerror.ParseOrAllocError;


/// Represents the result of a JSON parsing operation.
///
/// This tagged union separates parsing **success**, **structured failures**, and
/// **unrecoverable allocation errors** (which are thrown separately via `AllocError`).
pub const JsonParseResult = union(enum) {
    /// Parsing succeeded.
    ///
    /// The parsed value is available in `value`, and the arena allocator used for all
    /// internal allocations is available as `arena`. The caller is responsible for calling `deinit()`.
    success: struct {
        value: value.Value,
        arena: std.heap.ArenaAllocator,
    },

    /// Parsing failed due to a structural or semantic issue (not memory).
    ///
    /// Error details such as error type, line/column, and message are stored in `errorInfo`.
    /// The allocator used to create the error message (if any) is returned for cleanup.
    failure: struct {
        errorInfo: jerror.ParsingErrorInfo,
        allocator: std.mem.Allocator,
    },

    /// Deinitializes the result:
    /// - If `.success`, deinitializes the arena and frees associated memory.
    /// - If `.failure`, frees the memory allocated for the error message.
    pub fn deinit(self: *JsonParseResult) void {
        switch (self.*) {
            .success => |*s| s.arena.deinit(),
            .failure => |*f| f.allocator.free(f.errorInfo),
        }
    }
};

/// Configuration options for controlling JSON parsing behavior.
pub const ParseOptions = struct {
    /// Maximum allowed nesting depth for arrays and objects.
    ///
    /// If this depth is exceeded during parsing,
    /// a `ParseError.MaxDepthReached` is thrown.
    ///
    /// Defaults to 64.
    maxDepth: u16 = 64,

    /// Maximum size (in bytes) for any parsed string value (e.g. string literals, numbers as strings, etc.).
    ///
    /// This limit does **not** apply to object keys — see `maxKeyLength`.
    /// If exceeded, a `ParseError.StringValueTooLong` is thrown **after** parsing the full string.
    ///
    /// Defaults to 16 KB (16_384 bytes).
    maxStringValueLength: usize = 16_384,

    /// Maximum size (in bytes) for any parsed object key (i.e. field name).
    ///
    /// This is enforced after the key is fully parsed.
    /// If exceeded, a `ParseError.KeyTooLong` is thrown.
    ///
    /// Defaults to 256 bytes.
    maxKeyLength: usize = 256,

    /// Maximum size (in bytes) for the entire input buffer.
    ///
    /// Checked once before parsing begins. If the input exceeds this size,
    /// a `ParseError.InputTooLong` is thrown.
    ///
    /// Defaults to 1 MB (1_048_576 bytes).
    maxInputSize: usize = 1_048_576,

    /// Whether to validate that the input is valid UTF-8.
    ///
    /// If enabled and invalid UTF-8 is detected,
    /// a `ParseError.InvalidUtf8` is thrown.
    ///
    /// Defaults to `true`.
    shouldEnforceUtf8: bool = true,
};

/// Parses input into a `Value` tree.
pub fn parse(input: []const u8, options: ParseOptions) error{OutOfMemory}!JsonParseResult {
    var ctx = ParseContext.init(input, options);
    if (input.len > options.maxInputSize) {
        return ctx.throwErr(ParseError.InputTooLong, null);
    }
    if (options.shouldEnforceUtf8 and !std.unicode.utf8ValidateSlice(input)) {
        return ctx.throwErr(ParseError.InvalidUtf8, null);
    }
    const jsonData = parseValue(&ctx, 0) catch |e| {
        switch (e) {
            error.OutOfMemory => |outOfMemoryErr| return outOfMemoryErr,
            else => {
                ctx.arenaAlloc.deinit();
                return JsonParseResult{ .failure = .{
                    .errorInfo = ctx.err.?,
                    .allocator = ctx.errorMsgAllocator,
                } };
            },
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

    fn hasReachedEnd(self: *Cursor) bool {
        return self.pos >= self.input.len;
    }

    fn peek(self: *Cursor) ?u8 {
        if (self.hasReachedEnd()) return null;
        return self.input[self.pos];
    }

    fn peekOffset(self: *Cursor, n: usize) ?u8 {
        if (self.pos + n >= self.input.len) return null;
        return self.input[self.pos + n];
    }

    fn next(self: *Cursor) ?u8 {
        const byte = self.peek();
        self.pos += 1;
        return byte;
    }

    fn skip(self: *Cursor, n: usize) void {
        self.pos = math.min(usize, self.pos + n, self.input.len);
    }
};

const ParseContext = struct {
    arenaAlloc: std.heap.ArenaAllocator,
    errorMsgAllocator: std.mem.Allocator,
    gpa: std.heap.DebugAllocator(.{}),
    cursor: Cursor,
    input: []const u8,
    options: *const ParseOptions,
    err: ?jerror.ParsingErrorInfo = null,

    pub fn init(input: []const u8, options: ParseOptions) ParseContext {
        var gpa = std.heap.GeneralPurposeAllocator(.{}).init;
        return ParseContext{
            .gpa = gpa,
            .cursor = Cursor{ .input = input },
            .arenaAlloc = std.heap.ArenaAllocator.init(gpa.allocator()),
            .errorMsgAllocator = gpa.allocator(),
            .input = input,
            .options = options,
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

    fn consumeChar(self: *ParseContext, char: Character) ParseOrAllocError!void {
        const byte = try self.nextOrThrow();
        if (byte != char.toByte()) {
            return self.throwErr(ParseError.UnexpectedToken, try jerror.formatExpectMessage(self.errorMsgAllocator, &[_]Character{char}, byte));
        }
    }

    fn consumeEscapeSequence(self: *ParseContext) ParseOrAllocError!void {
        try self.consumeChar(Character.backwardSlash);
        const byte = try self.peekOrThrow();
        const char = Character.fromByte(byte);
        const expectedChars = &[_]Character{ .doubleQuotes, .forwardSlash, .backwardSlash, .b, .f, .n, .r, .t, .u };
        switch (char) {
            .doubleQuotes, .forwardSlash, .backwardSlash, .b, .f, .n, .r, .t => self.cursor.skip(1),
            .u => {
                self.cursor.skip(1);
                for (0..4) |offset| {
                    const hexDigit = self.cursor.peekOffset(offset) orelse return self.throwErr(ParseError.UnexpectedEOF, null);
                    if (!Character.fromByte(hexDigit).isHex()) {
                        const message = try std.fmt.allocPrint(self.errorMsgAllocator, "expected an hex value, but got '{c}'", .{byte});
                        return self.throwErr(ParseError.InvalidUnicodeSequence, message);
                    }
                }
                self.cursor.skip(4);
            },
            else => return self.throwErr(ParseError.InvalidUnicodeSequence, try jerror.formatExpectMessage(self.errorMsgAllocator, expectedChars, byte)),
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

    fn incrementAndGuardDepth(self: *ParseContext, depth: u16) ParseError!u16 {
        const incrementedDepth = depth + 1;
        if (depth > self.options.maxDepth) self.throwErr(ParseError.MaxDepthReached, null);
        return incrementedDepth;
    }
};

fn parseObject(ctx: *ParseContext, depth: u16) ParseOrAllocError!value.Object {
    const incrementedDepth = try ctx.incrementAndGuardDepth(depth);
    try ctx.consumeChar(Character.leftBrace);
    var object: value.Object = std.StringHashMap(value.Value).init(ctx.arenaAlloc.allocator());
    ctx.consumeWhitespace();
    if (try ctx.peekOrThrow() == Character.rightBrace.toByte()) {
        return object;
    }
    while (true) {
        const key = try parseString(ctx, StringType.value);
        ctx.consumeWhitespace();
        try ctx.consumeChar(Character.colon);
        ctx.consumeWhitespace();
        const val = try parseValue(ctx, incrementedDepth);
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
    const incrementedDepth = try ctx.incrementAndGuardDepth(depth);
    try ctx.consumeChar(Character.leftBracket);
    const list = std.ArrayListAligned(value.Value, null).init(ctx.arenaAlloc.allocator());
    ctx.consumeWhitespace();
    if (try ctx.peekOrThrow() == Character.rightBrace.toByte()) {
        return list.toOwnedSlice();
    }

    while (true) {
        ctx.consumeWhitespace();
        const val = try parseValue(ctx, incrementedDepth);
        try list.append(val);
        ctx.consumeWhitespace();
        if (try ctx.peekOrThrow() == Character.comma.toByte()) {
            try ctx.consumeChar(Character.comma);
            ctx.consumeWhitespace();
            continue;
        } else {
            ctx.consumeWhitespace();
            try ctx.consumeChar(Character.rightBracket);
            break;
        }
    }

    return list.toOwnedSlice();
}

const StringType = enum {
    key,
    value,
};

fn parseString(ctx: *ParseContext, stringType: StringType) ParseOrAllocError!value.String {
    try ctx.consumeChar(Character.doubleQuotes);
    const startIndex = ctx.cursor.pos;
    while (true) {
        const byte = try ctx.peekOrThrow();
        if (byte == Character.doubleQuotes.toByte()) {
            try ctx.consumeChar(Character.doubleQuotes);
            break;
        } else if (byte == Character.backwardSlash.toByte()) {
            try ctx.consumeEscapeSequence();
        } else {
            ctx.cursor.skip(1);
        }
    }
    const endIndex = ctx.cursor.pos - 1;
    const length = endIndex - startIndex;
    switch(stringType) {
        .key => if (ctx.options.maxKeyLength > length) return ctx.throwErr(ParseError.KeyTooLong, null),
        .value => if (ctx.options.maxStringValueLength > length) return ctx.throwErr(ParseError.StringValueTooLong, null),
    }
    return ctx.cursor.input[startIndex..endIndex];
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
    const byte = try ctx.peekOrThrow();
    const char = Character.fromByte(byte);

    if (char.isDigitOrMinus()) {
        return parseNum(ctx);
    }

    return switch (char) {
        .leftBracket => value.Value{ .array = try parseArray(ctx, depth) },
        .leftBrace => value.Value{ .object = try parseObject(ctx, depth) },
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
        .doubleQuotes => value.Value{ .string = try parseString(ctx, StringType.value) },
        else => return ctx.throwErr(ParseError.UnexpectedToken, try jerror.formatExpectMessage(ctx.errorMsgAllocator, expectedChars, byte)),
    };
}
