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
pub const ParseResult = union(enum) {
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
    /// Error details such as error type, line/column, and message are stored in `error_info`.
    /// The allocator used to create the error message (if any) is returned for cleanup.
    failure: struct {
        error_info: jerror.ParsingErrorInfo,
        allocator: std.mem.Allocator,
    },

    /// Deinitializes the result:
    /// - If `.success`, deinitializes the arena and frees associated memory.
    /// - If `.failure`, frees the memory allocated for the error message.
    pub fn deinit(self: *ParseResult) void {
        switch (self.*) {
            .success => |s| s.arena.deinit(),
            .failure => |f| {
                if (f.error_info.message) |msg| f.allocator.free(msg);
            },
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
    max_depth: u16 = 64,

    /// Maximum size (in bytes) for any parsed string value (e.g. string literals, numbers as strings, etc.).
    ///
    /// This limit does **not** apply to object keys — see `max_key_length`.
    /// If exceeded, a `ParseError.StringValueTooLong` is thrown **after** parsing the full string.
    ///
    /// Defaults to 16 KB (16_384 bytes).
    max_string_value_length: usize = 16_384,

    /// Maximum size (in bytes) for any parsed object key (i.e. field name).
    ///
    /// This is enforced after the key is fully parsed.
    /// If exceeded, a `ParseError.KeyTooLong` is thrown.
    ///
    /// Defaults to 256 bytes.
    max_key_length: usize = 256,

    /// Maximum size (in bytes) for the entire input buffer.
    ///
    /// Checked once before parsing begins. If the input exceeds this size,
    /// a `ParseError.InputTooLong` is thrown.
    ///
    /// Defaults to 1 MB (1_048_576 bytes).
    max_input_size: usize = 1_048_576,

    /// Whether to validate that the input is valid UTF-8.
    ///
    /// If enabled and invalid UTF-8 is detected,
    /// a `ParseError.InvalidUtf8` is thrown.
    ///
    /// Defaults to `true`.
    should_enforce_utf8: bool = true,
};

/// Parses a UTF-8 JSON input buffer into a structured `Value` tree.
///
/// This function performs a complete parse of the input string, applying
/// the constraints and safety checks defined in `ParseOptions`.
///
/// - On success, returns a `ParseResult.success` variant containing the parsed value
///   and the arena allocator used for all internal allocations.
/// - On structural or semantic failure, returns a `ParseResult.failure` with detailed
///   error information (e.g. unexpected token, depth overflow, invalid value).
/// - On allocation failure, throws `error.OutOfMemory`.
///
/// Notes:
/// - The input is expected to be valid UTF-8 unless `should_enforce_utf8` is set to false.
/// - Key and value lengths, input size, and depth are all enforced by `ParseOptions`.
///
/// The caller is responsible for calling `.deinit()` on the returned `ParseResult`
/// to release allocated memory.
///
/// Example usage:
/// ```zig
/// const result = try parse(input, .{});
/// defer result.deinit();
/// ```
///
/// See also:
/// - `ParseOptions`
/// - `ParseResult`
/// - `Value`
pub fn parse(input: []const u8, allocator: std.mem.Allocator, options: ParseOptions) error{OutOfMemory}!ParseResult {
    var ctx = ParseContext.init(input, allocator, options);
    if (input.len > options.max_input_size) {
        ctx.throwErr(ParseError.InputTooLong, null) catch {};
        return ctx.cleanUpAndGenerateFailureParseResult();
    }
    if (options.should_enforce_utf8 and !std.unicode.utf8ValidateSlice(input)) {
        ctx.throwErr(ParseError.InvalidUtf8, null) catch {};
        return ctx.cleanUpAndGenerateFailureParseResult();
    }
    const json_data = parseValue(&ctx, 0) catch |e| {
        switch (e) {
            error.OutOfMemory => {
                ctx.deinit();
                return error.OutOfMemory;
            },
            else => {
                return ctx.cleanUpAndGenerateFailureParseResult();
            },
        }
    };

    return ParseResult{ .success = .{
        .arena = ctx.arena_alloc,
        .value = json_data,
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
    arena_alloc: std.heap.ArenaAllocator,
    error_msg_allocator: std.mem.Allocator,
    cursor: Cursor,
    input: []const u8,
    options: ParseOptions,
    err: ?jerror.ParsingErrorInfo = null,

    pub fn init(input: []const u8, allocator: std.mem.Allocator, options: ParseOptions) ParseContext {
        return ParseContext{
            .cursor = Cursor{ .input = input },
            .arena_alloc = std.heap.ArenaAllocator.init(allocator),
            .error_msg_allocator = allocator,
            .input = input,
            .options = options,
        };
    }

    pub fn deinit(self: *ParseContext) void {
        self.arena_alloc.deinit();
    }

    fn throwErr(self: *ParseContext, err: ParseError, message: ?[]const u8) ParseError!noreturn {
        self.err = .{ 
            .error_type = err,
            .message = message,
            .input = self.input,
            .pos = self.cursor.pos,
        };
        return err;
    }

    fn nextOrThrow(self: *ParseContext) ParseError!u8 {
        return self.cursor.next() orelse try self.throwErr(ParseError.UnexpectedEOF, null);
    }

    fn peekOrThrow(self: *ParseContext) ParseError!u8 {
        return self.cursor.peek() orelse try self.throwErr(ParseError.UnexpectedEOF, null);
    }

    fn consumeSequence(self: *ParseContext, seq: []const u8) ParseError!void {
        for (seq) |expected_byte| {
            const byte = try self.nextOrThrow();
            if (byte != expected_byte) {
                return ParseError.InvalidValue;
            }
        }
    }

    fn consumeChar(self: *ParseContext, char: Character) ParseOrAllocError!void {
        const byte = try self.peekOrThrow();
        if (byte != char.toByte()) {
            try self.throwErr(ParseError.UnexpectedToken, try jerror.formatExpectMessage(self.error_msg_allocator, &[_]Character{char}, byte));
        }
        self.cursor.skip(1);
    }

    fn maybeConsumeChar(self: *ParseContext, char: Character) bool {
        const byte = self.cursor.peek() orelse return false;
        if (byte == char.toByte()) {
            self.cursor.skip(1);
            return true;
        }
        return false;
    }

    fn parseHex16(self: *ParseContext) !u16 {
        for (0..4) |i| {
            const digit = self.cursor.peekOffset(i) orelse return try self.throwErr(ParseError.UnexpectedEOF, null);
            if (!Character.fromByte(digit).isHex()) {
                return try self.throwErr(ParseError.InvalidUnicodeSequence, null);
            }
        }
        const start = self.cursor.pos;
        self.cursor.skip(4);
        return std.fmt.parseInt(u16, self.cursor.input[start .. start + 4], 16) catch unreachable;
    }

    fn decodeEscapedUnicode(self: *ParseContext, buf: []u8) ParseOrAllocError![]const u8 {
        var codepoint: u21 = try self.parseHex16();

        if (codepoint >= 0xD800 and codepoint <= 0xDBFF) {
            try self.consumeChar(Character.backwardSlash);
            try self.consumeChar(Character.u);
            const low = try self.parseHex16();
            if (!(low >= 0xDC00 and low <= 0xDFFF)) {
                return try self.throwErr(ParseError.InvalidUnicodeSequence, "expected low surrogate after high surrogate");
            }
            codepoint = 0x10000 + ((@as(u21, codepoint - 0xD800) << 10) | (low - 0xDC00));
        } else if (codepoint >= 0xDC00 and codepoint <= 0xDFFF) {
            return try self.throwErr(ParseError.InvalidUnicodeSequence, "unexpected low surrogate without high surrogate");
        }

        const len = std.unicode.utf8Encode(codepoint, buf) catch return try self.throwErr(ParseError.InvalidUtf8, null);
        return buf[0..len];
    }
    /// Caller must ensure `buf.len >= 4`. Behavior is undefined otherwise.
    fn consumeEscapeSequence(self: *ParseContext, buf: []u8) ParseOrAllocError![]const u8 {
        std.debug.assert(buf.len >= 4);
        try self.consumeChar(Character.backwardSlash);
        const byte = try self.peekOrThrow();
        const char = Character.fromByte(byte);
        const expected_chars = &[_]Character{ .doubleQuotes, .forwardSlash, .backwardSlash, .b, .f, .n, .r, .t, .u };
        switch (char) {
            .doubleQuotes,
            .forwardSlash,
            .backwardSlash => {
                self.cursor.skip(1);
                return char.toSlice(buf);
            },
            .b => {
                self.cursor.skip(1);
                return Character.backspace.toSlice(buf);
            },
            .f => {
                self.cursor.skip(1);
                return Character.formFeed.toSlice(buf);
            },
            .n => {
                self.cursor.skip(1);
                return Character.newline.toSlice(buf);
            },
            .r => {
                self.cursor.skip(1);
                return Character.cariageReturn.toSlice(buf);
            }, 
            .t => {
                self.cursor.skip(1);
                return Character.tab.toSlice(buf);
            },
            .u => {
                self.cursor.skip(1);
                return try self.decodeEscapedUnicode(buf);
            },
            else => try self.throwErr(ParseError.InvalidUnicodeSequence, try jerror.formatExpectMessage(self.error_msg_allocator, expected_chars, byte)),
        }
    }

    fn consumeDigit(self: *ParseContext) ParseOrAllocError!void {
        const byte = try self.peekOrThrow();
        const char = Character.fromByte(byte);

        if (!char.isDigit()) {
            const expected = &[_]Character{
                .zero, .one, .two,   .three, .four,
                .five, .six, .seven, .eight, .nine,
            };
            try self.throwErr(ParseError.UnexpectedToken, try jerror.formatExpectMessage(self.error_msg_allocator, expected, byte));
        }
        self.cursor.skip(1);
    }

    fn maybeConsumeDigit(self: *ParseContext) bool {
        const byte = self.cursor.peek() orelse return false;
        const char = Character.fromByte(byte);

        if (!char.isDigit()) {
            return false;
        }
        self.cursor.skip(1);
        return true;
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
        const incremented_depth = depth + 1;
        if (depth > self.options.max_depth) try self.throwErr(ParseError.MaxDepthReached, null);
        return incremented_depth;
    }

    fn cleanUpAndGenerateFailureParseResult(self: *ParseContext) ParseResult {
        self.arena_alloc.deinit();
        return ParseResult{ .failure = .{
            .error_info = self.err.?,
            .allocator = self.error_msg_allocator,
        } };
    }
};

fn parseObject(ctx: *ParseContext, depth: u16) ParseOrAllocError!value.Object {
    const incremented_depth = try ctx.incrementAndGuardDepth(depth);
    try ctx.consumeChar(Character.leftBrace);
    var map = try std.StringArrayHashMapUnmanaged(value.Value).init(ctx.arena_alloc.allocator(), &value.empty_keys_slice, &value.empty_values_slice);
    ctx.consumeWhitespace();
    if (try ctx.peekOrThrow() == Character.rightBrace.toByte()) {
        return map;
    }
    while (true) {
        const key = try parseString(ctx, StringType.value);
        ctx.consumeWhitespace();
        try ctx.consumeChar(Character.colon);
        ctx.consumeWhitespace();
        const val = try parseValue(ctx, incremented_depth);
        try map.put(ctx.arena_alloc.allocator(), key, val);
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

    return map;
}

fn parseArray(ctx: *ParseContext, depth: u16) ParseOrAllocError!value.Array {
    const incremented_depth = try ctx.incrementAndGuardDepth(depth);
    try ctx.consumeChar(Character.leftBracket);
    ctx.consumeWhitespace();
    var list = std.ArrayListAligned(value.Value, null).init(ctx.arena_alloc.allocator());
    if (try ctx.peekOrThrow() == Character.rightBrace.toByte()) {
        return list.toOwnedSlice();
    }

    while (true) {
        const val = try parseValue(ctx, incremented_depth);
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

fn parseString(ctx: *ParseContext, string_type: StringType) ParseOrAllocError!value.String {
    try ctx.consumeChar(Character.doubleQuotes);
    var list = std.ArrayList(u8).init(ctx.arena_alloc.allocator());
    const start_index = ctx.cursor.pos;
    while (true) {
        const char = Character.fromByte(try ctx.peekOrThrow());
        if (char.isControlCharacter()) {
            try ctx.throwErr(ParseError.UnescapedControlCharacter, null);
        } else if (char == Character.doubleQuotes) {
            try ctx.consumeChar(Character.doubleQuotes);
            break;
        } else if (char == Character.backwardSlash) {
            var buf:[4]u8 = undefined;
            const utf8_slice = try ctx.consumeEscapeSequence(&buf);
            try list.appendSlice(utf8_slice);
        } else {
            ctx.cursor.skip(1);
            try list.append(char.toByte());
        }
    }
    const end_index = ctx.cursor.pos - 1;
    const length = end_index - start_index;
    switch (string_type) {
        .key => if (length > ctx.options.max_key_length) try ctx.throwErr(ParseError.KeyTooLong, null),
        .value => if (length > ctx.options.max_string_value_length) try ctx.throwErr(ParseError.StringValueTooLong, null),
    }
    return list.toOwnedSlice();
}

fn validateHexDigit(ctx: *ParseContext, byte: u8) ParseError!void {
    return switch (byte) {
        Character.zero.toByte()...Character.nine.to, Character.a...Character.f, Character.A...Character.F => {},
        else => {
            const message = std.fmt.allocPrint(ctx.error_msg_allocator, "expected an hex value, but got '{c}'", .{byte});
            try ctx.throwErr(ParseError.InvalidUnicodeSequence, message);
        },
    };
}

fn parseNum(ctx: *ParseContext) ParseOrAllocError!value.Value {
    const start_index = ctx.cursor.pos;
    _ = ctx.maybeConsumeChar(Character.minus);
    const first_digit_byte = try ctx.peekOrThrow();
    const first_digit = Character.fromByte(first_digit_byte);
    try ctx.consumeDigit();

    if (first_digit != Character.zero) {
        while (ctx.maybeConsumeDigit()) {}
    }
    if (ctx.maybeConsumeChar(Character.dot)) {
        while (ctx.maybeConsumeDigit()) {}
    }
    if (ctx.maybeConsumeChar(Character.e) or ctx.maybeConsumeChar(Character.E)) {
        if (!ctx.maybeConsumeChar(Character.minus)) {
            _ = ctx.maybeConsumeChar(Character.plus);
        }
        while (ctx.maybeConsumeDigit()) {}
    }
    const end_index = ctx.cursor.pos;
    const sequence = ctx.input[start_index..end_index];

    const int_value = std.fmt.parseInt(i64, sequence, 10) catch {
        const float_value = std.fmt.parseFloat(f64, sequence) catch {
            try ctx.throwErr(ParseError.InvalidValue, null);
        };
        return value.Value{ .float = float_value };
    };

    return value.Value{ .int = int_value };
}

fn parseValue(ctx: *ParseContext, depth: u16) ParseOrAllocError!value.Value {
    ctx.consumeWhitespace();
    const expected_chars = &[_]Character{ Character.leftBracket, Character.leftBrace };
    const byte = try ctx.peekOrThrow();
    const char = Character.fromByte(byte);

    if (char.isDigitOrMinus()) {
        return parseNum(ctx);
    }

    return switch (char) {
        .leftBracket => value.Value{ .array = try parseArray(ctx, depth) },
        .leftBrace => value.Value{ .object = try parseObject(ctx, depth) },
        .t => {
            ctx.consumeSequence("true") catch |err| try ctx.throwErr(err, null);
            return value.Value{ .bool = true };
        },
        .f => {
            ctx.consumeSequence("false") catch |err| try ctx.throwErr(err, null);
            return value.Value{ .bool = false };
        },
        .n => {
            ctx.consumeSequence("null") catch |err| try ctx.throwErr(err, null);
            return value.Value{ .null = {} };
        },
        .doubleQuotes => value.Value{ .string = try parseString(ctx, StringType.value) },
        else => try ctx.throwErr(ParseError.UnexpectedToken, try jerror.formatExpectMessage(ctx.error_msg_allocator, expected_chars, byte)),
    };
}
