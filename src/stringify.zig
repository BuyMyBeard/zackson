const std = @import("std");
const jsonvalue = @import("value.zig");
const Value = jsonvalue.Value;
const Object = jsonvalue.Object;
const Array = jsonvalue.Array;
const jerror = @import("error.zig");
const AllocError = jerror.AllocError;
const StringifyError = jerror.StringifyError;
const StringifyOrAllocError = jerror.StringifyOrAllocError;
const Character = @import("character.zig").Character;

/// Controls the formatting style of the JSON output.
pub const FormatStyle = enum {
    /// Pretty-printed output with indentation and newlines.
    ///
    /// Use this when generating human-readable JSON (e.g., for logs, config files, or debugging).
    pretty,

    /// Compact output with no unnecessary whitespace.
    ///
    /// Use this when minimizing size is important (e.g., for APIs, storage, or transmission).
    compact,
};

/// Options for customizing the behavior of JSON stringification.
pub const StringifyOptions = struct {
    /// Output formatting style (`pretty` or `compact`).
    ///
    /// Defaults to `.compact`.
    format: FormatStyle = .compact,

    /// Number of spaces used per indentation level when `format` is `.pretty`.
    ///
    /// This must be a value between `0` and `15` (inclusive).
    /// Defaults to `2`.
    indentation: u4 = 2,

    /// Returns `true` if the formatter is set to pretty-print mode.
    fn isPretty(self: StringifyOptions) bool {
        return self.format == FormatStyle.pretty;
    }
};

/// Converts a parsed JSON `Value` back into a valid JSON string.
///
/// - `alloc`: The allocator used to allocate the resulting string.
/// - `value`: The JSON value to stringify.
/// - `options`: Stringify behavior customization (pretty/compact, indentation).
///
/// Returns: An allocated UTF-8 encoded JSON string. The caller is responsible for freeing it.
///
/// May fail with `AllocError.OutOfMemory` if memory allocation fails.
pub fn stringify(alloc: std.mem.Allocator, value: Value, options: StringifyOptions) StringifyOrAllocError![]const u8 {
    var list = std.ArrayListAligned(u8, null).init(alloc);
    try appendDecodedValue(value, &list, 0, &options);
    if (options.isPretty()) {
        try appendChar(Character.newline, &list);
    }
    return try list.toOwnedSlice();
}

fn appendDecodedValue(value: Value, list: *std.ArrayListAligned(u8, null), indent: u16, options: *const StringifyOptions) StringifyOrAllocError!void {
    switch (value) {
        .object => |object| try appendObject(object, list, indent, options),
        .array => |array| try appendArray(array, list, indent, options),
        .string => |string| try appendString(string, list),
        .float => |float| try appendFloat(float, list),
        .int => |int| try appendInt(int, list),
        .bool => |boolean| try appendBool(boolean, list),
        .null => try list.appendSlice("null"),
    }
}

fn appendBool(boolean: bool, list: *std.ArrayListAligned(u8, null)) AllocError!void {
    if (boolean) {
        try list.appendSlice("true");
    } else {
        try list.appendSlice("false");
    }
}

fn appendString(string: []const u8, list: *std.ArrayListAligned(u8, null)) StringifyOrAllocError!void {
    try appendChar(Character.doubleQuotes, list);
    try escapeAndAppend(string, list);
    try appendChar(Character.doubleQuotes, list);
}

fn escapeAndAppend(string: []const u8, list: *std.ArrayListAligned(u8, null)) StringifyOrAllocError!void {
    const utf8_view = try std.unicode.Utf8View.init(string);
    var iter = utf8_view.iterator();
    var buf: [12]u8 = undefined;
    while(iter.nextCodepoint()) |cp| {
        const charOrNull = Character.fromCodepoint(cp);
        if (charOrNull) |char| {
            try list.appendSlice(char.toEscapedSlice(&buf));
        } else if (cp <= 0xFFFF) {
            try list.appendSlice(std.fmt.bufPrint(&buf, "\\u{X:0>4}", .{cp}) catch unreachable);
        } else {
            try list.appendSlice(writeSurrogatePair(cp, &buf));
        }
    }
}

/// Caller must ensure `buf.len >= 12`. Behavior is undefined otherwise.
fn writeSurrogatePair(cp: u21, buf: []u8) []const u8 {
    std.debug.assert(buf.len >= 12);
    const high = 0xD800 + ((cp - 0x10000) >> 10);
    const low  = 0xDC00 + (cp & 0x3FF);
    return std.fmt.bufPrint(buf, "\\u{X:0>4}\\u{X:0>4}", .{high, low}) catch unreachable;
}

fn appendObject(object: Object, list: *std.ArrayListAligned(u8, null), indent: u16, options: *const StringifyOptions) StringifyOrAllocError!void {
    const incremented_indent = indent + options.indentation;
    var iterator = object.iterator();
    try appendChar(Character.leftBrace, list);
    var index: usize = 0;
    while (iterator.next()) |entry| {
        if (index != 0) {
            try appendChar(Character.comma, list);
        }
        if (options.isPretty()) {
            try appendChar(Character.newline, list);
            try appendIndent(incremented_indent, list);
        }
        try appendString(entry.key_ptr.*, list);
        try appendChar(Character.colon, list);
        if (options.isPretty()) {
            try appendChar(Character.space, list);
        }
        try appendDecodedValue(entry.value_ptr.*, list, incremented_indent, options);
        index += 1;
    }
    if (options.isPretty() and index != 0) {
        try appendChar(Character.newline, list);
        try appendIndent(indent, list);
    }
    try appendChar(Character.rightBrace, list);
}

fn appendArray(array: Array, list: *std.ArrayListAligned(u8, null), indent: u16, options: *const StringifyOptions) StringifyOrAllocError!void {
    const incremented_indent = indent + options.indentation;
    try appendChar(Character.leftBracket, list);
    var index: usize = 0;
    for (array) |value| {
        if (index != 0) {
            try appendChar(Character.comma, list);
        }
        if (options.isPretty()) {
            try appendChar(Character.newline, list);
            try appendIndent(incremented_indent, list);
        }
        try appendDecodedValue(value, list, incremented_indent, options);
        index += 1;
    }
    if (options.isPretty() and index != 0) {
        try appendChar(Character.newline, list);
        try appendIndent(indent, list);
    }
    try appendChar(Character.rightBracket, list);
}

fn appendInt(int: i64, list: *std.ArrayListAligned(u8, null)) AllocError!void {
    const alloc = std.heap.page_allocator;
    const formatted_int = try std.fmt.allocPrint(alloc, "{d}", .{int});
    defer alloc.free(formatted_int);
    try list.appendSlice(formatted_int);
}

fn appendFloat(float: f64, list: *std.ArrayListAligned(u8, null)) AllocError!void {
    const alloc = std.heap.page_allocator;
    const formatted_float = try std.fmt.allocPrint(alloc, "{d}", .{float});
    defer alloc.free(formatted_float);
    try list.appendSlice(formatted_float);
}

fn appendChar(char: Character, list: *std.ArrayListAligned(u8, null)) AllocError!void {
    try list.append(char.toByte());
}

fn appendIndent(indent: u16, list: *std.ArrayListAligned(u8, null)) AllocError!void {
    const alloc = std.heap.page_allocator;
    const indent_buffer = try alloc.alloc(u8, indent);
    defer alloc.free(indent_buffer);
    @memset(indent_buffer, Character.space.toByte());
    try list.appendSlice(indent_buffer);
}
