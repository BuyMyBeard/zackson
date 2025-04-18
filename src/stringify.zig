const std = @import("std");
const jsonvalue = @import("value.zig");
const Value = jsonvalue.Value;
const Object = jsonvalue.Object;
const Array = jsonvalue.Array;
const AllocError = @import("error.zig").AllocError;
const Character = @import("character.zig").Character;

pub const FormatStyle = enum{
    pretty, 
    compact,
};

pub const StringifyOptions = struct {
    format: FormatStyle = .compact,
    indentation: u3 = 2,
    
    fn isPretty(self: StringifyOptions) bool {
        return self.format == FormatStyle.pretty;
    }
};

pub fn stringify(alloc: std.mem.Allocator, value: Value, options: StringifyOptions) AllocError![]const u8 {
    var list = std.ArrayListAligned(u8, null).init(alloc);
    try appendDecodedValue(value, &list, 0, &options);
    if (options.isPretty()) {
        try appendChar(Character.newline, &list);
    }
    return try list.toOwnedSlice();
}

fn appendDecodedValue(value: Value, list: *std.ArrayListAligned(u8, null), indent: u16, options: *const StringifyOptions) AllocError!void {
    switch(value) {
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

fn appendString(string: []const u8, list: *std.ArrayListAligned(u8, null)) AllocError!void {
    try appendChar(Character.doubleQuotes, list);
    try list.appendSlice(string);
    try appendChar(Character.doubleQuotes,list);
}

fn appendObject(object: Object, list: *std.ArrayListAligned(u8, null), indent: u16, options: *const StringifyOptions) AllocError!void {
    const incrementedIndent = indent + options.indentation;
    var iterator = object.iterator();
    try appendChar(Character.leftBrace, list);
    var index: usize = 0;
    while(true) {
        const entry = iterator.next() orelse break;
        if (index != 0) {
            try appendChar(Character.comma,list);
        } 
        if (options.isPretty()) {
            try appendChar(Character.newline, list);
            try appendIndent(incrementedIndent, list);
        }
        try appendString(entry.key_ptr.*, list);
        try appendChar(Character.colon,list);
        if (options.isPretty()) {
            try appendChar(Character.space, list);
        }
        try appendDecodedValue(entry.value_ptr.*, list, incrementedIndent, options);
        index += 1;
    }
    if (options.isPretty() and index != 0) {
        try appendChar(Character.newline,list);
        try appendIndent(indent, list);
    }
    try appendChar(Character.rightBrace, list);
}

fn appendArray(array: Array, list: *std.ArrayListAligned(u8, null), indent: u16, options: *const StringifyOptions) AllocError!void {
    const incrementedIndent = indent + options.indentation;
    try appendChar(Character.leftBracket, list);
    var index: usize = 0;
    for(array) |value| {
        if (index != 0) {
            try appendChar(Character.comma,list);
        } 
        if (options.isPretty()) {
            try appendChar(Character.newline, list);
            try appendIndent(incrementedIndent, list);
        }
        try appendDecodedValue(value, list, incrementedIndent, options);
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
    const formattedInt = try std.fmt.allocPrint(alloc, "{d}", .{int});
    defer alloc.free(formattedInt);
    try list.appendSlice(formattedInt);
}

fn appendFloat(float: f64, list: *std.ArrayListAligned(u8, null)) AllocError!void {
    const alloc = std.heap.page_allocator;
    const formattedInt = try std.fmt.allocPrint(alloc, "{d}", .{float});
    defer alloc.free(formattedInt);
    try list.appendSlice(formattedInt);
}

fn appendChar(char: Character, list: *std.ArrayListAligned(u8, null)) AllocError!void {
    try list.append(char.toByte());
}

fn appendIndent(indent :u16, list: *std.ArrayListAligned(u8, null)) AllocError!void {
    const alloc = std.heap.page_allocator;
    const indentBuffer = try alloc.alloc(u8, indent);
    defer alloc.free(indentBuffer);
    @memset(indentBuffer, Character.space.toByte());
    try list.appendSlice(indentBuffer);
}
