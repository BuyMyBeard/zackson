const std = @import("std");
const testing = std.testing;
const value = @import("value.zig");
const Character = @import("character.zig").Character;
const parser = @import("parser.zig");
const jerror = @import("error.zig");

test "Json Object" {
    const alloc = std.heap.page_allocator;
    var jsonData: value.Object = std.StringHashMap(value.Value).init(alloc);
    defer jsonData.deinit();
    try jsonData.put("key1", value.Value{ .string = "value" });
    try jsonData.put("key2", value.Value{ .float = 69.7 });
    try jsonData.put("key3", value.Value{ .array = &[_]value.Value{
        .{ .int = 25 },
        .{ .string = "value" },
    } });
    try jsonData.put("key4", value.Value{ .bool = true });

    const key1_val = jsonData.get("key1") orelse return error.MissingKey;

    const key1_string_value = switch (key1_val) {
        .string => |s| s,
        else => return error.UnexpectedType,
    };

    std.debug.print("\n{s}\n", .{key1_string_value});
}

test "Parsing Simple" {
    const jsonString: []const u8 = "{\"key1\": \"value🙂\", \"key2\": true}";

    const result = parser.parse(jsonString, .{}) catch |err| {
        std.debug.panic("{!}\n", .{err});
    };

    switch (result) {
        .success => std.debug.print("ok", .{}),
        .failure => |err| try err.errorInfo.printError(),
    }
}

test "Characters" {
    const chars = &[_]Character{ .leftBracket, .leftBrace };
    try testing.expectEqual(chars[0].toByte(), 123);
    try testing.expectEqual(chars[1].toByte(), 91);
}

test "Character enum toByte is correct" {
    try testing.expectEqual(@intFromEnum(Character.leftBracket), '{');
    try testing.expectEqual(Character.leftBracket.toByte(), '{');
    try testing.expectEqual(Character.leftBrace.toByte(), '[');
}

test "joinChars with valid values" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var ctx: parser.ParseContext = .{
        .arenaAlloc = arena,
        .errorMsgAllocator = arena.allocator(),
        .gpa = undefined,
        .cursor = parser.Cursor{ .input = "" },
        .input = "",
        .err = null,
    };

    const chars = &[_]Character{ Character.a, Character.b, Character.c };
    const result = try parser.joinChars(&ctx, chars);

    try testing.expectEqualStrings("a, b, c", result);
}

test "Character values must be valid before printing" {
    const byte: u8 = 200;
    _ = std.meta.intToEnum(Character, byte) catch |err| {
        try testing.expectEqual(error.InvalidEnumTag, err);
        return;
    };

    return error.TestExpectedErrorNotThrown;
}

test "formatExpectMessage shows expected tokens" {
    var arena = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer arena.deinit();

    var ctx: parser.ParseContext = .{
        .arenaAlloc = arena,
        .errorMsgAllocator = arena.allocator(),
        .gpa = undefined,
        .cursor = parser.Cursor{ .input = "X" },
        .input = "X",
        .err = null,
    };

    const expected = &[_]Character{ Character.n, Character.r };
    const msg = try jerror.formatExpectMessage(&ctx, expected, 'X');

    std.debug.print("Formatted: {s}\n", .{msg});
    try testing.expect(msg.len > 0);
}

test "Character.isHex" {
    try testing.expect(Character.A.isHex());
    try testing.expect(Character.zero.isHex());
    try testing.expect(!Character.leftBrace.isHex());
}
