const std = @import("std");
const testing = std.testing;
const value = @import("value.zig");
const Value = value.Value;
const Character = @import("character.zig").Character;
const parser = @import("parser.zig");
const jerror = @import("error.zig");
const stringify = @import("stringify.zig").stringify;

test "Json Object" {
    var json_data: value.Object = std.StringArrayHashMap(value.Value).init(std.testing.allocator, &value.empty_keys_slice, &value.empty_values_slice);
    defer json_data.deinit();
    try json_data.put("key1", value.Value{ .string = "value" });
    try json_data.put("key2", value.Value{ .float = 69.7 });
    try json_data.put("key3", value.Value{ .array = &[_]value.Value{
        .{ .int = 25 },
        .{ .string = "value" },
    } });
    try json_data.put("key4", value.Value{ .bool = true });

    const key1_val = json_data.get("key1") orelse return error.MissingKey;

    const key1_string_value = switch (key1_val) {
        .string => |s| s,
        else => return error.UnexpectedType,
    };

    std.debug.print("\n{s}\n", .{key1_string_value});
}

pub fn expectEqualJsonValue(expected: Value, actual: Value) !void {
    try testing.expectEqual(@tagName(expected), @tagName(actual));

    switch (expected) {
        .string => |s| try testing.expectEqualStrings(s, actual.string),
        .float => |f| try testing.expectEqual(f, actual.float),
        .int => |i| try testing.expectEqual(i, actual.int),
        .bool => |b| try testing.expectEqual(b, actual.bool),
        .null => {},
        .array => |exp_array| {
            const act_array = actual.array;
            try testing.expectEqual(exp_array.len, act_array.len);
            for (exp_array, 0..) |v, i| {
                try expectEqualJsonValue(v, act_array[i]);
            }
        },
        .object => |exp_obj| {
            const act_obj = actual.object;
            try testing.expectEqual(exp_obj.count(), act_obj.count());

            var it = exp_obj.iterator();
            while (it.next()) |entry| {
                const key = entry.key_ptr.*;
                const exp_val = entry.value_ptr.*;
                const act_val = act_obj.get(key) orelse return error.TestExpectedEqual;
                try expectEqualJsonValue(exp_val, act_val);
            }
        },
    }
}

test "Parsing Simple" {
    const json_string: []const u8 = "[123, 0.5223, 10e10]";

    const result = parser.parse(json_string, .{}) catch |err| {
        std.debug.panic("{!}\n", .{err});
    };

    var map = try value.UnorderedStringValueHashMap.init(std.testing.allocator, &value.empty_keys_slice, &value.empty_values_slice);
    defer map.deinit(std.testing.allocator);

    try map.put(std.testing.allocator, "key1", Value{ .string = "value🙂" });
    try map.put(std.testing.allocator,"key2", Value{ .bool = true });

    // const expected = Value{.object = map};

    switch (result) {
        .success => |success_result| {
            // try expectEqualJsonValue(expected, successResult.value);
            const string = try stringify(testing.allocator, success_result.value, .{ .format = .pretty });
            defer testing.allocator.free(string);
            std.debug.print("{s}", .{string});
        },
        .failure => |err| {
            try err.error_info.printError();
            return err.error_info.error_type;
        },
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
        .arena_alloc = arena,
        .error_msg_allocator = arena.allocator(),
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
        .arena_alloc = arena,
        .error_msg_allocator = arena.allocator(),
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
