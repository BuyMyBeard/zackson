const std = @import("std");
const testing = std.testing;
const value = @import("value.zig");
const Value = value.Value;
const Character = @import("character.zig").Character;
const parser = @import("parser.zig");
const jerror = @import("error.zig");
const stringify = @import("stringify.zig").stringify;

// test "Json Object" {
//     var json_data: value.Object = std.StringArrayHashMap(value.Value).init(std.testing.allocator, &value.empty_keys_slice, &value.empty_values_slice);
//     defer json_data.deinit();
//     try json_data.put("key1", value.Value{ .string = "value" });
//     try json_data.put("key2", value.Value{ .float = 69.7 });
//     try json_data.put("key3", value.Value{ .array = &[_]value.Value{
//         .{ .int = 25 },
//         .{ .string = "value" },
//     } });
//     try json_data.put("key4", value.Value{ .bool = true });

//     const key1_val = json_data.get("key1") orelse return error.MissingKey;

//     const key1_string_value = switch (key1_val) {
//         .string => |s| s,
//         else => return error.UnexpectedType,
//     };

//     std.debug.print("\n{s}\n", .{key1_string_value});
// }

// pub fn expectEqualJsonValue(expected: Value, actual: Value) !void {
//     try testing.expectEqual(@tagName(expected), @tagName(actual));

//     switch (expected) {
//         .string => |s| try testing.expectEqualStrings(s, actual.string),
//         .float => |f| try testing.expectEqual(f, actual.float),
//         .int => |i| try testing.expectEqual(i, actual.int),
//         .bool => |b| try testing.expectEqual(b, actual.bool),
//         .null => {},
//         .array => |exp_array| {
//             const act_array = actual.array;
//             try testing.expectEqual(exp_array.len, act_array.len);
//             for (exp_array, 0..) |v, i| {
//                 try expectEqualJsonValue(v, act_array[i]);
//             }
//         },
//         .object => |exp_obj| {
//             const act_obj = actual.object;
//             try testing.expectEqual(exp_obj.count(), act_obj.count());

//             var it = exp_obj.iterator();
//             while (it.next()) |entry| {
//                 const key = entry.key_ptr.*;
//                 const exp_val = entry.value_ptr.*;
//                 const act_val = act_obj.get(key) orelse return error.TestExpectedEqual;
//                 try expectEqualJsonValue(exp_val, act_val);
//             }
//         },
//     }
// }

test "Parsing Simple" {
    const json_string: []const u8 = "[123, 0.5223, \"Big \\nPP\", \"\\uD83D\\uDE80\"]";

    var result = parser.parse(json_string, std.testing.allocator, .{}) catch |err| {
        std.debug.panic("{!}\n", .{err});
    };
    defer result.deinit();

    var map = try value.UnorderedStringValueHashMap.init(std.testing.allocator, &value.empty_keys_slice, &value.empty_values_slice);
    defer map.deinit(std.testing.allocator);

    try map.put(std.testing.allocator, "key1", Value{ .string = "value🙂" });
    try map.put(std.testing.allocator, "key2", Value{ .bool = true });

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