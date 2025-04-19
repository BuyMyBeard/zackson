const std = @import("std");

pub fn OrderedStringHashMap(comptime T: type) type {
    return struct {
        hash_map: std.StringHashMap(T),
        key_order: std.ArrayList(u8),

        fn push(_: T) !void {
            unreachable;
        }
    };
}