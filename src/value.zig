const std = @import("std");

pub const Value = union(enum) {
    string: String,
    float: f64,
    int: i64,
    object: Object,
    array: []const Value,
    bool: bool,
    null: void,
};

pub const String = []const u8;

pub const Object = std.StringHashMap(Value);

pub const Array = []const Value;
