const std = @import("std");

/// Represents any valid JSON value.
///
/// This tagged union covers all data types supported by the JSON specification:
/// - `string`: UTF-8 encoded string
/// - `float`: Floating-point number (64-bit IEEE 754)
/// - `int`: Integer (64-bit signed)
/// - `object`: JSON object (key-value map)
/// - `array`: JSON array (ordered list of values)
/// - `bool`: Boolean value (`true` or `false`)
/// - `null`: JSON null
pub const Value = union(enum) {
    /// A UTF-8 encoded string value.
    string: String,

    /// A floating-point number (`f64`).
    ///
    /// Used when the value contains a decimal or exponent (e.g. `1.23`, `2e10`).
    float: f64,

    /// A whole number (`i64`).
    ///
    /// Used when the number can be parsed as a signed integer without loss.
    int: i64,

    /// A JSON object represented as a string-keyed map.
    ///
    /// ⚠️ Key order is **not preserved**.
    object: Object,

    /// A JSON array, represented as a list of `Value` elements.
    array: Array,

    /// A boolean value (`true` or `false`).
    bool: bool,

    /// A JSON null value.
    null: void,
};

/// Represents a UTF-8 encoded JSON string.
///
/// This type is used for both:
/// - JSON **string values** (e.g. `"hello world"`)
/// - JSON **object keys** (e.g. `"name": "Ziggy"`)
///
/// Note: Key strings are typically shorter and may be subject to different size limits
/// via `ParseOptions.max_key_length`.
pub const String = []const u8;

/// Represents a JSON object (key-value map).
///
/// Implemented using Zig's `std.StringHashMap`, where:
/// - Keys are `[]const u8`
/// - Values are recursively typed as `Value`
///
/// ⚠️ Note: The order of keys is **not preserved** during parsing.
/// This is due to the use of a hash map for performance and lookup efficiency.
pub const Object = std.StringHashMap(Value);



/// Represents a JSON array (ordered list of values).
pub const Array = []const Value;
