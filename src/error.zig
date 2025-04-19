const std = @import("std");
const Character = @import("character.zig").Character;
const math = @import("math.zig");

/// Formats a human-readable error message describing an unexpected character.
///
/// This function is typically used in `ParseError.UnexpectedToken` situations,
/// where the parser expected one of several characters but encountered a different one.
///
/// The resulting message is dynamically allocated using the provided `alloc`
/// and follows the format:  
/// `"expected one of 'x, y, z' but got 'c'"`
///
/// - `alloc`: The allocator used to allocate the final formatted message.
/// - `characters`: A list of expected `Character` enum values.
/// - `received`: The byte that was actually encountered (as `u8`).
///
/// Returns: A heap-allocated `[]u8` error message. Caller is responsible for freeing it.
///
/// May fail with `error.OutOfMemory`.
pub fn formatExpectMessage(alloc: std.mem.Allocator, characters: []const Character, received: u8) error{OutOfMemory}![]u8 {
    var charsAlloc = std.heap.page_allocator;
    const formattedChars = try Character.join(charsAlloc, characters, ", ");
    defer charsAlloc.free(formattedChars);
    return try std.fmt.allocPrint(alloc, "expected one of '{s}' but got '{c}'", .{ formattedChars, received });
}

/// Represents structural and semantic parsing errors.
///
/// These errors are **never thrown**. Instead, they are wrapped inside a
/// `ParsingErrorInfo` and returned via `JsonParseResult.failure`.
pub const ParseError = error{
    /// Thrown when a token doesn't match the expected grammar.
    UnexpectedToken,

    /// Thrown when a Unicode escape sequence is malformed or invalid.
    InvalidUnicodeSequence,

    /// Thrown when the input ends before the parser expects it.
    UnexpectedEOF,

    /// Thrown when a number or literal is not recognized as valid.
    InvalidValue,

    /// Thrown when the maximum allowed object/array nesting depth is exceeded.
    MaxDepthReached,

    /// Thrown when an object key exceeds `ParseOptions.maxKeyLength`.
    KeyTooLong,

    /// Thrown when a string value exceeds `ParseOptions.maxStringValueLength`.
    StringValueTooLong,

    /// Thrown when the input slice exceeds `ParseOptions.maxInputSize`.
    InputTooLong,

    /// Thrown when UTF-8 validation is enabled and the input is invalid.
    InvalidUtf8,
};

/// Represents unrecoverable allocation failures during parsing.
///
/// These errors are **bubbled up directly** via Zig’s error system,
/// and are not included in the `JsonParseResult.failure` variant.
pub const AllocError = error{
    OutOfMemory,
};

/// A union of all possible errors that can occur during parsing.
///
/// Includes:
/// - `ParseError`: structural, semantic, or limit-related JSON errors (returned in `.failure`)
/// - `AllocError`: memory allocation failures (thrown directly)
pub const ParseOrAllocError = (ParseError || AllocError);


pub const Offset = struct {
    column: usize,
    line: usize,
};

/// Detailed information about a structured JSON parsing failure.
///
/// This struct is used in the `.failure` variant of `JsonParseResult`.
/// It includes:
/// - The `ParseError` type.
/// - The input slice that caused the failure.
/// - The byte offset at which the failure occurred.
/// - An optional human-readable message (allocated).
pub const ParsingErrorInfo = struct {
    errorType: ParseError,
    input: []const u8,
    pos: usize = 0,
    message: ?[]const u8,

    /// Computes the line and column from the byte offset (`pos`) in the original input.
    pub fn getOffset(self: *const ParsingErrorInfo) Offset {
        var col: usize = 1;
        var line: usize = 1;

        for (self.input, 0..) |byte, index| {
            if (self.pos == index) break;
            if (byte == Character.newline.toByte()) {
                col = 1;
                line += 1;
            } else {
                col += 1;
            }
        }
        return Offset{ .column = col, .line = line };
    }

    /// Prints the error with source context, line/column, and caret for clarity.
    ///
    /// May fail with `OutOfMemory` if allocation for the caret buffer fails.
    pub fn printError(self: *const ParsingErrorInfo) error{OutOfMemory}!void {
        const offset = self.getOffset();
        if (self.message == null) {
            std.debug.print("{!} at position {}:{}\n", .{ self.errorType, offset.column, offset.column });
        } else {
            std.debug.print("{!} at position {}:{}, {s}\n", .{ self.errorType, offset.line, offset.column, self.message.? });
        }
        const snippetMaxSize: usize = 8;
        const startCol: usize = math.min(usize, offset.column, snippetMaxSize);
        var i: usize = math.max(usize, startCol, offset.column - 1);
        var endCol = i;
        while (i < offset.column + snippetMaxSize and i < self.input.len) {
            endCol = i;
            if (endCol == Character.newline.toByte()) {
                break;
            }
            i += 1;
        }
        const snippet = self.input[startCol..endCol];
        const alloc = std.heap.page_allocator;
        const caretSpaceBuffer = try alloc.alloc(u8, offset.column - startCol);
        defer alloc.free(caretSpaceBuffer);
        @memset(caretSpaceBuffer, Character.space.toByte());

        std.debug.print("\n{s}\n{s}^\n", .{ snippet, caretSpaceBuffer });
    }
};
