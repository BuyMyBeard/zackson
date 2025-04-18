const std = @import("std");
const Character = @import("character.zig").Character;
const math = @import("math.zig");

pub fn formatExpectMessage(alloc: std.mem.Allocator, characters: []const Character, received: u8) error{OutOfMemory}![]u8 {
    var charsAlloc = std.heap.page_allocator;
    const formattedChars = try Character.join(charsAlloc, characters, ", ");
    defer charsAlloc.free(formattedChars);
    return try std.fmt.allocPrint(alloc, "expected one of '{s}' but got '{c}'", .{ formattedChars, received });
}

pub const ParseError = error{
    UnexpectedToken,
    InvalidUnicodeSequence,
    UnexpectedEOF,
    MaxDepthReached,
    InvalidValue,
};

pub const ParseOrAllocError = (ParseError || error{OutOfMemory});

pub const Offset = struct {
    column: usize,
    line: usize,
};

pub const ParsingErrorInfo = struct {
    errorType: ParseError,
    input: []const u8,
    pos: usize = 0,
    message: ?[]const u8,

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
