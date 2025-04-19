const std = @import("std");
const AllocError = @import("error.zig").AllocError;

/// Returns the iterator entry type used in both ordered and unordered hash maps.
/// Provides pointers to the key and value stored internally.
pub fn Entry(comptime V: type) type {
    return std.StringArrayHashMapUnmanaged(V).Entry;
}

/// An iterator over an `OrderedStringHashMap(V)`, preserving key insertion order.
/// 
/// Yields key-value entries in the order they were added to the map.
pub fn OrderedStringHashMapIterator(comptime V: type) type {
    return struct {
        index: usize = 0,
        collection: *const OrderedStringHashMap(V),

        /// Returns the next entry in the map, or null if iteration is complete.
        /// Keys are returned in insertion order.
        pub fn next(self: *OrderedStringHashMapIterator(V)) ?Entry(V) {
            if (self.remaining() == 0) return null;
            const key = self.collection.keys[self.index];
            const key_ptr = self.collection.internal_map.getKeyPtr(key) orelse unreachable;
            const value_ptr = self.collection.internal_map.getPtr(key) orelse unreachable;
            self.index += 1;
            return Entry(V) {
                .key_ptr = key_ptr,
                .value_ptr = value_ptr,
            };
        }

        /// Resets the iterator to the beginning.
        pub fn reset(self: *OrderedStringHashMapIterator(V)) void {
            self.index = 0;
        }

        /// Returns the number of entries remaining in the iteration.
        pub fn remaining(self: *const OrderedStringHashMapIterator(V)) usize {
            return self.collection.keys.len - self.index;
        }
    };
}

/// A builder for creating an `OrderedStringHashMap(V)` incrementally.
/// 
/// Preserves insertion order of keys and supports optional overwrite detection.
pub fn OrderedStringHashMapBuilder(comptime V: type) type {
    const List = std.ArrayListAligned([]const u8, null);
    const Map = std.StringArrayHashMapUnmanaged(V);

    return struct {
        list: List,
        map: Map,
        allocator: std.mem.Allocator,

        /// Initializes a new builder with the provided allocator.
        pub fn init(allocator: std.mem.Allocator) AllocError!OrderedStringHashMapBuilder(V) {
            const list = List.init(allocator);
            const map = try Map.init(allocator, &[_][]const u8{}, &[_]V{});
            return .{ .list = list, .map = map, .allocator = allocator };
        }

        /// Inserts a key and value.
        ///
        /// Returns `true` if the key already existed and was overwritten,
        /// or `false` if it was newly inserted.
        pub fn put(self: *OrderedStringHashMapBuilder(V), key: []const u8, value: V) !bool {
            const result = try self.map.getOrPut(self.allocator, key);
            if (!result.found_existing) {
                try self.list.append(key);
            }
            result.value_ptr.* = value;
            return result.found_existing;
        }

        /// Finalizes the builder and returns an immutable `OrderedStringHashMap(V)`.
        ///
        /// After calling this, the builder must not be used.
        pub fn freeze(self: *OrderedStringHashMapBuilder(V)) AllocError!OrderedStringHashMap(V) {
            return OrderedStringHashMap(V){
                .keys = try self.list.toOwnedSlice(),
                .internal_map = self.map,
            };
        }

        /// Frees all allocations used by the builder.
        /// 
        /// ⚠️ Should only be used if `freeze()` is **not** called.
        pub fn deinit(self: *OrderedStringHashMapBuilder(V)) void {
            self.list.deinit();
            self.map.deinit(self.allocator);
        }
    };
}

/// An immutable map of string keys to values of type `V`,
/// preserving the insertion order of keys.
///
/// Lookups are fast using an internal hash map, while iteration yields keys
/// in the same order they were added.
pub fn OrderedStringHashMap(comptime V: type) type {
    const Iterator = OrderedStringHashMapIterator(V);
    return struct {
        /// Keys in insertion order.
        keys: []const []const u8,

        /// Internal hash map for fast key → value lookup.
        ///
        /// Do not mutate directly. Values are backed by the same allocator
        /// used during builder construction.
        internal_map: std.StringArrayHashMapUnmanaged(V),

        /// Retrieves the value for a given key, or null if not found.
        pub fn get(self: *const OrderedStringHashMap(V), key: []const u8) ?V {
            return self.internal_map.get(key);
        }

        /// Returns `true` if the map contains the given key.
        pub fn contains(self: *const OrderedStringHashMap(V), key: []const u8) bool {
            return self.internal_map.contains(key);
        }

        /// Returns all values in the order their keys were inserted.
        ///
        /// Allocates a new slice using the given allocator.
        pub fn values(self: *const OrderedStringHashMap(V), allocator: std.mem.Allocator) AllocError![]const V {
            var list = try std.ArrayList(V).initCapacity(allocator, self.keys.len);
            defer list.deinit();
            for (self.keys) |key| {
                try list.append(self.internal_map.get(key) orelse unreachable);
            }
            return list.toOwnedSlice();
        }

        /// Returns an iterator over the map, yielding entries in insertion order.
        pub fn iter(self: *const OrderedStringHashMap(V)) Iterator {
            return Iterator{ .collection = self };
        }

        /// Frees all memory associated with this map, including key order tracking and internal storage.
        ///
        /// This must be called if the map was created using a standard allocator. If the map was created
        /// using an arena allocator, calling this is unnecessary and may be invalid.
        ///
        /// The same allocator used during construction (via the builder) must be provided here.
        pub fn deinit(self: *OrderedStringHashMap(V), allocator: std.mem.Allocator) void {
            allocator.free(self.keys);
            self.internal_map.deinit(allocator);
        }
    };
}
