const std = @import("std");
const AllocError = @import("error.zig").AllocError;

pub fn Entry(comptime V: type) type {
    return  std.StringArrayHashMapUnmanaged(V).Entry;
}

pub fn OrderedStringHashMapIterator(comptime V: type) type {
    return struct {
        index: usize = 0,
        collection: *const OrderedStringHashMap(V),

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
        pub fn reset(self: *OrderedStringHashMapIterator(V)) void {
            self.index = 0;
        }
        pub fn remaining(self: *const OrderedStringHashMapIterator(V)) usize {
            const length = self.collection.keys.len;
            return length - self.index;
        }
    };
}

pub fn OrderedStringHashMapBuilder(comptime V: type) type {
    const List = std.ArrayListAligned([]const u8, null);
    const Map = std.StringArrayHashMapUnmanaged(V);
    return struct {
        list: List,
        map: Map,
        allocator: std.mem.Allocator,

        pub fn init(allocator: std.mem.Allocator) AllocError!OrderedStringHashMapBuilder(V) {
            const list = List.init(allocator);
            const map = try Map.init(allocator, &[_][]const u8{}, &[_]V{});
            return OrderedStringHashMapBuilder(V){.list = list, .map = map, .allocator = allocator};
        }
        /// Inserts a key and value. 
        /// 
        /// Returns `true` if the key was newly inserted,
        /// or `false` if it replaced an existing value.
        pub fn put(self: *OrderedStringHashMapBuilder(V), key: []const u8, value: V) !bool {
            const result = try self.map.getOrPut(self.allocator, key);
            if (!result.found_existing) {
                try self.list.append(key);
            }
            result.value_ptr.* = value;

            return result.found_existing;
        }

        pub fn freeze(self: *OrderedStringHashMapBuilder(V)) AllocError!OrderedStringHashMap(V) {
            return OrderedStringHashMap(V){
                .keys = try self.list.toOwnedSlice(),
                .internal_map = self.map,
            };
        }
        pub fn deinit(self: *OrderedStringHashMapBuilder(V)) void {
            self.allocator.free(self.list);
            self.allocator.free(self.map);
        }
    };
}

pub fn OrderedStringHashMap(comptime V: type) type {
    const Iterator = OrderedStringHashMapIterator(V);
    return struct {
        keys: []const []const u8,
        /// Internal lookup map for fast key → value access.
        /// 
        /// Values are backed by the same allocator used during construction.
        /// 
        /// Do not modify directly.
        internal_map: std.StringArrayHashMapUnmanaged(V),

        pub fn get(self: *const OrderedStringHashMap(V), key: []const u8) ?V {
            return self.internal_map.get(key);
        }

        pub fn contains(self: *const OrderedStringHashMap(V), key: []const u8) bool {
            return self.internal_map.contains(key);
        }

        pub fn values(self: *const OrderedStringHashMap(V), allocator: std.mem.Allocator) AllocError![]const V {
            var list = try std.ArrayList(V).initCapacity(allocator, self.keys.len);
            defer list.deinit();
            for (self.keys) |key| {
                list.append(self.internal_map.get(key) orelse unreachable);
            }
            return list.toOwnedSlice();
        }
        pub fn iter(self: *const OrderedStringHashMap(V)) Iterator {
            return Iterator{.collection = self};
        }
    };
}