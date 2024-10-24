const std = @import("std");
const Allocator = std.mem.Allocator;
const res = @import("res.zig");
const NameOrOrdinal = res.NameOrOrdinal;
const MemoryFlags = res.MemoryFlags;
const Language = res.Language;
const numPaddingBytesNeeded = @import("compile.zig").Compiler.numPaddingBytesNeeded;

pub const Resource = struct {
    type_value: NameOrOrdinal,
    name_value: NameOrOrdinal,
    data_version: u32,
    memory_flags: MemoryFlags,
    language: Language,
    version: u32,
    characteristics: u32,
    data: []const u8,

    pub fn deinit(self: Resource, allocator: Allocator) void {
        self.name_value.deinit(allocator);
        self.type_value.deinit(allocator);
        allocator.free(self.data);
    }

    /// Returns true if all fields match the expected value of the resource at the
    /// start of all .res files that distinguishes the .res file as 32-bit (as
    /// opposed to 16-bit).
    pub fn is32BitPreface(self: Resource) bool {
        if (self.type_value != .ordinal or self.type_value.ordinal != 0) return false;
        if (self.name_value != .ordinal or self.name_value.ordinal != 0) return false;
        if (self.data_version != 0) return false;
        if (@as(u16, @bitCast(self.memory_flags)) != 0) return false;
        if (@as(u16, @bitCast(self.language)) != 0) return false;
        if (self.version != 0) return false;
        if (self.characteristics != 0) return false;
        if (self.data.len != 0) return false;
        return true;
    }
};

pub const ParseResOptions = struct {
    skip_zero_data_resources: bool = true,
    max_size: u64,
};

/// The returned slice should be freed by calling `freeResources`.
pub fn parseRes(allocator: Allocator, reader: anytype, options: ParseResOptions) ![]Resource {
    var bytes_remaining: u64 = options.max_size;
    {
        const first_resource_and_size = try parseResource(allocator, reader, bytes_remaining);
        defer first_resource_and_size.resource.deinit(allocator);
        if (!first_resource_and_size.resource.is32BitPreface()) return error.InvalidPreface;
        bytes_remaining -= first_resource_and_size.total_size;
    }

    var resources: std.ArrayListUnmanaged(Resource) = .empty;
    errdefer {
        for (resources.items) |resource| {
            resource.deinit(allocator);
        }
        resources.deinit(allocator);
    }

    while (bytes_remaining != 0) {
        const resource_and_size = try parseResource(allocator, reader, bytes_remaining);
        if (options.skip_zero_data_resources and resource_and_size.resource.data.len == 0) {
            resource_and_size.resource.deinit(allocator);
        } else {
            errdefer resource_and_size.resource.deinit(allocator);
            try resources.append(allocator, resource_and_size.resource);
        }
        bytes_remaining -= resource_and_size.total_size;
    }

    return try resources.toOwnedSlice(allocator);
}

pub fn freeResources(allocator: Allocator, resources: []const Resource) void {
    for (resources) |resource| {
        resource.deinit(allocator);
    }
    allocator.free(resources);
}

pub const ResourceAndSize = struct {
    resource: Resource,
    total_size: u64,
};

pub fn parseResource(allocator: Allocator, reader: anytype, max_size: u64) !ResourceAndSize {
    var header_counting_reader = std.io.countingReader(reader);
    const header_reader = header_counting_reader.reader();
    const data_size = try header_reader.readInt(u32, .little);
    const header_size = try header_reader.readInt(u32, .little);
    const total_size: u64 = @as(u64, header_size) + data_size;
    if (total_size > max_size) return error.ImpossibleSize;

    var header_bytes_available = header_size -| 8;
    var type_reader = std.io.limitedReader(header_reader, header_bytes_available);
    const type_value = try parseNameOrOrdinal(allocator, type_reader.reader());
    errdefer type_value.deinit(allocator);

    header_bytes_available -|= @intCast(type_value.byteLen());
    var name_reader = std.io.limitedReader(header_reader, header_bytes_available);
    const name_value = try parseNameOrOrdinal(allocator, name_reader.reader());
    errdefer name_value.deinit(allocator);

    const padding_after_name = numPaddingBytesNeeded(@intCast(header_counting_reader.bytes_read));
    try header_reader.skipBytes(padding_after_name, .{ .buf_size = 3 });

    std.debug.assert(header_counting_reader.bytes_read % 4 == 0);
    const data_version = try header_reader.readInt(u32, .little);
    const memory_flags: MemoryFlags = @bitCast(try header_reader.readInt(u16, .little));
    const language: Language = @bitCast(try header_reader.readInt(u16, .little));
    const version = try header_reader.readInt(u32, .little);
    const characteristics = try header_reader.readInt(u32, .little);

    const header_bytes_read = header_counting_reader.bytes_read;
    if (header_size != header_bytes_read) return error.HeaderSizeMismatch;

    const data = try allocator.alloc(u8, data_size);
    errdefer allocator.free(data);
    try reader.readNoEof(data);

    const padding_after_data = numPaddingBytesNeeded(@intCast(data_size));
    try reader.skipBytes(padding_after_data, .{ .buf_size = 3 });

    return .{
        .resource = .{
            .name_value = name_value,
            .type_value = type_value,
            .language = language,
            .memory_flags = memory_flags,
            .version = version,
            .characteristics = characteristics,
            .data_version = data_version,
            .data = data,
        },
        .total_size = header_size + data.len + padding_after_data,
    };
}

pub fn parseNameOrOrdinal(allocator: Allocator, reader: anytype) !NameOrOrdinal {
    const first_code_unit = try reader.readInt(u16, .little);
    if (first_code_unit == 0xFFFF) {
        const ordinal_value = try reader.readInt(u16, .little);
        return .{ .ordinal = ordinal_value };
    }
    var name_buf = try std.ArrayListUnmanaged(u16).initCapacity(allocator, 16);
    errdefer name_buf.deinit(allocator);
    var code_unit = first_code_unit;
    while (code_unit != 0) {
        try name_buf.append(allocator, std.mem.nativeToLittle(u16, code_unit));
        code_unit = try reader.readInt(u16, .little);
    }
    return .{ .name = try name_buf.toOwnedSliceSentinel(allocator, 0) };
}

// https://learn.microsoft.com/en-us/windows/win32/debug/pe-format#type-indicators
pub fn rvaRelocationTypeIndicator(target: std.coff.MachineType) u16 {
    return switch (target) {
        .X64 => 0x3, // IMAGE_REL_AMD64_ADDR32NB
        .I386 => 0x7, // IMAGE_REL_I386_DIR32NB
        .ARM, .ARMNT => 0x2, // IMAGE_REL_ARM_ADDR32NB
        .ARM64, .ARM64EC, .ARM64X => 0x2, // IMAGE_REL_ARM64_ADDR32NB
        .IA64 => 0x10, // IMAGE_REL_IA64_DIR32NB
        .EBC => 0x1, // This is what cvtres.exe writes for this target, unsure where it comes from
        else => 0,
    };
}

pub const CoffOptions = struct {
    target: std.coff.MachineType = .X64,
    /// If true, zeroes will be written to all timestamp fields
    reproducible: bool = true,
};

pub fn writeCoff(allocator: Allocator, writer: anytype, resources: []const Resource, options: CoffOptions) !void {
    var resource_tree = ResourceTree.init(allocator);
    defer resource_tree.deinit();

    for (resources, 0..) |*resource, i| {
        try resource_tree.put(resource, i);
    }

    const lengths = resource_tree.dataLengths();
    const byte_size_of_relocation = 10;
    const relocations_len: u32 = @intCast(byte_size_of_relocation * resources.len);
    const pointer_to_rsrc01_data = @sizeOf(std.coff.CoffHeader) + (@sizeOf(std.coff.SectionHeader) * 2);
    const pointer_to_relocations = pointer_to_rsrc01_data + lengths.rsrc01;
    const pointer_to_rsrc02_data = pointer_to_relocations + relocations_len;
    const pointer_to_symbol_table = pointer_to_rsrc02_data + lengths.rsrc02;

    const timestamp: i64 = if (options.reproducible) 0 else std.time.timestamp();
    const size_of_optional_header = 0;
    const machine_type: std.coff.MachineType = options.target;
    const flags = std.coff.CoffHeaderFlags{
        .@"32BIT_MACHINE" = 1,
    };
    const coff_header = std.coff.CoffHeader{
        .machine = machine_type,
        .number_of_sections = 2,
        .time_date_stamp = @as(u32, @truncate(@as(u64, @bitCast(timestamp)))),
        .pointer_to_symbol_table = pointer_to_symbol_table,
        .number_of_symbols = 5 + @as(u32, @intCast(resources.len)),
        .size_of_optional_header = size_of_optional_header,
        .flags = flags,
    };

    try writer.writeStructEndian(coff_header, .little);

    const rsrc01_header = std.coff.SectionHeader{
        .name = ".rsrc$01".*,
        .virtual_size = 0,
        .virtual_address = 0,
        .size_of_raw_data = lengths.rsrc01,
        .pointer_to_raw_data = pointer_to_rsrc01_data,
        .pointer_to_relocations = if (relocations_len != 0) pointer_to_relocations else 0,
        .pointer_to_linenumbers = 0,
        .number_of_relocations = @intCast(resources.len),
        .number_of_linenumbers = 0,
        .flags = .{
            .CNT_INITIALIZED_DATA = 1,
            .MEM_WRITE = 1,
            .MEM_READ = 1,
        },
    };
    try writer.writeStructEndian(rsrc01_header, .little);

    const rsrc02_header = std.coff.SectionHeader{
        .name = ".rsrc$02".*,
        .virtual_size = 0,
        .virtual_address = 0,
        .size_of_raw_data = lengths.rsrc02,
        .pointer_to_raw_data = pointer_to_rsrc02_data,
        .pointer_to_relocations = 0,
        .pointer_to_linenumbers = 0,
        .number_of_relocations = 0,
        .number_of_linenumbers = 0,
        .flags = .{
            .CNT_INITIALIZED_DATA = 1,
            .MEM_WRITE = 1,
            .MEM_READ = 1,
        },
    };
    try writer.writeStructEndian(rsrc02_header, .little);

    // TODO: test surrogate pairs
    try resource_tree.sort();

    const resource_symbols = try resource_tree.writeCoff(
        allocator,
        writer,
        resources,
        lengths,
        options,
    );
    defer allocator.free(resource_symbols);

    try writeSymbol(writer, .{
        .name = "@feat.00".*,
        .value = 0x11,
        .section_number = .ABSOLUTE,
        .type = .{
            .base_type = .NULL,
            .complex_type = .NULL,
        },
        .storage_class = .STATIC,
        .number_of_aux_symbols = 0,
    });

    try writeSymbol(writer, .{
        .name = ".rsrc$01".*,
        .value = 0,
        .section_number = @enumFromInt(1),
        .type = .{
            .base_type = .NULL,
            .complex_type = .NULL,
        },
        .storage_class = .STATIC,
        .number_of_aux_symbols = 1,
    });
    try writeSectionDefinition(writer, .{
        .length = lengths.rsrc01,
        .number_of_relocations = @intCast(resources.len),
        .number_of_linenumbers = 0,
        .checksum = 0,
        .number = 0,
        .selection = .NONE,
        .unused = .{0} ** 3,
    });

    try writeSymbol(writer, .{
        .name = ".rsrc$02".*,
        .value = 0,
        .section_number = @enumFromInt(2),
        .type = .{
            .base_type = .NULL,
            .complex_type = .NULL,
        },
        .storage_class = .STATIC,
        .number_of_aux_symbols = 1,
    });
    try writeSectionDefinition(writer, .{
        .length = lengths.rsrc02,
        .number_of_relocations = 0,
        .number_of_linenumbers = 0,
        .checksum = 0,
        .number = 0,
        .selection = .NONE,
        .unused = .{0} ** 3,
    });

    for (resource_symbols) |resource_symbol| {
        try writeSymbol(writer, resource_symbol);
    }

    const string_table_byte_count = 4;
    try writer.writeInt(u32, string_table_byte_count, .little);
}

fn writeSymbol(writer: anytype, symbol: std.coff.Symbol) !void {
    try writer.writeAll(&symbol.name);
    try writer.writeInt(u32, symbol.value, .little);
    try writer.writeInt(u16, @intFromEnum(symbol.section_number), .little);
    try writer.writeInt(u8, @intFromEnum(symbol.type.base_type), .little);
    try writer.writeInt(u8, @intFromEnum(symbol.type.complex_type), .little);
    try writer.writeInt(u8, @intFromEnum(symbol.storage_class), .little);
    try writer.writeInt(u8, symbol.number_of_aux_symbols, .little);
}

fn writeSectionDefinition(writer: anytype, def: std.coff.SectionDefinition) !void {
    try writer.writeInt(u32, def.length, .little);
    try writer.writeInt(u16, def.number_of_relocations, .little);
    try writer.writeInt(u16, def.number_of_linenumbers, .little);
    try writer.writeInt(u32, def.checksum, .little);
    try writer.writeInt(u16, def.number, .little);
    try writer.writeInt(u8, @intFromEnum(def.selection), .little);
    try writer.writeAll(&def.unused);
}

pub const ResourceDirectoryTable = extern struct {
    characteristics: u32,
    timestamp: u32,
    major_version: u16,
    minor_version: u16,
    number_of_name_entries: u16,
    number_of_id_entries: u16,
};

pub const ResourceDirectoryEntry = extern struct {
    entry: packed union {
        name_offset: packed struct(u32) {
            address: u31,
            /// This is undocumented in the PE/COFF spec, but the high bit
            /// is set by cvtres.exe for string addresses
            to_string: bool = true,
        },
        integer_id: u32,
    },
    offset: packed struct(u32) {
        address: u31,
        to_subdirectory: bool,
    },

    pub fn writeCoff(self: ResourceDirectoryEntry, writer: anytype) !void {
        try writer.writeInt(u32, @bitCast(self.entry), .little);
        try writer.writeInt(u32, @bitCast(self.offset), .little);
    }
};

pub const ResourceDataEntry = extern struct {
    data_rva: u32,
    size: u32,
    codepage: u32,
    reserved: u32 = 0,
};

/// type -> name -> language
const ResourceTree = struct {
    type_to_name_map: std.ArrayHashMapUnmanaged(NameOrOrdinal, NameToLanguageMap, NameOrOrdinalHashContext, true),
    string_table: std.ArrayHashMapUnmanaged(NameOrOrdinal, void, NameOrOrdinalHashContext, true),
    allocator: Allocator,

    const RelocatableResource = struct {
        resource: *const Resource,
        original_index: usize,
    };
    const LanguageToResourceMap = std.AutoArrayHashMapUnmanaged(Language, RelocatableResource);
    const NameToLanguageMap = std.ArrayHashMapUnmanaged(NameOrOrdinal, LanguageToResourceMap, NameOrOrdinalHashContext, true);

    const NameOrOrdinalHashContext = struct {
        pub fn hash(self: @This(), v: NameOrOrdinal) u32 {
            _ = self;
            var hasher = std.hash.Wyhash.init(0);
            const tag = std.meta.activeTag(v);
            hasher.update(std.mem.asBytes(&tag));
            switch (v) {
                .name => |name| {
                    hasher.update(std.mem.asBytes(name));
                },
                .ordinal => |*ordinal| {
                    hasher.update(std.mem.asBytes(ordinal));
                },
            }
            return @truncate(hasher.final());
        }
        pub fn eql(self: @This(), a: NameOrOrdinal, b: NameOrOrdinal, b_index: usize) bool {
            _ = self;
            _ = b_index;
            const tag_a = std.meta.activeTag(a);
            const tag_b = std.meta.activeTag(b);
            if (tag_a != tag_b) return false;

            return switch (a) {
                .name => std.mem.eql(u16, a.name, b.name),
                .ordinal => a.ordinal == b.ordinal,
            };
        }
    };

    pub fn init(allocator: Allocator) ResourceTree {
        return .{
            .type_to_name_map = .empty,
            .string_table = .empty,
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *ResourceTree) void {
        for (self.type_to_name_map.values()) |*name_to_lang_map| {
            for (name_to_lang_map.values()) |*lang_to_resources_map| {
                lang_to_resources_map.deinit(self.allocator);
            }
            name_to_lang_map.deinit(self.allocator);
        }
        self.type_to_name_map.deinit(self.allocator);
        self.string_table.deinit(self.allocator);
    }

    pub fn put(self: *ResourceTree, resource: *const Resource, original_index: usize) !void {
        const name_to_lang_map = blk: {
            const gop_result = try self.type_to_name_map.getOrPut(self.allocator, resource.type_value);
            if (!gop_result.found_existing) {
                gop_result.value_ptr.* = .empty;
            }
            break :blk gop_result.value_ptr;
        };
        const lang_to_resources_map = blk: {
            const gop_result = try name_to_lang_map.getOrPut(self.allocator, resource.name_value);
            if (!gop_result.found_existing) {
                gop_result.value_ptr.* = .empty;
            }
            break :blk gop_result.value_ptr;
        };
        {
            const gop_result = try lang_to_resources_map.getOrPut(self.allocator, resource.language);
            if (gop_result.found_existing) return error.DuplicateResource;
            gop_result.value_ptr.* = .{
                .original_index = original_index,
                .resource = resource,
            };
        }

        if (resource.type_value == .name and !self.string_table.contains(resource.type_value)) {
            try self.string_table.putNoClobber(self.allocator, resource.type_value, {});
        }
        if (resource.name_value == .name and !self.string_table.contains(resource.name_value)) {
            try self.string_table.putNoClobber(self.allocator, resource.name_value, {});
        }
    }

    const Lengths = struct {
        level1: u32,
        level2: u32,
        level3: u32,
        data_entries: u32,
        strings: u32,
        padding: u32,

        rsrc01: u32,
        rsrc02: u32,

        fn stringsStart(self: Lengths) u32 {
            return self.rsrc01 - self.strings - self.padding;
        }
    };

    pub fn dataLengths(self: *const ResourceTree) Lengths {
        var lengths: Lengths = .{
            .level1 = 0,
            .level2 = 0,
            .level3 = 0,
            .data_entries = 0,
            .strings = 0,
            .padding = 0,
            .rsrc01 = undefined,
            .rsrc02 = 0,
        };
        lengths.level1 += @sizeOf(ResourceDirectoryTable);
        for (self.type_to_name_map.values()) |name_to_lang_map| {
            lengths.level1 += @sizeOf(ResourceDirectoryEntry);
            lengths.level2 += @sizeOf(ResourceDirectoryTable);
            for (name_to_lang_map.values()) |lang_to_resources_map| {
                lengths.level2 += @sizeOf(ResourceDirectoryEntry);
                lengths.level3 += @sizeOf(ResourceDirectoryTable);
                for (lang_to_resources_map.values()) |reloc_resource| {
                    lengths.level3 += @sizeOf(ResourceDirectoryEntry);
                    lengths.data_entries += @sizeOf(ResourceDataEntry);
                    lengths.rsrc02 += std.mem.alignForward(u32, @intCast(reloc_resource.resource.data.len), 8);
                }
            }
        }
        for (self.string_table.keys()) |v| {
            lengths.strings += @sizeOf(u16); // string length
            lengths.strings += @intCast(v.name.len * @sizeOf(u16));
        }
        lengths.rsrc01 = lengths.level1 + lengths.level2 + lengths.level3 + lengths.data_entries + lengths.strings;
        lengths.padding = @intCast((4 -% lengths.rsrc01) % 4);
        lengths.rsrc01 += lengths.padding;
        return lengths;
    }

    pub fn sort(self: *ResourceTree) !void {
        const NameOrOrdinalSortContext = struct {
            keys: []NameOrOrdinal,

            pub fn lessThan(ctx: @This(), a_index: usize, b_index: usize) bool {
                const a = ctx.keys[a_index];
                const b = ctx.keys[b_index];
                if (std.meta.activeTag(a) != std.meta.activeTag(b)) {
                    return if (a == .name) true else false;
                }
                switch (a) {
                    .name => {
                        const n = @min(a.name.len, b.name.len);
                        for (a.name[0..n], b.name[0..n]) |a_c, b_c| {
                            switch (std.math.order(std.mem.littleToNative(u16, a_c), std.mem.littleToNative(u16, b_c))) {
                                .eq => continue,
                                .lt => return true,
                                .gt => return false,
                            }
                        }
                        return a.name.len < b.name.len;
                    },
                    .ordinal => {
                        return a.ordinal < b.ordinal;
                    },
                }
            }
        };
        self.type_to_name_map.sortUnstable(NameOrOrdinalSortContext{ .keys = self.type_to_name_map.keys() });
        for (self.type_to_name_map.values()) |*name_to_lang_map| {
            name_to_lang_map.sortUnstable(NameOrOrdinalSortContext{ .keys = name_to_lang_map.keys() });
        }
        const LangSortContext = struct {
            keys: []Language,

            pub fn lessThan(ctx: @This(), a_index: usize, b_index: usize) bool {
                return @as(u16, @bitCast(ctx.keys[a_index])) < @as(u16, @bitCast(ctx.keys[b_index]));
            }
        };
        for (self.type_to_name_map.values()) |*name_to_lang_map| {
            for (name_to_lang_map.values()) |*lang_to_resource_map| {
                lang_to_resource_map.sortUnstable(LangSortContext{ .keys = lang_to_resource_map.keys() });
            }
        }
    }

    pub fn writeCoff(self: *const ResourceTree, allocator: Allocator, writer: anytype, resources_in_data_order: []const Resource, lengths: Lengths, options: CoffOptions) ![]const std.coff.Symbol {
        if (self.type_to_name_map.count() == 0) {
            try writer.writeByteNTimes(0, 16);
            return &.{};
        }

        var counting_writer = std.io.countingWriter(writer);
        const w = counting_writer.writer();

        var level2_list: std.ArrayListUnmanaged(*const NameToLanguageMap) = .empty;
        defer level2_list.deinit(allocator);

        var level3_list: std.ArrayListUnmanaged(*const LanguageToResourceMap) = .empty;
        defer level3_list.deinit(allocator);

        var resources_list: std.ArrayListUnmanaged(*const RelocatableResource) = .empty;
        defer resources_list.deinit(allocator);

        var relocations = Relocations.init(allocator);
        defer relocations.deinit();

        var string_offsets = try allocator.alloc(u31, self.string_table.count());
        const strings_start = lengths.stringsStart();
        defer allocator.free(string_offsets);
        {
            var string_address: u31 = @intCast(strings_start);
            for (self.string_table.keys(), 0..) |v, i| {
                string_offsets[i] = string_address;
                string_address += @sizeOf(u16) + @as(u31, @intCast(v.name.len * @sizeOf(u16)));
            }
        }

        const level2_start = lengths.level1;
        var level2_address = level2_start;
        {
            const counts = entryTypeCounts(self.type_to_name_map.keys());
            const table = ResourceDirectoryTable{
                .characteristics = 0,
                .timestamp = 0,
                .major_version = 0,
                .minor_version = 0,
                .number_of_id_entries = counts.ids,
                .number_of_name_entries = counts.names,
            };
            try w.writeStructEndian(table, .little);

            var it = self.type_to_name_map.iterator();
            while (it.next()) |entry| {
                const type_value = entry.key_ptr;
                const dir_entry = ResourceDirectoryEntry{
                    .entry = switch (type_value.*) {
                        .name => .{ .name_offset = .{ .address = string_offsets[self.string_table.getIndex(type_value.*).?] } },
                        .ordinal => .{ .integer_id = type_value.ordinal },
                    },
                    .offset = .{
                        .address = @intCast(level2_address),
                        .to_subdirectory = true,
                    },
                };
                try dir_entry.writeCoff(w);
                level2_address += @sizeOf(ResourceDirectoryTable) + @as(u32, @intCast(entry.value_ptr.count() * @sizeOf(ResourceDirectoryEntry)));

                const name_to_lang_map = entry.value_ptr;
                try level2_list.append(allocator, name_to_lang_map);
            }
        }
        std.debug.assert(counting_writer.bytes_written == level2_start);

        const level3_start = level2_start + lengths.level2;
        var level3_address = level3_start;
        for (level2_list.items) |name_to_lang_map| {
            const counts = entryTypeCounts(name_to_lang_map.keys());
            const table = ResourceDirectoryTable{
                .characteristics = 0,
                .timestamp = 0,
                .major_version = 0,
                .minor_version = 0,
                .number_of_id_entries = counts.ids,
                .number_of_name_entries = counts.names,
            };
            try w.writeStructEndian(table, .little);

            var it = name_to_lang_map.iterator();
            while (it.next()) |entry| {
                const name_value = entry.key_ptr;
                const dir_entry = ResourceDirectoryEntry{
                    .entry = switch (name_value.*) {
                        .name => .{ .name_offset = .{ .address = string_offsets[self.string_table.getIndex(name_value.*).?] } },
                        .ordinal => .{ .integer_id = name_value.ordinal },
                    },
                    .offset = .{
                        .address = @intCast(level3_address),
                        .to_subdirectory = true,
                    },
                };
                try dir_entry.writeCoff(w);
                level3_address += @sizeOf(ResourceDirectoryTable) + @as(u32, @intCast(entry.value_ptr.count() * @sizeOf(ResourceDirectoryEntry)));

                const lang_to_resources_map = entry.value_ptr;
                try level3_list.append(allocator, lang_to_resources_map);
            }
        }
        std.debug.assert(counting_writer.bytes_written == level3_start);

        var reloc_addresses = try allocator.alloc(u32, resources_in_data_order.len);
        defer allocator.free(reloc_addresses);

        const data_entries_start = level3_start + lengths.level3;
        var data_entry_address = data_entries_start;
        for (level3_list.items) |lang_to_resources_map| {
            const counts = EntryTypeCounts{
                .names = 0,
                .ids = @intCast(lang_to_resources_map.count()),
            };
            const table = ResourceDirectoryTable{
                .characteristics = 0,
                .timestamp = 0,
                .major_version = 0,
                .minor_version = 0,
                .number_of_id_entries = counts.ids,
                .number_of_name_entries = counts.names,
            };
            try w.writeStructEndian(table, .little);

            var it = lang_to_resources_map.iterator();
            while (it.next()) |entry| {
                const lang = entry.key_ptr.*;
                const dir_entry = ResourceDirectoryEntry{
                    .entry = .{ .integer_id = lang.asInt() },
                    .offset = .{
                        .address = @intCast(data_entry_address),
                        .to_subdirectory = false,
                    },
                };

                const reloc_resource = entry.value_ptr;
                reloc_addresses[reloc_resource.original_index] = @intCast(data_entry_address);

                try dir_entry.writeCoff(w);
                data_entry_address += @sizeOf(ResourceDataEntry);

                try resources_list.append(allocator, reloc_resource);
            }
        }
        std.debug.assert(counting_writer.bytes_written == data_entries_start);

        for (resources_list.items, 0..) |reloc_resource, i| {
            // TODO: This logic works but is convoluted, would be good to clean this up
            const orig_resource = &resources_in_data_order[reloc_resource.original_index];
            const address: u32 = reloc_addresses[i];
            try relocations.add(address, &resources_in_data_order[i]);
            const data_entry = ResourceDataEntry{
                .data_rva = 0, // relocation
                .size = @intCast(orig_resource.data.len),
                .codepage = 0,
            };
            try w.writeStructEndian(data_entry, .little);
        }
        std.debug.assert(counting_writer.bytes_written == strings_start);

        for (self.string_table.keys()) |v| {
            const str = v.name;
            try w.writeInt(u16, @intCast(str.len), .little);
            try w.writeAll(std.mem.sliceAsBytes(str));
        }

        try w.writeByteNTimes(0, lengths.padding);

        for (relocations.list.items) |relocation| {
            try writeRelocation(w, std.coff.Relocation{
                .virtual_address = relocation.relocation_address,
                .symbol_table_index = relocation.symbol_index,
                .type = rvaRelocationTypeIndicator(options.target),
            });
        }

        for (resources_in_data_order) |resource| {
            const padding_bytes: u4 = @intCast((8 -% resource.data.len) % 8);
            try w.writeAll(resource.data);
            try w.writeByteNTimes(0, padding_bytes);
        }

        var symbols = try allocator.alloc(std.coff.Symbol, resources_list.items.len);
        errdefer allocator.free(symbols);

        for (relocations.list.items, 0..) |relocation, i| {
            var name_buf: [8]u8 = undefined;
            const name_slice = try std.fmt.bufPrint(&name_buf, "$R{X:0>6}", .{relocation.data_offset});
            std.debug.assert(name_slice.len == 8);
            symbols[i] = .{
                .name = name_buf,
                .value = relocation.data_offset,
                .section_number = @enumFromInt(2),
                .type = .{
                    .base_type = .NULL,
                    .complex_type = .NULL,
                },
                .storage_class = .STATIC,
                .number_of_aux_symbols = 0,
            };
        }

        return symbols;
    }

    fn writeRelocation(writer: anytype, relocation: std.coff.Relocation) !void {
        try writer.writeInt(u32, relocation.virtual_address, .little);
        try writer.writeInt(u32, relocation.symbol_table_index, .little);
        try writer.writeInt(u16, relocation.type, .little);
    }

    const EntryTypeCounts = struct {
        names: u16,
        ids: u16,
    };

    fn entryTypeCounts(s: []const NameOrOrdinal) EntryTypeCounts {
        var names: u16 = 0;
        var ordinals: u16 = 0;
        for (s) |v| {
            switch (v) {
                .name => names += 1,
                .ordinal => ordinals += 1,
            }
        }
        return .{ .names = names, .ids = ordinals };
    }
};

const Relocation = struct {
    symbol_index: u32,
    data_offset: u32,
    relocation_address: u32,
};

const Relocations = struct {
    allocator: Allocator,
    list: std.ArrayListUnmanaged(Relocation) = .empty,
    cur_symbol_index: u32 = 5,
    cur_data_offset: u32 = 0,

    pub fn init(allocator: Allocator) Relocations {
        return .{ .allocator = allocator };
    }

    pub fn deinit(self: *Relocations) void {
        self.list.deinit(self.allocator);
    }

    pub fn add(self: *Relocations, relocation_address: u32, resource: *const Resource) !void {
        try self.list.append(self.allocator, .{
            .symbol_index = self.cur_symbol_index,
            .data_offset = self.cur_data_offset,
            .relocation_address = relocation_address,
        });
        self.cur_symbol_index += 1;
        self.cur_data_offset += std.mem.alignForward(u32, @intCast(resource.data.len), 8);
    }
};

test writeCoff {
    var buf = std.ArrayList(u8).init(std.testing.allocator);
    defer buf.deinit();

    {
        buf.clearRetainingCapacity();

        try writeCoff(std.testing.allocator, buf.writer(), &.{}, .{});

        try std.testing.expectEqualSlices(
            u8,
            "d\x86\x02\x00\x00\x00\x00\x00t\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x01.rsrc$01\x00\x00\x00\x00\x00\x00\x00\x00\x10\x00\x00\x00d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00@\x00\x00\xc0.rsrc$02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00t\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00@\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00@feat.00\x11\x00\x00\x00\xff\xff\x00\x00\x03\x00.rsrc$01\x00\x00\x00\x00\x01\x00\x00\x00\x03\x01\x10\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00.rsrc$02\x00\x00\x00\x00\x02\x00\x00\x00\x03\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00",
            buf.items,
        );
    }

    {
        buf.clearRetainingCapacity();

        try writeCoff(std.testing.allocator, buf.writer(), &.{
            .{
                .type_value = .{ .ordinal = @intFromEnum(res.RT.RCDATA) },
                .name_value = .{ .ordinal = 1 },
                .data_version = 0,
                .memory_flags = .{ .value = 0 },
                .language = res.Language.fromInt(0x409),
                .version = 0,
                .characteristics = 0,
                .data = "foo",
            },
        }, .{});

        try std.testing.expectEqualSlices(
            u8,
            "d\x86\x02\x00\x00\x00\x00\x00\xce\x00\x00\x00\x06\x00\x00\x00\x00\x00\x00\x01.rsrc$01\x00\x00\x00\x00\x00\x00\x00\x00X\x00\x00\x00d\x00\x00\x00\xbc\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00@\x00\x00\xc0.rsrc$02\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\xc6\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00@\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\n\x00\x00\x00\x18\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x00\x000\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\t\x04\x00\x00H\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00H\x00\x00\x00\x05\x00\x00\x00\x03\x00foo\x00\x00\x00\x00\x00@feat.00\x11\x00\x00\x00\xff\xff\x00\x00\x03\x00.rsrc$01\x00\x00\x00\x00\x01\x00\x00\x00\x03\x01X\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00.rsrc$02\x00\x00\x00\x00\x02\x00\x00\x00\x03\x01\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00$R000000\x00\x00\x00\x00\x02\x00\x00\x00\x03\x00\x04\x00\x00\x00",
            buf.items,
        );
    }
}

test "res to coff" {
    {
        const res_data = "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x03\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00bar\x00\t\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00barbazfoo\x00\x00\x00\x03\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x02\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00foo\x00";
        const coff = "d\x86\x02\x00\x00\x00\x00\x00Z\x01\x00\x00\x08\x00\x00\x00\x00\x00\x00\x01.rsrc$01\x00\x00\x00\x00\x00\x00\x00\x00\xb8\x00\x00\x00d\x00\x00\x00\x1c\x01\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00@\x00\x00\xc0.rsrc$02\x00\x00\x00\x00\x00\x00\x00\x00 \x00\x00\x00:\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00@\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\n\x00\x00\x00\x18\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x01\x00\x00\x00@\x00\x00\x80\x02\x00\x00\x00X\x00\x00\x80\x03\x00\x00\x00p\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\t\x04\x00\x00\x88\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\t\x04\x00\x00\x98\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\t\x04\x00\x00\xa8\x00\x00\x00\x00\x00\x00\x00\t\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xa8\x00\x00\x00\x05\x00\x00\x00\x03\x00\x88\x00\x00\x00\x06\x00\x00\x00\x03\x00\x98\x00\x00\x00\x07\x00\x00\x00\x03\x00bar\x00\x00\x00\x00\x00barbazfoo\x00\x00\x00\x00\x00\x00\x00foo\x00\x00\x00\x00\x00@feat.00\x11\x00\x00\x00\xff\xff\x00\x00\x03\x00.rsrc$01\x00\x00\x00\x00\x01\x00\x00\x00\x03\x01\xb8\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00.rsrc$02\x00\x00\x00\x00\x02\x00\x00\x00\x03\x01 \x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00$R000000\x00\x00\x00\x00\x02\x00\x00\x00\x03\x00$R000008\x08\x00\x00\x00\x02\x00\x00\x00\x03\x00$R000018\x18\x00\x00\x00\x02\x00\x00\x00\x03\x00\x04\x00\x00\x00";
        try testResToCoff(res_data, coff);
    }
    {
        const res_data = "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x03\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00bar\x00\t\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00barbazfoo\x00\x00\x00\x03\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x02\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00foo\x00\x04\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x03\x00\x00\x00\x00\x000\x00\x01\x08\x00\x00\x00\x00\x00\x00\x00\x00lbar\n\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\x01\x08\x00\x00\x00\x00\x00\x00\x00\x00lbarbazfoo\x00\x00\x04\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x02\x00\x00\x00\x00\x000\x00\x01\x08\x00\x00\x00\x00\x00\x00\x00\x00lfoo";
        const coff = "d\x86\x02\x00\x00\x00\x00\x00\xe0\x01\x00\x00\x0b\x00\x00\x00\x00\x00\x00\x01.rsrc$01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00d\x00\x00\x00d\x01\x00\x00\x00\x00\x00\x00\x06\x00\x00\x00@\x00\x00\xc0.rsrc$02\x00\x00\x00\x00\x00\x00\x00\x00@\x00\x00\x00\xa0\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00@\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\n\x00\x00\x00\x18\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x01\x00\x00\x00@\x00\x00\x80\x02\x00\x00\x00`\x00\x00\x80\x03\x00\x00\x00\x80\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\t\x04\x00\x00\xa0\x00\x00\x00\x01\x08\x00\x00\xb0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\t\x04\x00\x00\xc0\x00\x00\x00\x01\x08\x00\x00\xd0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\t\x04\x00\x00\xe0\x00\x00\x00\x01\x08\x00\x00\xf0\x00\x00\x00\x00\x00\x00\x00\t\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\n\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xe0\x00\x00\x00\x05\x00\x00\x00\x03\x00\xa0\x00\x00\x00\x06\x00\x00\x00\x03\x00\xc0\x00\x00\x00\x07\x00\x00\x00\x03\x00\xf0\x00\x00\x00\x08\x00\x00\x00\x03\x00\xb0\x00\x00\x00\t\x00\x00\x00\x03\x00\xd0\x00\x00\x00\n\x00\x00\x00\x03\x00bar\x00\x00\x00\x00\x00barbazfoo\x00\x00\x00\x00\x00\x00\x00foo\x00\x00\x00\x00\x00lbar\x00\x00\x00\x00lbarbazfoo\x00\x00\x00\x00\x00\x00lfoo\x00\x00\x00\x00@feat.00\x11\x00\x00\x00\xff\xff\x00\x00\x03\x00.rsrc$01\x00\x00\x00\x00\x01\x00\x00\x00\x03\x01\x00\x01\x00\x00\x06\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00.rsrc$02\x00\x00\x00\x00\x02\x00\x00\x00\x03\x01@\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00$R000000\x00\x00\x00\x00\x02\x00\x00\x00\x03\x00$R000008\x08\x00\x00\x00\x02\x00\x00\x00\x03\x00$R000018\x18\x00\x00\x00\x02\x00\x00\x00\x03\x00$R000020 \x00\x00\x00\x02\x00\x00\x00\x03\x00$R000028(\x00\x00\x00\x02\x00\x00\x00\x03\x00$R0000388\x00\x00\x00\x02\x00\x00\x00\x03\x00\x04\x00\x00\x00";
        try testResToCoff(res_data, coff);
    }
    {
        const res_data = "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00 \x00\x00\x00\xff\xffc\x00\xff\xff\x03\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00zbar\n\x00\x00\x00 \x00\x00\x00\xff\xffe\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00zbarbazfoo\x00\x00\x05\x00\x00\x00 \x00\x00\x00\xff\xffd\x00\xff\xff\x02\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00zfooz\x00\x00\x00\x05\x00\x00\x00 \x00\x00\x00\xff\xffd\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00zfooz\x00\x00\x00\x03\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x03\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00bar\x00\t\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00barbazfoo\x00\x00\x00\x03\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x02\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00foo\x00\x04\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x03\x00\x00\x00\x00\x000\x00\x01\x08\x00\x00\x00\x00\x00\x00\x00\x00lbar\n\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x01\x00\x00\x00\x00\x000\x00\x01\x08\x00\x00\x00\x00\x00\x00\x00\x00lbarbazfoo\x00\x00\x04\x00\x00\x00 \x00\x00\x00\xff\xff\n\x00\xff\xff\x02\x00\x00\x00\x00\x000\x00\x01\x08\x00\x00\x00\x00\x00\x00\x00\x00lfoo";
        const coff = "d\x86\x02\x00\x00\x00\x00\x008\x03\x00\x00\x0f\x00\x00\x00\x00\x00\x00\x01.rsrc$01\x00\x00\x00\x00\x00\x00\x00\x00\x08\x02\x00\x00d\x00\x00\x00l\x02\x00\x00\x00\x00\x00\x00\n\x00\x00\x00@\x00\x00\xc0.rsrc$02\x00\x00\x00\x00\x00\x00\x00\x00h\x00\x00\x00\xd0\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00@\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\n\x00\x00\x000\x00\x00\x80c\x00\x00\x00X\x00\x00\x80d\x00\x00\x00p\x00\x00\x80e\x00\x00\x00\x90\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x01\x00\x00\x00\xa8\x00\x00\x80\x02\x00\x00\x00\xc8\x00\x00\x80\x03\x00\x00\x00\xe8\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x03\x00\x00\x00\x08\x01\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x01\x00\x00\x00 \x01\x00\x80\x02\x00\x00\x008\x01\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x01\x00\x00\x00P\x01\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\t\x04\x00\x00h\x01\x00\x00\x01\x08\x00\x00x\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\t\x04\x00\x00\x88\x01\x00\x00\x01\x08\x00\x00\x98\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\t\x04\x00\x00\xa8\x01\x00\x00\x01\x08\x00\x00\xb8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\t\x04\x00\x00\xc8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\t\x04\x00\x00\xd8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\t\x04\x00\x00\xe8\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\t\x04\x00\x00\xf8\x01\x00\x00\x00\x00\x00\x00\t\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\n\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\n\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\xc8\x01\x00\x00\x05\x00\x00\x00\x03\x00\xf8\x01\x00\x00\x06\x00\x00\x00\x03\x00\xe8\x01\x00\x00\x07\x00\x00\x00\x03\x00\xd8\x01\x00\x00\x08\x00\x00\x00\x03\x00\xa8\x01\x00\x00\t\x00\x00\x00\x03\x00h\x01\x00\x00\n\x00\x00\x00\x03\x00\x88\x01\x00\x00\x0b\x00\x00\x00\x03\x00\xb8\x01\x00\x00\x0c\x00\x00\x00\x03\x00x\x01\x00\x00\r\x00\x00\x00\x03\x00\x98\x01\x00\x00\x0e\x00\x00\x00\x03\x00zbar\x00\x00\x00\x00zbarbazfoo\x00\x00\x00\x00\x00\x00zfooz\x00\x00\x00zfooz\x00\x00\x00bar\x00\x00\x00\x00\x00barbazfoo\x00\x00\x00\x00\x00\x00\x00foo\x00\x00\x00\x00\x00lbar\x00\x00\x00\x00lbarbazfoo\x00\x00\x00\x00\x00\x00lfoo\x00\x00\x00\x00@feat.00\x11\x00\x00\x00\xff\xff\x00\x00\x03\x00.rsrc$01\x00\x00\x00\x00\x01\x00\x00\x00\x03\x01\x08\x02\x00\x00\n\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00.rsrc$02\x00\x00\x00\x00\x02\x00\x00\x00\x03\x01h\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00$R000000\x00\x00\x00\x00\x02\x00\x00\x00\x03\x00$R000008\x08\x00\x00\x00\x02\x00\x00\x00\x03\x00$R000018\x18\x00\x00\x00\x02\x00\x00\x00\x03\x00$R000020 \x00\x00\x00\x02\x00\x00\x00\x03\x00$R000028(\x00\x00\x00\x02\x00\x00\x00\x03\x00$R0000300\x00\x00\x00\x02\x00\x00\x00\x03\x00$R000040@\x00\x00\x00\x02\x00\x00\x00\x03\x00$R000048H\x00\x00\x00\x02\x00\x00\x00\x03\x00$R000050P\x00\x00\x00\x02\x00\x00\x00\x03\x00$R000060`\x00\x00\x00\x02\x00\x00\x00\x03\x00\x04\x00\x00\x00";
        try testResToCoff(res_data, coff);
    }
    {
        const res_data = "\x00\x00\x00\x00 \x00\x00\x00\xff\xff\x00\x00\xff\xff\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00(\x00\x00\x00B\x00A\x00R\x00\x00\x00F\x00O\x00O\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00a\x00b\x00c\x00d\x00e\x00f\x00\n\x00\x00\x00(\x00\x00\x00B\x00A\x00R\x00\x00\x00A\x00O\x00O\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00a\x00b\x00c\x00d\x00e\x00\x00\x00\x04\x00\x00\x00(\x00\x00\x00B\x00A\x00R\x00\x00\x00\xac O\x00O\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00abcd\x03\x00\x00\x00(\x00\x00\x00B\x00A\x00R\x00\x00\x00\xc7\x00O\x00O\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00abc\x00\x02\x00\x00\x00(\x00\x00\x00B\x00A\x00R\x00\x00\x00\xe7\x00O\x00O\x00\x00\x00\x00\x00\x00\x000\x00\t\x04\x00\x00\x00\x00\x00\x00\x00\x00ab\x00\x00";
        const coff = "d\x86\x02\x00\x00\x00\x00\x00\x16\x02\x00\x00\n\x00\x00\x00\x00\x00\x00\x01.rsrc$01\x00\x00\x00\x00\x00\x00\x00\x00H\x01\x00\x00d\x00\x00\x00\xac\x01\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00@\x00\x00\xc0.rsrc$02\x00\x00\x00\x00\x00\x00\x00\x008\x00\x00\x00\xde\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00@\x00\x00\xc0\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x00\x18\x01\x00\x80\x18\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x05\x00\x00\x00(\x01\x00\x80P\x00\x00\x80 \x01\x00\x80h\x00\x00\x808\x01\x00\x80\x80\x00\x00\x80@\x01\x00\x80\x98\x00\x00\x800\x01\x00\x80\xb0\x00\x00\x80\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\t\x04\x00\x00\xc8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\t\x04\x00\x00\xd8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\t\x04\x00\x00\xe8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\t\x04\x00\x00\xf8\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x01\x00\t\x04\x00\x00\x08\x01\x00\x00\x00\x00\x00\x00\n\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x03\x00B\x00A\x00R\x00\x03\x00F\x00O\x00O\x00\x03\x00A\x00O\x00O\x00\x03\x00\xac O\x00O\x00\x03\x00\xc7\x00O\x00O\x00\x03\x00\xe7\x00O\x00O\x00\xd8\x00\x00\x00\x05\x00\x00\x00\x03\x00\xc8\x00\x00\x00\x06\x00\x00\x00\x03\x00\x08\x01\x00\x00\x07\x00\x00\x00\x03\x00\xe8\x00\x00\x00\x08\x00\x00\x00\x03\x00\xf8\x00\x00\x00\t\x00\x00\x00\x03\x00a\x00b\x00c\x00d\x00e\x00f\x00\x00\x00\x00\x00a\x00b\x00c\x00d\x00e\x00\x00\x00\x00\x00\x00\x00abcd\x00\x00\x00\x00abc\x00\x00\x00\x00\x00ab\x00\x00\x00\x00\x00\x00@feat.00\x11\x00\x00\x00\xff\xff\x00\x00\x03\x00.rsrc$01\x00\x00\x00\x00\x01\x00\x00\x00\x03\x01H\x01\x00\x00\x05\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00.rsrc$02\x00\x00\x00\x00\x02\x00\x00\x00\x03\x018\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00$R000000\x00\x00\x00\x00\x02\x00\x00\x00\x03\x00$R000010\x10\x00\x00\x00\x02\x00\x00\x00\x03\x00$R000020 \x00\x00\x00\x02\x00\x00\x00\x03\x00$R000028(\x00\x00\x00\x02\x00\x00\x00\x03\x00$R0000300\x00\x00\x00\x02\x00\x00\x00\x03\x00\x04\x00\x00\x00";
        try testResToCoff(res_data, coff);
    }
}

fn testResToCoff(res_data: []const u8, expected_coff: []const u8) !void {
    var fbs = std.io.fixedBufferStream(res_data);

    const resources = try parseRes(std.testing.allocator, fbs.reader(), .{ .max_size = res_data.len });
    defer freeResources(std.testing.allocator, resources);

    var buf = try std.ArrayList(u8).initCapacity(std.testing.allocator, expected_coff.len);
    defer buf.deinit();

    try writeCoff(std.testing.allocator, buf.writer(), resources, .{});

    try std.testing.expectEqualSlices(u8, expected_coff, buf.items);
}
