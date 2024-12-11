const std = @import("std");

const HashMap = std.AutoArrayHashMap(u8, std.ArrayList(u8));
const List = std.ArrayList(u8);
var GPA = std.heap.GeneralPurposeAllocator(.{}){};

const equation_size = u64;

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("wow", .{});
    };
    var parsed = try parse(allocator);
    defer {
        parsed.file_blocks.deinit();
    }

    // std.debug.print("{}\n", .{parsed.file_blocks});

    const ans_part1 = try part1(&parsed, allocator);
    std.debug.print("Part 1: {d}\n", .{ans_part1});

    const ans_part2 = try part2(&parsed, allocator);
    std.debug.print("Part 2: {d}\n", .{ans_part2});
}

const FileType = union(enum) { file: usize, free };

const FileBlock = struct { size: u8, type: FileType };

const ParsedInput = struct { file_blocks: std.ArrayList(FileBlock) };

pub fn part1(input: *ParsedInput, _: std.mem.Allocator) !usize {
    var disk_size: usize = 0;
    var last_file_id: usize = 0;
    for (input.file_blocks.items) |i| {
        disk_size += i.size;
        switch (i.type) {
            .file => |id| last_file_id = id,
            else => {},
        }
    }

    var files = try input.file_blocks.clone();
    defer files.deinit();

    var disk_ptr: usize = 0;
    var checksum: usize = 0;

    for (files.items, 0..) |*file, b| {
        // If the id we are looking at is less than the last file id, it is 0.
        switch (file.type) {
            .file => |id| if (last_file_id < id) {
                break;
            },
            else => {},
        }
        for (0..file.size) |_| {
            switch (file.type) {
                .file => |id| {
                    checksum += id * disk_ptr;
                },
                else => {
                    while (true) {
                        const last_file = &(files.items[last_file_id * 2]);
                        if (last_file.size == 0) {
                            if (last_file_id * 2 < b) {
                                break;
                            }
                            last_file_id -= 1;
                            continue;
                        } else {
                            checksum += last_file_id * disk_ptr;
                            last_file.size -= 1;
                            break;
                        }
                    }
                },
            }
            disk_ptr += 1;
        }
        // We read the entire file.
        file.size = 0;
    }

    return checksum;
}

pub fn find_file_index(file_id: usize, file_blocks: *std.ArrayList(FileBlock)) usize {
    for (file_blocks.items, 0..) |block, i| {
        switch (block.type) {
            .file => |id| if (id == file_id) {
                return i;
            },
            else => {},
        }
    }

    return 0;
}

pub fn part2(input: *ParsedInput, _: std.mem.Allocator) !usize {
    var last_file_id: usize = 0;
    for (input.file_blocks.items) |i| {
        switch (i.type) {
            .file => |id| last_file_id = id,
            else => {},
        }
    }

    next: while (last_file_id != 0) {
        const last_file_index = find_file_index(last_file_id, &input.file_blocks);
        const last_file_size = input.file_blocks.items[last_file_index].size;
        var find_block: usize = 0;
        for (input.file_blocks.items, 0..) |*block, i| {
            if (last_file_index <= i) {
                last_file_id -= 1;
                continue :next;
            }
            if (block.type == .free) {
                if (block.size >= last_file_size) {
                    find_block = i;
                    break;
                }
            }
        }

        input.file_blocks.items[find_block].size -= last_file_size;
        const removed_file = input.file_blocks.items[last_file_index];
        input.file_blocks.items[last_file_index] = FileBlock{ .size = last_file_size, .type = .free };
        try input.file_blocks.insert(find_block, removed_file);
        last_file_id -= 1;
    }

    var disk_ptr: usize = 0;
    var check_sum: usize = 0;

    for (input.file_blocks.items) |o| {
        switch (o.type) {
            .file => |id| {
                for (0..o.size) |_| {
                    // std.debug.print("{}", .{id});
                    check_sum += id * disk_ptr;
                    disk_ptr += 1;
                }
            },
            else => {
                for (0..o.size) |_| {
                    // std.debug.print(".", .{});
                }

                disk_ptr += o.size;
            },
        }
    }

    return check_sum;
}

inline fn cast(T: type, v: anytype) T {
    return @intCast(v);
}

pub fn parse(allocator: std.mem.Allocator) !ParsedInput {
    const stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    const stdin = br.reader();

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var input_itr = std.mem.split(u8, buf[0..size], "\n");

    var file_blocks = std.ArrayList(FileBlock).init(allocator);

    var file = true;
    var file_index: usize = 0;

    while (input_itr.next()) |line| {
        if (line.len <= 0) continue;
        for (line) |ch| {
            const s = ch - 48;
            const file_type = if (file) FileType{ .file = file_index } else FileType{ .free = {} };
            try file_blocks.append(FileBlock{
                .size = s,
                .type = file_type,
            });
            if (file) {
                file_index += 1;
            }
            file = !file;
        }
    }

    return ParsedInput{ .file_blocks = file_blocks };
}
