const std = @import("std");

const ArrayListU8 = std.ArrayList(u8);
var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("wow", .{});
    };
    const list = try parse(allocator);
    defer {
        for (list.items) |i| {
            i.deinit();
        }
        list.deinit();
    }

    const ans_p1 = part1(list);
    std.debug.print("Part 1: {d}\n", .{ans_p1});

    const ans_p2 = part2(list);
    std.debug.print("Part 2: {d}\n", .{ans_p2});
}

const word = "XMAS";

pub fn parse(allocator: std.mem.Allocator) !std.ArrayList(ArrayListU8) {
    const stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    const stdin = br.reader();

    var buf: [1000]u8 = undefined;
    var list = std.ArrayList(ArrayListU8).init(allocator);
    while (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var l = ArrayListU8.init(allocator);
        try l.appendSlice(line);
        try list.append(l);
    }

    return list;
}

const needle = .{true};

pub fn part1(list: std.ArrayList(ArrayListU8)) usize {
    var count: usize = 0;
    for (list.items, 0..) |l, y| {
        for (l.items, 0..) |c, x| {
            if (c == word[0]) {
                var y_dir: isize = -1;
                while (y_dir <= 1) : (y_dir += 1) {
                    var x_dir: isize = -1;
                    while (x_dir <= 1) : (x_dir += 1) {
                        if (x_dir == 0 and y_dir == 0) {
                            continue;
                        }
                        var is_valid: bool = true;
                        for (word[1..], 1..) |tomatch, i| {
                            const x_offset: isize = x_dir * cast(isize, i);
                            const y_offset: isize = y_dir * cast(isize, i);
                            const x_pos: isize = cast(isize, x) + x_offset;
                            const y_pos: isize = cast(isize, y) + y_offset;
                            // If it invalid
                            if (!(0 <= y_pos and y_pos < list.items.len and
                                0 <= x_pos and
                                x_pos < list.items[@intCast(y_pos)].items.len and
                                tomatch ==
                                list.items[@intCast(y_pos)].items[@intCast(x_pos)]))
                            {
                                is_valid = false;
                                break;
                            }
                        }
                        if (is_valid) count += 1;
                    }
                }
            }
        }
    }

    return count;
}

const convolution_pattern = [_][]const u8{ "M S", " A ", "M S" };

pub fn part2(list: std.ArrayList(ArrayListU8)) usize {
    var count: usize = 0;
    for (list.items[1 .. list.items.len - 1], 1..) |l, y| {
        for (l.items[1 .. l.items.len - 1], 1..) |_, x| {
            for (0..4) |r| {
                var is_valid = true;

                var y_pos: isize = -1;
                while (y_pos <= 1) : (y_pos += 1) {
                    var x_pos: isize = -1;
                    while (x_pos <= 1) : (x_pos += 1) {
                        const rotated_x: isize = rotate(r, x_pos, y_pos)[0];
                        const rotated_y: isize = rotate(r, x_pos, y_pos)[1];

                        const x_index: usize = @intCast(cast(isize, x) + rotated_x);
                        const y_index: usize = @intCast(cast(isize, y) + rotated_y);

                        const conv_x_index: usize = @intCast(x_pos + 1);
                        const conv_y_index: usize = @intCast(y_pos + 1);

                        if (list.items[y_index].items[x_index] != convolution_pattern[conv_y_index][conv_x_index] and
                            convolution_pattern[conv_y_index][conv_x_index] != ' ')
                        {
                            is_valid = false;
                        }
                    }
                }

                if (is_valid) count += 1;
            }
        }
    }

    return count;
}

inline fn cast(T: type, v: anytype) T {
    return @intCast(v);
}

// Used a rotation matrix to calculate these values.
pub fn rotate(rotation: usize, x: isize, y: isize) [2]isize {
    return switch (rotation) {
        0 => .{ x, y },
        1 => .{ -y, x },
        2 => .{ -x, -y },
        3 => .{ y, -x },
        else => .{ x, y },
    };
}
