const std = @import("std");

const ArrayListU32 = std.ArrayList(u32);
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

    const ans_p2 = try part2(list);
    std.debug.print("Part 2: {d}\n", .{ans_p2});
}

pub fn parse(allocator: std.mem.Allocator) !std.ArrayList(ArrayListU32) {
    const stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    const stdin = br.reader();

    var buf: [1000]u8 = undefined;
    var list = std.ArrayList(ArrayListU32).init(allocator);
    while (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var u32_list = ArrayListU32.init(allocator);
        var iterator = std.mem.splitAny(u8, line, " ");
        while (iterator.next()) |o| {
            try u32_list.append(try std.fmt.parseInt(u32, o, 10));
        }

        try list.append(u32_list);
    }

    return list;
}

pub fn is_valid(list: *const ArrayListU32) bool {
    const increasing = list.items[0] < list.items[1];
    var window_iterator = std.mem.window(u32, list.items, 2, 1);
    var valid = true;

    while (window_iterator.next()) |window| {
        const window_increasing = window[0] < window[1];
        const distance = if (window_increasing) window[1] - window[0] else window[0] - window[1];

        if (window_increasing != increasing or 1 > distance or distance > 3) {
            valid = false;
            break;
        }
    }

    return valid;
}

pub fn part1(list: std.ArrayList(ArrayListU32)) usize {
    var total: usize = 0;
    for (list.items) |line| {
        if (is_valid(&line)) total += 1;
    }
    return total;
}

pub fn part2(list: std.ArrayList(ArrayListU32)) error{OutOfMemory}!usize {
    var total: usize = 0;
    for (list.items) |*line| {
        if (!is_valid(line)) {
            for (0..line.*.items.len) |i| {
                const removed = line.orderedRemove(i);
                if (is_valid(line)) {
                    total += 1;
                    try line.insert(i, removed);
                    break;
                }
                try line.insert(i, removed);
            }
        } else {
            total += 1;
        }
    }
    return total;
}
