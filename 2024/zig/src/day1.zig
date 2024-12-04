const std = @import("std");

const ArrayListU32 = std.ArrayList(u32);
var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("wow", .{});
    };
    const list = try parse(allocator);
    const ans_part1 = part1(try list[0].clone(), try list[1].clone());
    std.debug.print("Part 1: {d}\n", .{ans_part1});
    const ans_part2 = part2(list[0], list[1]);
    std.debug.print("Part 2: {d}\n", .{ans_part2});
}

pub fn parse(allocator: std.mem.Allocator) ![2]ArrayListU32 {
    const stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    const stdin = br.reader();

    var buf: [1000]u8 = undefined;
    var first_list = ArrayListU32.init(allocator);
    var second_list = ArrayListU32.init(allocator);
    while (try stdin.readUntilDelimiterOrEof(&buf, '\n')) |line| {
        var iterator = std.mem.split(u8, line, "   ");
        const first_num = iterator.next().?;
        const second_num = iterator.next().?;
        try first_list.append(try std.fmt.parseInt(u32, first_num, 10));
        try second_list.append(try std.fmt.parseInt(u32, second_num, 10));
    }

    return .{ first_list, second_list };
}

pub fn part1(first_list: ArrayListU32, second_list: ArrayListU32) usize {
    std.mem.sort(u32, first_list.items, {}, std.sort.asc(u32));
    std.mem.sort(u32, second_list.items, {}, std.sort.asc(u32));

    var total: usize = 0;

    for (first_list.items, second_list.items) |fst, snd| {
        total += if (fst > snd) fst - snd else snd - fst;
    }

    first_list.deinit();
    second_list.deinit();
    return total;
}

pub fn part2(first_list: ArrayListU32, second_list: ArrayListU32) usize {
    var total: usize = 0;
    for (first_list.items) |needle| {
        const needle_array = .{needle};
        const count = std.mem.count(u32, second_list.items, &needle_array);
        total += count * needle;
    }

    first_list.deinit();
    second_list.deinit();
    return total;
}
