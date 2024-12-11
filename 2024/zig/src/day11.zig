const std = @import("std");

var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("wow", .{});
    };

    var input = try parse(allocator);
    defer {
        input.stones.deinit();
    }

    std.debug.print("{}", .{input.stones});

    const ans_pt1 = try part1(&input, allocator);
    std.debug.print("Part 1: {}\n", .{ans_pt1});

    const ans_pt2 = try part2(&input, allocator);
    std.debug.print("Part 2: {}\n", .{ans_pt2});
}

const State = struct {
    current_num: usize,
    blinks_left: usize,
};

fn part1(input: *Input, allocator: std.mem.Allocator) !usize {
    var total: usize = 0;

    var cache = std.AutoHashMap(State, usize).init(allocator);
    for (input.stones.items) |stone| {
        total += try run_rule(&cache, stone, 25);
    }

    return total;
}

fn part2(input: *Input, allocator: std.mem.Allocator) !usize {
    var total: usize = 0;

    var cache = std.AutoHashMap(State, usize).init(allocator);
    for (input.stones.items) |stone| {
        total += try run_rule(&cache, stone, 75);
    }

    return total;
}

fn run_rule(dp: *std.AutoHashMap(State, usize), current_num: usize, blinks_left: usize) !usize {
    if (blinks_left == 0) {
        return 1;
    }
    if (dp.get(State{ .current_num = current_num, .blinks_left = blinks_left })) |cached| {
        return cached;
    }
    var total: usize = 0;
    if (current_num == 0) {
        total = try run_rule(dp, 1, blinks_left - 1);
    } else if (std.math.log10(current_num) % 2 == 1) {
        const base = std.math.pow(usize, 10, (std.math.log10(current_num) / 2) + 1);
        total = try run_rule(dp, current_num / base, blinks_left - 1) + try run_rule(dp, current_num % base, blinks_left - 1);
    } else {
        total = try run_rule(dp, current_num * 2024, blinks_left - 1);
    }

    try dp.put(State{ .current_num = current_num, .blinks_left = blinks_left }, total);
    return total;
}

const Input = struct {
    stones: std.ArrayList(usize),
};

fn parse(allocator: std.mem.Allocator) !Input {
    const stdinFile = std.io.getStdIn();
    var br = std.io.bufferedReader(stdinFile.reader());
    const stdin = br.reader();

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var line_itr = std.mem.split(u8, buf[0..size], "\n");

    var grid = std.ArrayList(usize).init(allocator);

    while (line_itr.next()) |line| {
        if (line.len == 0) continue;
        var numbers = std.mem.split(u8, line, " ");
        while (numbers.next()) |number| {
            const num = try std.fmt.parseInt(usize, number, 10);
            try grid.append(num);
        }
    }

    return Input{ .stones = grid };
}
