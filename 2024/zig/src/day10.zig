const std = @import("std");

var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("wow", .{});
    };

    var input = try parse(allocator);
    defer {
        for (input.grid.items) |o| {
            o.deinit();
        }
        input.grid.deinit();
    }

    const ans_pt1 = try part1(&input, allocator);
    std.debug.print("Part 1: {}\n", .{ans_pt1});

    const ans_pt2 = part2(&input, allocator);
    std.debug.print("Part 2: {}\n", .{ans_pt2});
}

const Position = struct {
    x: usize,
    y: usize,
};

fn dfs(input: *Input, x: usize, y: usize, position: u8, found: *std.AutoHashMap(Position, void)) !void {
    if (position == 9) {
        try found.put(Position{ .x = x, .y = y }, {});
    }
    if (x +% 1 < input.width) {
        if (input.grid.items[y].items[x + 1] == position + 1) {
            try dfs(input, x + 1, y, position + 1, found);
        }
    }
    if (y +% 1 < input.height) {
        if (input.grid.items[y + 1].items[x] == position + 1) {
            try dfs(input, x, y + 1, position + 1, found);
        }
    }
    if (x -% 1 < input.width) {
        if (input.grid.items[y].items[x - 1] == position + 1) {
            try dfs(input, x - 1, y, position + 1, found);
        }
    }
    if (y -% 1 < input.height) {
        if (input.grid.items[y - 1].items[x] == position + 1) {
            try dfs(input, x, y - 1, position + 1, found);
        }
    }
}

fn dfs_rating(input: *Input, x: usize, y: usize, position: u8) usize {
    if (position == 9) {
        return 1;
    }
    var total: usize = 0;
    if (x +% 1 < input.width) {
        if (input.grid.items[y].items[x + 1] == position + 1) {
            total += dfs_rating(input, x + 1, y, position + 1);
        }
    }
    if (y +% 1 < input.height) {
        if (input.grid.items[y + 1].items[x] == position + 1) {
            total += dfs_rating(input, x, y + 1, position + 1);
        }
    }
    if (x -% 1 < input.width) {
        if (input.grid.items[y].items[x - 1] == position + 1) {
            total += dfs_rating(input, x - 1, y, position + 1);
        }
    }
    if (y -% 1 < input.height) {
        if (input.grid.items[y - 1].items[x] == position + 1) {
            total += dfs_rating(input, x, y - 1, position + 1);
        }
    }

    return total;
}

fn part1(input: *Input, allocator: std.mem.Allocator) !usize {
    var total: usize = 0;
    for (input.grid.items, 0..) |line, y| {
        for (line.items, 0..) |n, x| {
            if (n == 0) {
                var found = std.AutoHashMap(Position, void).init(allocator);
                defer found.deinit();
                try dfs(input, x, y, 0, &found);
                total += found.count();
            }
        }
    }

    return total;
}

fn part2(input: *Input, _: std.mem.Allocator) usize {
    var total: usize = 0;
    for (input.grid.items, 0..) |line, y| {
        for (line.items, 0..) |n, x| {
            if (n == 0) {
                total += dfs_rating(input, x, y, 0);
            }
        }
    }

    return total;
}

const Input = struct {
    grid: std.ArrayList(std.ArrayList(u8)),
    width: usize,
    height: usize,
};

fn parse(allocator: std.mem.Allocator) !Input {
    const stdinFile = std.io.getStdIn();
    var br = std.io.bufferedReader(stdinFile.reader());
    const stdin = br.reader();

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var line_itr = std.mem.split(u8, buf[0..size], "\n");

    var grid = std.ArrayList(std.ArrayList(u8)).init(allocator);
    var width: usize = 0;
    var height: usize = 0;

    while (line_itr.next()) |line| {
        if (line.len == 0) continue;
        height += 1;
        var grid_line = std.ArrayList(u8).init(allocator);
        for (line, 0..) |ch, i| {
            try grid_line.append(ch - 48);
            width = i + 1;
        }

        try grid.append(grid_line);
    }

    return Input{ .grid = grid, .width = width, .height = height };
}
