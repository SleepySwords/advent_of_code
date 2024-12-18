const std = @import("std");

const GRID_SIZE = 71;
var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("Mmeory leak detected", .{});
    };
    var input = try parse(allocator);
    defer input.bytes.deinit();

    std.debug.print("{}\n", .{input});
    try part1(&input, allocator);
    try binary_search(&input, allocator);
}

fn part1(input: *Input, allocator: std.mem.Allocator) !void {
    var bytes = std.AutoHashMap(Location, void).init(allocator);
    defer bytes.deinit();
    for (0..1024) |i| {
        try bytes.put(input.bytes.items[i], {});
    }
    print_map(&bytes);
    const b = try bfs(.{ .x = 0, .y = 0 }, &bytes, allocator);

    std.debug.print("Part 1: {?}\n", .{b});
}

fn binary_search(input: *Input, allocator: std.mem.Allocator) !void {
    var lowest: usize = 0;
    var highest: usize = input.bytes.items.len - 1;
    var bytes = std.AutoHashMap(Location, void).init(allocator);
    defer bytes.deinit();
    while (lowest != highest) {
        bytes.clearRetainingCapacity();
        const mid: usize = try std.math.divCeil(usize, (lowest + highest), 2);
        for (0..(mid + 1)) |i| {
            try bytes.put(input.bytes.items[i], {});
        }
        const b = try bfs(.{ .x = 0, .y = 0 }, &bytes, allocator);
        if (b == null) {
            highest = mid - 1;
        } else {
            lowest = mid;
        }
    }

    const found = input.bytes.items[lowest + 1];
    std.debug.print("Part 2: {},{}", .{ found.x, found.y });
}

fn part2(input: *Input, allocator: std.mem.Allocator) !void {
    var bytes = std.AutoHashMap(Location, void).init(allocator);
    defer bytes.deinit();
    for (input.bytes.items) |i| {
        try bytes.put(i, {});
        const b = try bfs(.{ .x = 0, .y = 0 }, &bytes, allocator);
        if (b == null) {
            std.debug.print("Part 2: {},{}\n", .{ i.x, i.y });
            break;
        }
    }
    print_map(&bytes);
}

fn print_map(bytes: *std.AutoHashMap(Location, void)) void {
    for (0..GRID_SIZE) |y| {
        for (0..GRID_SIZE) |x| {
            if (bytes.contains(.{ .x = x, .y = y })) {
                std.debug.print("#", .{});
            } else {
                std.debug.print(".", .{});
            }
        }
        std.debug.print("\n", .{});
    }
}

fn bfs(current: Location, bytes: *std.AutoHashMap(Location, void), allocator: std.mem.Allocator) !?usize {
    var queue = std.ArrayList(State).init(allocator);
    defer queue.deinit();
    var visited = std.AutoHashMap(Location, void).init(allocator);
    defer visited.deinit();
    try queue.append(.{
        .location = current,
        .count = 0,
    });

    while (queue.items.len != 0) {
        const state = queue.orderedRemove(0);

        for (directions) |dir| {
            const new_loc = state.location.move(dir);
            if (new_loc.x < GRID_SIZE and new_loc.y < GRID_SIZE) {
                if (new_loc.x == GRID_SIZE - 1 and new_loc.y == GRID_SIZE - 1) {
                    return state.count + 1;
                }
                if (!bytes.contains(new_loc) and !visited.contains(new_loc)) {
                    try queue.append(.{ .location = new_loc, .count = state.count + 1 });
                }
                try visited.put(new_loc, {});
            }
        }
    }

    return null;
}

fn parse(allocator: std.mem.Allocator) !Input {
    const stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    var stdin = br.reader();

    var buf: [65_536]u8 = undefined;

    const size = try stdin.readAll(&buf);
    var line_itr = std.mem.split(u8, buf[0..size], "\n");

    var bytes = std.ArrayList(Location).init(allocator);

    while (line_itr.next()) |line| {
        if (line.len == 0) continue;
        var coord = std.mem.split(u8, line, ",");
        const x = try std.fmt.parseInt(usize, coord.next().?, 10);
        const y = try std.fmt.parseInt(usize, coord.next().?, 10);
        try bytes.append(Location{
            .x = x,
            .y = y,
        });
    }

    return Input{ .bytes = bytes };
}

const State = struct {
    location: Location,
    count: usize,
};

const directions: [4]Direction = .{ .north, .south, .east, .west };

const Location = struct {
    x: usize,
    y: usize,

    fn move(self: Location, direction: Direction) Location {
        var new_loc = self;
        switch (direction) {
            .north => new_loc.y -%= 1,
            .south => new_loc.y +%= 1,
            .east => new_loc.x +%= 1,
            .west => new_loc.x -%= 1,
        }
        return new_loc;
    }
};

const Direction = enum { north, east, south, west };

const Input = struct {
    bytes: std.ArrayList(Location),
};
