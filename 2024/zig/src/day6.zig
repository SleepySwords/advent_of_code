const std = @import("std");

const HashMap = std.AutoArrayHashMap(u8, std.ArrayList(u8));
const List = std.ArrayList(u8);
var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("wow", .{});
    };
    var parsed = try parse(allocator);
    defer allocator.free(parsed.tiles);

    const ans_part1 = try part1(&parsed, allocator);
    std.debug.print("Part 1: {d}\n", .{ans_part1});

    const ans_part2 = try part2(&parsed, allocator);
    std.debug.print("Part 2: {d}\n", .{ans_part2});
}

const Tile = enum { Obstacle, Air };

const ParsedInput = struct { tiles: []Tile, width: usize, length: usize, initial_pos: GuardPosition };

const GuardDirection = enum { North, East, South, West };

const GuardPosition = struct { x: isize, y: isize };

const Guard = struct {
    position: GuardPosition,
    direction: GuardDirection,
};

pub fn part1(input: *ParsedInput, allocator: std.mem.Allocator) !usize {
    var go_to = std.AutoHashMap(GuardPosition, void).init(allocator);
    defer go_to.deinit();
    try go_to.put(input.initial_pos, {});
    try move_guard(input, input.initial_pos, .North, &go_to);

    return go_to.count();
}

pub fn part2(input: *ParsedInput, allocator: std.mem.Allocator) !usize {
    var go_to = std.AutoHashMap(GuardPosition, void).init(allocator);
    defer go_to.deinit();
    try go_to.put(input.initial_pos, {});
    try move_guard(input, input.initial_pos, .North, &go_to);

    var count: usize = 0;
    var itr = go_to.iterator();
    while (itr.next()) |value| {
        if (value.key_ptr.x != input.initial_pos.x or value.key_ptr.x != input.initial_pos.x) {
            var o = std.AutoHashMap(Guard, void).init(allocator);
            defer o.deinit();
            input.tiles[cast(usize, value.key_ptr.x) + cast(usize, value.key_ptr.y) * input.width] = .Obstacle;
            if (try check_time_loop(input, input.initial_pos, .North, &o)) count += 1;
            input.tiles[cast(usize, value.key_ptr.x) + cast(usize, value.key_ptr.y) * input.width] = .Air;
        }
    }

    return count;
}

fn move_guard(input: *ParsedInput, position: GuardPosition, direction: GuardDirection, gone_to: *std.AutoHashMap(GuardPosition, void)) !void {
    const new_position = move_direction(direction, position);
    if (new_position.x < 0 or new_position.y < 0 or new_position.x >= input.width or new_position.y >= input.length) {
        return;
    }
    const tile_to_move = input.tiles[cast(usize, new_position.x) + cast(usize, new_position.y) * input.width];
    if (tile_to_move == .Obstacle) {
        try move_guard(input, position, rotate(direction), gone_to);
    } else {
        try gone_to.put(new_position, {});
        try move_guard(input, new_position, direction, gone_to);
    }
}

fn check_time_loop(input: *ParsedInput, position: GuardPosition, direction: GuardDirection, gone_to: *std.AutoHashMap(Guard, void)) !bool {
    const new_position = move_direction(direction, position);

    if (gone_to.contains(Guard{ .position = new_position, .direction = direction })) {
        return true;
    }
    if (new_position.x < 0 or new_position.y < 0 or new_position.x >= input.width or new_position.y >= input.length) {
        return false;
    }
    const tile_to_move = input.tiles[cast(usize, new_position.x) + cast(usize, new_position.y) * input.width];
    if (tile_to_move == .Obstacle) {
        return try check_time_loop(input, position, rotate(direction), gone_to);
    } else {
        try gone_to.put(Guard{
            .position = new_position,
            .direction = direction,
        }, {});
        return try check_time_loop(input, new_position, direction, gone_to);
    }
}

fn cast(T: type, i: anytype) T {
    return @intCast(i);
}

fn rotate(direction: GuardDirection) GuardDirection {
    return switch (direction) {
        .North => .East,
        .East => .South,
        .South => .West,
        .West => .North,
    };
}

fn move_direction(direction: GuardDirection, position: GuardPosition) GuardPosition {
    switch (direction) {
        .North => return GuardPosition{
            .x = position.x,
            .y = position.y - 1,
        },
        .South => return GuardPosition{
            .x = position.x,
            .y = position.y + 1,
        },
        .East => return GuardPosition{
            .x = position.x + 1,
            .y = position.y,
        },
        .West => return GuardPosition{
            .x = position.x - 1,
            .y = position.y,
        },
    }
}

pub fn parse(allocator: std.mem.Allocator) !ParsedInput {
    const stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    const stdin = br.reader();

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var input_itr = std.mem.split(u8, buf[0..size], "\n");

    var tiles = std.ArrayList(Tile).init(allocator);
    var width: usize = undefined;
    var height: usize = 0;
    var position: GuardPosition = undefined;

    while (input_itr.next()) |line| {
        if (line.len > 0) {
            width = line.len;
            height += 1;
        }
        for (line, 0..) |ch, i| {
            if (ch == '^') {
                position = GuardPosition{
                    .x = @intCast(i),
                    .y = @intCast(height),
                };
            }
            try tiles.append(switch (ch) {
                '.' => .Air,
                '#' => .Obstacle,
                '^' => .Air,
                else => .Air,
            });
        }
    }

    return ParsedInput{ .tiles = try tiles.toOwnedSlice(), .width = width, .length = height, .initial_pos = position };
}
