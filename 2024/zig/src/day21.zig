const std = @import("std");

var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    // defer if (GPA.deinit() == std.heap.Check.leak) {
    //     std.debug.print("Wow", .{});
    // };

    const input = try parse(allocator);
    defer {
        for (input.sequences.items) |o| o.deinit();
        input.sequences.deinit();
    }

    var robots = std.ArrayList(Robot).init(allocator);
    try robots.append(.{ .x = 2, .y = 3 });
    try robots.append(.{ .x = 2, .y = 0 });
    try robots.append(.{ .x = 2, .y = 0 });

    const state = State{ .robots = robots, .output_index = 0 };

    std.debug.print("Current state: {}\n", .{try bfs(state, allocator)});
}

fn with_direction(T: type) type {
    return struct {
        item: T,
        distance: usize,
    };
}

const wow = "0";

// Idea: moving will alway be in a straight line in two directions.
// Just have to repeat that.
//
//
// Each sequence must end with an A, the position pretty much resets...
fn bfs(initial_state: State, allocator: std.mem.Allocator) !usize {
    var queue = std.ArrayList(with_direction(State)).init(allocator);
    try queue.append(.{ .item = initial_state, .distance = 0 });

    while (queue.items.len != 0) {
        const item = queue.orderedRemove(0);
        const state = item.item;
        for (0..2) |y| {
            for (0..3) |x| {
                var test_state: State = .{ .output_index = state.output_index, .robots = try state.robots.clone() };
                if (simulate(&test_state, 3, x, y)) {
                    if (test_state.output_index == wow.len) {
                        return item.distance + 1;
                    }

                    std.debug.print("{}  ", .{queue.items.len});
                    std.debug.print("{}\n", .{item.distance + 1});

                    try queue.append(.{ .item = test_state, .distance = item.distance + 1 });
                }
            }
        }
    }

    return 0;
}

fn pad_size(level: usize) [2]usize {
    if (level == 0) {
        return .{ 3, 4 };
    } else {
        return .{ 3, 2 };
    }
}

fn simulate(state: *State, level: usize, button_x: usize, button_y: usize) bool {
    if (level != 0) {
        // Directional
        if (button_x == 0 and button_y == 0) {
            return false;
        } else if (button_x == 1 and button_y == 0) {
            state.robots.items[level - 1].y -%= 1;
            if (state.robots.items[level - 1].y < pad_size(button_x)[1]) {
                return false;
            }
        } else if (button_x == 2 and button_y == 0) {
            return simulate(state, level - 1, state.robots.items[level - 1].x, state.robots.items[level - 1].y);
        } else if (button_x == 0 and button_y == 1) {
            state.robots.items[level - 1].x -%= 1;
            if (state.robots.items[level - 1].x < pad_size(button_x)[0]) {
                return false;
            }
        } else if (button_x == 1 and button_y == 1) {
            state.robots.items[level - 1].y +%= 1;
            if (state.robots.items[level - 1].y < pad_size(button_x)[1]) {
                return false;
            }
        } else if (button_x == 2 and button_y == 1) {
            state.robots.items[level - 1].x +%= 1;
            if (state.robots.items[level - 1].x < pad_size(button_x)[0]) {
                return false;
            }
        }
    } else {
        if (button_x == 0 and button_y == 0) {
            if (wow[state.output_index] != '7') {
                return false;
            }
            state.output_index += 1;
        } else if (button_x == 1 and button_y == 0) {
            if (wow[state.output_index] != '8') {
                return false;
            }
            state.output_index += 1;
        } else if (button_x == 2 and button_y == 0) {
            if (wow[state.output_index] != '9') {
                return false;
            }
            state.output_index += 1;
        } else if (button_x == 0 and button_y == 1) {
            if (wow[state.output_index] != '4') {
                return false;
            }
            state.output_index += 1;
        } else if (button_x == 1 and button_y == 1) {
            if (wow[state.output_index] != '5') {
                return false;
            }
            state.output_index += 1;
        } else if (button_x == 2 and button_y == 1) {
            if (wow[state.output_index] != '6') {
                return false;
            }
            state.output_index += 1;
        } else if (button_x == 0 and button_y == 2) {
            if (wow[state.output_index] != '1') {
                return false;
            }
            state.output_index += 1;
        } else if (button_x == 1 and button_y == 2) {
            if (wow[state.output_index] != '2') {
                return false;
            }
            state.output_index += 1;
        } else if (button_x == 2 and button_y == 2) {
            if (wow[state.output_index] == '3') {
                return false;
            }
            state.output_index += 1;
        } else if (button_x == 0 and button_y == 3) {
            return false;
        } else if (button_x == 1 and button_y == 3) {
            if (wow[state.output_index] != '0') {
                return false;
            }
            state.output_index += 1;
        } else if (button_x == 2 and button_y == 3) {
            if (wow[state.output_index] != 'A') {
                return false;
            }
            state.output_index += 1;
        }
    }

    return true;
}

fn parse(allocator: std.mem.Allocator) !Input {
    var stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    var stdin = br.reader();

    var sequences = std.ArrayList(std.ArrayList(u8)).init(allocator);

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var line_itr = std.mem.split(u8, buf[0..size], "\n");
    while (line_itr.next()) |line| {
        if (line.len == 0) continue;
        var sequence = std.ArrayList(u8).init(allocator);
        try sequence.appendSlice(line);
        try sequences.append(sequence);
    }

    return Input{
        .sequences = sequences,
    };
}

const Input = struct {
    sequences: std.ArrayList(std.ArrayList(u8)),
};

const Robot = struct { x: usize, y: usize };

const State = struct { robots: std.ArrayList(Robot), output_index: usize };
