// Node in this case is a location and orientation.

const std = @import("std");

var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("Wow", .{});
    };

    const input = try parse(allocator);
    defer {
        for (input.maze.items) |o| o.deinit();
        input.maze.deinit();
    }
    print_maze(&input.maze);

    const ans_pt1 = try djiksta(&input.maze, allocator);
    std.debug.print("Part 1: {}\n", .{ans_pt1});
}

fn with_priority(T: type) type {
    return struct {
        priority: usize,
        item: T,
    };
}

fn priority_sort(_: void, a: with_priority(Vertex), b: with_priority(Vertex)) std.math.Order {
    if (a.priority < b.priority) {
        return std.math.Order.lt;
    } else if (a.priority > b.priority) {
        return std.math.Order.gt;
    } else {
        return std.math.Order.eq;
    }
}

fn neighbours(grid: *const std.ArrayList(std.ArrayList(Tile)), vertex: Vertex, allocator: std.mem.Allocator) !std.AutoHashMap(Vertex, usize) {
    var hash = std.AutoHashMap(Vertex, usize).init(allocator);
    try hash.put(.{ .direction = vertex.direction.rotate_left(), .location = vertex.location }, 1000);
    try hash.put(.{ .direction = vertex.direction.rotate_right(), .location = vertex.location }, 1000);

    const forward = vertex.location.move(vertex.direction);

    if (forward.x < grid.items[0].items.len and forward.y < grid.items.len) {
        if (grid.items[forward.y].items[forward.x] == .air or grid.items[forward.y].items[forward.x] == .start or grid.items[forward.y].items[forward.x] == .end) {
            try hash.put(.{ .direction = vertex.direction, .location = forward }, 1);
        }
    }

    return hash;
}

fn djiksta(grid: *const std.ArrayList(std.ArrayList(Tile)), allocator: std.mem.Allocator) !usize {
    var distance = std.AutoHashMap(Vertex, usize).init(allocator);
    defer distance.deinit();

    var priority_queue = std.PriorityQueue(with_priority(Vertex), void, priority_sort).init(allocator, {});
    defer priority_queue.deinit();

    for (grid.items, 0..) |line, y| {
        for (line.items, 0..) |tile, x| {
            if (tile == .air or tile == .end) {
                for (directions) |dir| {
                    const vertex = Vertex{ .location = Location{
                        .x = x,
                        .y = y,
                    }, .direction = dir };
                    try distance.put(vertex, std.math.maxInt(usize));
                    try priority_queue.add(.{ .item = vertex, .priority = std.math.maxInt(usize) });
                }
            } else if (tile == .start) {
                for (directions) |dir| {
                    const vertex = Vertex{ .location = Location{
                        .x = x,
                        .y = y,
                    }, .direction = dir };
                    try distance.put(vertex, std.math.maxInt(usize));
                    try priority_queue.add(.{ .item = vertex, .priority = std.math.maxInt(usize) });
                }
                const start = Vertex{ .location = Location{
                    .x = x,
                    .y = y,
                }, .direction = .east };
                try distance.put(start, 0);
                try priority_queue.update(.{ .item = start, .priority = std.math.maxInt(usize) }, .{ .item = start, .priority = 0 });
            }
        }
    }

    while (priority_queue.count() != 0) {
        // std.debug.print("New element!\n", .{});
        // for (priority_queue.items) |i| {
        //     if (i.priority != std.math.maxInt(usize)) {
        //         std.debug.print("{} ", .{i.priority});
        //     }
        // }
        const item = priority_queue.remove().item;
        std.debug.print("Processed element: {}", .{item});
        var item_neighbours = try neighbours(grid, item, allocator);

        var itr = item_neighbours.iterator();
        while (itr.next()) |entry| {
            const item_to_check = entry.key_ptr.*;
            const alternative = distance.get(item).? + entry.value_ptr.*;
            const current_distance = distance.get(item_to_check).?;
            if (alternative < current_distance) {
                try distance.put(entry.key_ptr.*, alternative);
                try priority_queue.update(.{ .item = item_to_check, .priority = current_distance }, .{ .item = item_to_check, .priority = alternative });
            }
        }
        std.debug.print(" => {?}\n", .{distance.get(item)});

        defer item_neighbours.deinit();
    }
    var lowest: usize = std.math.maxInt(usize);
    for (grid.items, 0..) |line, y| {
        for (line.items, 0..) |tile, x| {
            if (tile == .end) {
                for (directions) |dir| {
                    const vertex = Vertex{ .location = Location{
                        .x = x,
                        .y = y,
                    }, .direction = dir };
                    if (lowest > distance.get(vertex).?) {
                        lowest = distance.get(vertex).?;
                    }
                }
            }
        }
    }

    return lowest;
}

fn print_maze(maze: *const std.ArrayList(std.ArrayList(Tile))) void {
    for (maze.items) |line| {
        for (line.items) |tile| {
            const char: u8 = switch (tile) {
                .wall => '#',
                .air => '.',
                .start => 'S',
                .end => 'E',
            };
            std.debug.print("{c}", .{char});
        }
        std.debug.print("\n", .{});
    }
}

fn parse(allocator: std.mem.Allocator) !Input {
    var stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    var stdin = br.reader();

    var maze = std.ArrayList(std.ArrayList(Tile)).init(allocator);

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var line_itr = std.mem.split(u8, buf[0..size], "\n");
    while (line_itr.next()) |line| {
        var maze_line = std.ArrayList(Tile).init(allocator);
        for (line) |ch| {
            switch (ch) {
                '#' => try maze_line.append(.wall),
                '.' => try maze_line.append(.air),
                'S' => try maze_line.append(.start),
                'E' => try maze_line.append(.end),
                else => {},
            }
        }
        try maze.append(maze_line);
    }

    return Input{
        .maze = maze,
    };
}

const Tile = enum { wall, air, start, end };

const Input = struct { maze: std.ArrayList(std.ArrayList(Tile)) };

const directions: [4]Direction = .{ .north, .south, .east, .west };
const Direction = enum {
    north,
    south,
    east,
    west,

    fn rotate_left(self: Direction) Direction {
        return switch (self) {
            .north => .east,
            .east => .south,
            .south => .west,
            .west => .north,
        };
    }

    fn rotate_right(self: Direction) Direction {
        return switch (self) {
            .east => .north,
            .south => .east,
            .west => .south,
            .north => .west,
        };
    }
};

const Location = struct {
    x: usize,
    y: usize,

    fn eq(self: Location, other: Location) bool {
        return self.x == other.x and self.y == other.y;
    }

    fn move(self: Location, direction: Direction) Location {
        var new: Location = self;
        switch (direction) {
            .north => new.y -%= 1,
            .south => new.y +%= 1,
            .east => new.x +%= 1,
            .west => new.x -%= 1,
        }

        return new;
    }
};

const Vertex = struct {
    location: Location,
    direction: Direction,

    fn eq(self: Vertex, other: Vertex) bool {
        return self.location.eq(other.location) and self.direction == other.direction;
    }
};
