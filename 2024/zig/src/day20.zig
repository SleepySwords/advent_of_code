// Node in this case is a location and orientation.

const std = @import("std");

var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    // defer if (GPA.deinit() == std.heap.Check.leak) {
    //     std.debug.print("Wow", .{});
    // };

    const input = try parse(allocator);
    defer {
        for (input.maze.items) |o| o.deinit();
        input.maze.deinit();
    }
    print_maze(&input.maze);

    var visited = std.AutoHashMap(Location, void).init(allocator);
    defer visited.deinit();

    if (find_start(&input.maze)) |start| {
        const djiksta_map = try djiksta(&input.maze, allocator);
        const result = try bfs(&input.maze, start, &djiksta_map, allocator);
        std.debug.print("Part 1: {?}\n", .{result});
    }
}

fn with_distance(T: type) type {
    return struct {
        distance: usize,
        item: T,
    };
}

fn neighbours(grid: *const std.ArrayList(std.ArrayList(Tile)), vertex: Location, jump_wall: bool, allocator: std.mem.Allocator) !std.AutoHashMap(Location, bool) {
    var hash = std.AutoHashMap(Location, bool).init(allocator);

    for (directions) |dir| {
        const forward = vertex.move(dir);

        if (forward.x < grid.items[0].items.len and forward.y < grid.items.len) {
            if (grid.items[forward.y].items[forward.x] == .air or grid.items[forward.y].items[forward.x] == .start or grid.items[forward.y].items[forward.x] == .end) {
                try hash.put(forward, false);
            } else if (jump_wall) {
                const jumped_wall = forward.move(dir);
                if (jumped_wall.y < grid.items.len and jumped_wall.x < grid.items[jumped_wall.y].items.len) {
                    if (grid.items[jumped_wall.y].items[jumped_wall.x] == .air or grid.items[jumped_wall.y].items[jumped_wall.x] == .start or grid.items[jumped_wall.y].items[jumped_wall.x] == .end) {
                        try hash.put(jumped_wall, true);
                    }
                }
            }
        }
    }

    return hash;
}

fn find_start(grid: *const std.ArrayList(std.ArrayList(Tile))) ?Vertex {
    for (grid.items, 0..) |line, y| {
        for (line.items, 0..) |tile, x| {
            if (tile == .start) {
                return .{ .location = .{ .x = x, .y = y } };
            }
        }
    }
    return null;
}

fn find_end(grid: *const std.ArrayList(std.ArrayList(Tile))) ?Vertex {
    for (grid.items, 0..) |line, y| {
        for (line.items, 0..) |tile, x| {
            if (tile == .end) {
                return .{ .location = .{ .x = x, .y = y } };
            }
        }
    }
    return null;
}

fn shortest_path(grid: *const std.ArrayList(std.ArrayList(Tile)), start: Location, end: Location, allocator: std.mem.Allocator) !usize {
    var queue = std.ArrayList(with_distance(Location)).init(allocator);
    defer queue.deinit();

    try queue.append(.{ .item = start, .distance = 0 });

    var visited = std.AutoHashMap(Location, void).init(allocator);
    defer visited.deinit();

    while (queue.items.len != 0) {
        const item = queue.orderedRemove(0);
        if (visited.contains(item.item)) {
            continue;
        } else {
            try visited.put(item.item, {});
        }
        var item_neighbours = try neighbours(grid, item.item, false, allocator);

        var itr = item_neighbours.iterator();
        while (itr.next()) |entry| {
            const next_point = entry.key_ptr.*;
            if (next_point.eq(end)) {
                return item.distance;
            }

            try queue.append(.{ .item = entry.key_ptr.*, .distance = item.distance + 1 });
        }

        item_neighbours.deinit();
    }

    return std.math.maxInt(usize);
}

fn priority_sort(_: void, a: with_distance(Location), b: with_distance(Location)) std.math.Order {
    if (a.distance < b.distance) {
        return std.math.Order.lt;
    } else if (a.distance > b.distance) {
        return std.math.Order.gt;
    } else {
        return std.math.Order.eq;
    }
}

fn djiksta(grid: *const std.ArrayList(std.ArrayList(Tile)), allocator: std.mem.Allocator) !std.AutoHashMap(Location, usize) {
    var distance = std.AutoHashMap(Location, usize).init(allocator);

    var priority_queue = std.PriorityQueue(with_distance(Location), void, priority_sort).init(allocator, {});
    defer priority_queue.deinit();

    for (grid.items, 0..) |line, y| {
        for (line.items, 0..) |tile, x| {
            if (tile == .air or tile == .end) {
                const vertex = Location{
                    .x = x,
                    .y = y,
                };
                try distance.put(vertex, std.math.maxInt(usize));
            } else if (tile == .start) {
                const start = Location{
                    .x = x,
                    .y = y,
                };
                try distance.put(start, 0);
                try priority_queue.add(.{ .item = start, .distance = 0 });
            }
        }
    }

    while (priority_queue.count() != 0) {
        const item = priority_queue.remove().item;
        var item_neighbours = try neighbours(grid, item, false, allocator);

        var itr = item_neighbours.iterator();
        while (itr.next()) |entry| {
            const item_to_check = entry.key_ptr.*;
            const alternative = distance.get(item).? + 1;
            const current_distance = distance.get(item_to_check).?;

            if (alternative < current_distance) {
                try distance.put(item_to_check, alternative);
                try priority_queue.add(.{ .item = item_to_check, .distance = alternative });
            }
        }

        defer item_neighbours.deinit();
    }

    return distance;
}

fn bfs(grid: *const std.ArrayList(std.ArrayList(Tile)), start: Vertex, djiksta_map: *const std.AutoHashMap(Location, usize), allocator: std.mem.Allocator) !?usize {
    var queue = std.ArrayList(Location).init(allocator);
    defer queue.deinit();

    try queue.append(start.location);

    var visited = std.AutoHashMap(Location, void).init(allocator);
    defer visited.deinit();
    var count: usize = 0;

    while (queue.items.len != 0) {
        const vertex = queue.orderedRemove(0);

        if (grid.items[vertex.y].items[vertex.x] == .end) {
            continue;
        }
        if (visited.contains(vertex)) continue;
        try visited.put(vertex, {});

        var neigh = try neighbours(grid, vertex, true, allocator);
        defer neigh.deinit();

        var itr = neigh.iterator();
        while (itr.next()) |entry| {
            if (entry.value_ptr.*) {
                if (!visited.contains(entry.key_ptr.*)) {
                    const saved = djiksta_map.get(entry.key_ptr.*).? - djiksta_map.get(vertex).?;
                    if (saved >= 1 + 100) {
                        std.debug.print("res: {} {} {}\n", .{ vertex, entry.key_ptr, saved - 1 });
                        count += 1;
                    }
                }
            } else {
                try queue.append(entry.key_ptr.*);
            }
        }
    }
    return count;
}

fn get_paths(vertex: Vertex, previous: *const std.AutoHashMap(Vertex, std.ArrayList(Vertex)), best_paths: *std.AutoHashMap(Location, void)) !void {
    try best_paths.put(vertex.location, {});
    for (previous.get(vertex).?.items) |p| {
        try get_paths(p, previous, best_paths);
    }
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
        if (line.len == 0) continue;
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

const Result = struct {
    best_spots: usize,
    distance: usize,
};

const Tile = enum { wall, air, start, end };

const Input = struct { maze: std.ArrayList(std.ArrayList(Tile)) };

const directions: [4]Direction = .{ .north, .south, .east, .west };
const Direction = enum {
    north,
    south,
    east,
    west,
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

    fn eq(self: Vertex, other: Vertex) bool {
        return self.location.eq(other.location);
    }
};
