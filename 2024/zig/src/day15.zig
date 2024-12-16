const std = @import("std");

var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("wow", .{});
    };

    const input = try parse(allocator);
    defer {
        for (input.grid.items) |o| {
            o.deinit();
        }
        input.grid.deinit();
        input.directions.deinit();
    }

    const ans_pt1 = try part1(&input, allocator);
    std.debug.print("Part 1: {}\n", .{ans_pt1});

    const ans_pt2 = try part2(&input, allocator);
    std.debug.print("Part 2: {}\n", .{ans_pt2});
}

fn part1(input: *const Input, allocator: std.mem.Allocator) !usize {
    var robot = input.initial_robot;
    var grid = try clone_grid(&input.grid, allocator);
    defer {
        for (grid.items) |o| {
            o.deinit();
        }
        grid.deinit();
    }
    for (input.directions.items) |dir| {
        const new_loc = robot.move(dir);
        if (get(&grid, new_loc) == .air) {
            robot = new_loc;
        } else if (get(&grid, new_loc) == .box) {
            var find_air = new_loc;
            while (true) {
                if (get(&grid, find_air) == .wall) {
                    break;
                } else if (get(&grid, find_air) == .air) {
                    get_mut(&grid, find_air).* = .box;
                    get_mut(&grid, new_loc).* = .air;
                    robot = new_loc;

                    break;
                }
                find_air = find_air.move(dir);
            }
        }
    }
    print_grid(&grid, robot);

    var total: usize = 0;
    for (grid.items, 0..) |line, y| {
        for (line.items, 0..) |tile, x| {
            if (tile == .box) {
                total += 100 * y + x;
            }
        }
    }

    return total;
}

fn part2(input: *const Input, allocator: std.mem.Allocator) !usize {
    var robot = input.initial_robot;
    robot.x *= 2;
    var grid = try enlarge(&input.grid, allocator);
    defer {
        for (grid.items) |o| {
            o.deinit();
        }
        grid.deinit();
    }
    print_big_grid(&grid, robot);
    for (input.directions.items) |dir| {
        const new_loc = robot.move(dir);
        if (get_big(&grid, new_loc).is_box() and can_move(&grid, dir, new_loc)) {
            move(&grid, dir, new_loc);
            robot = new_loc;
        } else if (get_big(&grid, new_loc) == .air) {
            robot = new_loc;
        }
    }
    print_big_grid(&grid, robot);

    var total: usize = 0;
    for (grid.items, 0..) |line, y| {
        for (line.items, 0..) |tile, x| {
            if (tile == .left_box) {
                total += 100 * y + x;
            }
        }
    }

    return total;
}

fn clone_grid(grid: *const std.ArrayList(std.ArrayList(Tile)), allocator: std.mem.Allocator) !std.ArrayList(std.ArrayList(Tile)) {
    var cloned_grid = std.ArrayList(std.ArrayList(Tile)).init(allocator);

    for (grid.items) |o| {
        try cloned_grid.append(try o.clone());
    }

    return cloned_grid;
}

fn enlarge(grid: *const std.ArrayList(std.ArrayList(Tile)), allocator: std.mem.Allocator) !std.ArrayList(std.ArrayList(BigTile)) {
    var new_grid = std.ArrayList(std.ArrayList(BigTile)).init(allocator);
    for (grid.items) |line| {
        var l = std.ArrayList(BigTile).init(allocator);
        for (line.items) |tile| {
            switch (tile) {
                .wall => {
                    try l.append(.wall);
                    try l.append(.wall);
                },
                .air => {
                    try l.append(.air);
                    try l.append(.air);
                },
                .box => {
                    try l.append(.left_box);
                    try l.append(.right_box);
                },
            }
        }
        try new_grid.append(l);
    }

    return new_grid;
}

fn can_move(grid: *const std.ArrayList(std.ArrayList(BigTile)), direction: Direction, crate: Location) bool {
    if (direction == .north or direction == .south) {
        const other_crate = if (get_big(grid, crate) == .left_box) Location{ .x = crate.x + 1, .y = crate.y } else Location{ .x = crate.x - 1, .y = crate.y };

        if (get_big(grid, crate.move(direction)) == .wall or get_big(grid, other_crate.move(direction)) == .wall) {
            return false;
        }

        const is_crate_movable = (!get_big(grid, crate.move(direction)).is_box() or can_move(grid, direction, crate.move(direction)));

        const is_other_movable = (!get_big(grid, other_crate.move(direction)).is_box() or can_move(grid, direction, other_crate.move(direction)));

        return is_crate_movable and is_other_movable;
    } else {
        if (get_big(grid, crate.move(direction)) == .wall) {
            return false;
        }
        if (get_big(grid, crate.move(direction)) == .air) {
            return true;
        }

        return can_move(grid, direction, crate.move(direction));
    }
}

fn move(grid: *std.ArrayList(std.ArrayList(BigTile)), direction: Direction, crate: Location) void {
    const is_left_box = get_big(grid, crate) == .left_box;
    if (get_big(grid, crate.move(direction)) == .air) {
        get_big_mut(grid, crate.move(direction)).* = get_big(grid, crate);
        get_big_mut(grid, crate).* = .air;
    } else {
        move(grid, direction, crate.move(direction));
        get_big_mut(grid, crate.move(direction)).* = get_big(grid, crate);
        get_big_mut(grid, crate).* = .air;
    }

    if (direction == .north or direction == .south) {
        const other_crate = if (is_left_box) Location{ .x = crate.x + 1, .y = crate.y } else Location{ .x = crate.x - 1, .y = crate.y };

        if (get_big(grid, other_crate.move(direction)) == .air) {
            get_big_mut(grid, other_crate.move(direction)).* = get_big(grid, other_crate);
            get_big_mut(grid, other_crate).* = .air;
        } else {
            move(grid, direction, other_crate.move(direction));
            get_big_mut(grid, other_crate.move(direction)).* = get_big(grid, other_crate);
            get_big_mut(grid, other_crate).* = .air;
        }
    }
}

fn print_big_grid(grid: *const std.ArrayList(std.ArrayList(BigTile)), robot: Location) void {
    for (grid.items, 0..) |line, y| {
        for (line.items, 0..) |tile, x| {
            if (robot.x == x and robot.y == y) {
                std.debug.print("@", .{});
                continue;
            }
            switch (tile) {
                .left_box => std.debug.print("[", .{}),
                .right_box => std.debug.print("]", .{}),
                .air => std.debug.print(".", .{}),
                .wall => std.debug.print("#", .{}),
            }
        }
        std.debug.print("\n", .{});
    }
    std.debug.print("\n", .{});
}

fn print_grid(grid: *const std.ArrayList(std.ArrayList(Tile)), robot: Location) void {
    for (grid.items, 0..) |line, y| {
        for (line.items, 0..) |tile, x| {
            if (robot.x == x and robot.y == y) {
                std.debug.print("@", .{});
                continue;
            }
            switch (tile) {
                .wall => std.debug.print("#", .{}),
                .air => std.debug.print(".", .{}),
                .box => std.debug.print("o", .{}),
            }
        }
        std.debug.print("\n", .{});
    }
    std.debug.print("\n", .{});
}

fn get(grid: *const std.ArrayList(std.ArrayList(Tile)), robot: Location) Tile {
    return grid.items[robot.y].items[robot.x];
}

fn get_mut(grid: *const std.ArrayList(std.ArrayList(Tile)), robot: Location) *Tile {
    return &grid.items[robot.y].items[robot.x];
}

fn get_big(grid: *const std.ArrayList(std.ArrayList(BigTile)), robot: Location) BigTile {
    return grid.items[robot.y].items[robot.x];
}

fn get_big_mut(grid: *const std.ArrayList(std.ArrayList(BigTile)), robot: Location) *BigTile {
    return &grid.items[robot.y].items[robot.x];
}

const Tile = enum { wall, box, air };

const BigTile = enum {
    wall,
    left_box,
    right_box,
    air,
    fn is_box(self: BigTile) bool {
        return self == .left_box or self == .right_box;
    }
};

const Direction = enum { north, south, east, west };

const Location = struct {
    x: usize,
    y: usize,

    fn move(self: Location, direction: Direction) Location {
        var new: Location = self;
        switch (direction) {
            .north => new.y -= 1,
            .south => new.y += 1,
            .east => new.x += 1,
            .west => new.x -= 1,
        }

        return new;
    }
};

const Input = struct {
    grid: std.ArrayList(std.ArrayList(Tile)),
    initial_robot: Location,
    directions: std.ArrayList(Direction),
};

fn parse(allocator: std.mem.Allocator) !Input {
    const stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    const stdin = br.reader();

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var itr = std.mem.split(u8, buf[0..size], "\n\n");
    const grid_str = itr.next().?;

    var grid_itr = std.mem.split(u8, grid_str, "\n");
    var grid = std.ArrayList(std.ArrayList(Tile)).init(allocator);

    var location: Location = undefined;

    var y: usize = 0;
    while (grid_itr.next()) |line| {
        var l_arr = std.ArrayList(Tile).init(allocator);
        var x: usize = 0;
        for (line) |ch| {
            const tile: Tile = switch (ch) {
                '#' => .wall,
                'O' => .box,
                else => .air,
            };
            try l_arr.append(tile);
            if (ch == '@') {
                location = Location{ .x = x, .y = y };
            }
            x += 1;
        }
        y += 1;
        try grid.append(l_arr);
    }

    const direction_str = itr.next().?;
    var direction = std.ArrayList(Direction).init(allocator);
    for (direction_str) |d| {
        switch (d) {
            'v' => try direction.append(.south),
            '^' => try direction.append(.north),
            '>' => try direction.append(.east),
            '<' => try direction.append(.west),
            else => {},
        }
    }

    return Input{
        .grid = grid,
        .directions = direction,
        .initial_robot = location,
    };
}
