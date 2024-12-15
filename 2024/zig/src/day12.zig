const std = @import("std");

var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("wow", .{});
    };

    var input = try parse(allocator);
    defer {
        for (input.farm.items) |o| {
            o.deinit();
        }
        input.farm.deinit();
    }

    // std.debug.print("{}", .{input.farm});

    const ans_pt1 = try part1(&input, allocator);
    std.debug.print("Part 1: {}\n", .{ans_pt1});

    const ans_pt2 = try part2(&input, allocator);
    std.debug.print("Part 2: {}\n", .{ans_pt2});
}

const Location = struct {
    x: usize,
    y: usize,

    fn inbounds(location: *const Location, farm: *const std.ArrayList(std.ArrayList(u8))) bool {
        return location.x < farm.items[0].items.len and location.y < farm.items.len;
    }
};

// fn get(self: *const std.ArrayList(std.ArrayList(u8)), location: Location) {
//
// }

const OuterPerimeter = struct {
    location: Location,
    direction: Direction,
};

fn part1(input: *Input, allocator: std.mem.Allocator) !usize {
    var total: usize = 0;
    var visited = std.AutoHashMap(Location, void).init(allocator);
    defer visited.deinit();

    for (input.farm.items, 0..) |l, y| {
        for (l.items, 0..) |c, x| {
            const current_location = Location{ .x = x, .y = y };
            if (visited.contains(current_location)) continue;
            try visited.put(current_location, {});
            const result = try floodfill(current_location, c, &input.farm, &visited);
            std.debug.print("{} {} {} {}\n", .{ c, x, y, result });
            total += result.area * result.perimeter;
        }
    }

    return total;
}

fn part2(input: *Input, allocator: std.mem.Allocator) !usize {
    var total: usize = 0;

    var visited = std.AutoHashMap(Location, void).init(allocator);
    defer visited.deinit();
    for (input.farm.items, 0..) |l, y| {
        for (l.items, 0..) |c, x| {
            const current_location = Location{ .x = x, .y = y };
            if (visited.contains(current_location)) continue;
            var found_outer_perimeter = std.AutoHashMap(OuterPerimeter, void).init(allocator);
            defer found_outer_perimeter.deinit();

            try visited.put(current_location, {});

            const result = try floodfill_bfs(current_location, c, &input.farm, &visited, &found_outer_perimeter, allocator);
            std.debug.print("{c} {} {} {}\n", .{ c, x, y, result });
            total += result.area * result.perimeter;
        }
    }

    return total;
}

const ResultFloodFill = struct { area: usize, perimeter: usize };

fn floodfill(current: Location, farm_id: u8, farm: *const std.ArrayList(std.ArrayList(u8)), visited: *std.AutoHashMap(Location, void)) !ResultFloodFill {
    var perimeter: usize = 0;
    var area: usize = 1;

    for (all_directions) |dir| {
        if (dir.move(current).inbounds(farm)) {
            const location = dir.move(current);
            if (farm.items[location.y].items[location.x] == farm_id and !visited.contains(location)) {
                try visited.put(location, {});
                const result = try floodfill(location, farm_id, farm, visited);

                perimeter += result.perimeter;
                area += result.area;
            } else if (farm.items[location.y].items[location.x] != farm_id) {
                perimeter += 1;
            }
        } else {
            perimeter += 1;
        }
    }

    return ResultFloodFill{
        .area = area,
        .perimeter = perimeter,
    };
}

fn floodfill_bfs(init: Location, farm_id: u8, farm: *const std.ArrayList(std.ArrayList(u8)), visited: *std.AutoHashMap(Location, void), found_outer_perimeter: *std.AutoHashMap(OuterPerimeter, void), allocator: std.mem.Allocator) !ResultFloodFill {
    var perimeter: usize = 0;
    var area: usize = 0;

    var queue = std.ArrayList(Location).init(allocator);
    defer queue.deinit();
    try queue.append(init);

    while (queue.items.len != 0) {
        const current = queue.pop();
        area += 1;

        for (all_directions) |dir| {
            const location = dir.move(current);
            if (location.inbounds(farm)) {
                if (farm.items[location.y].items[location.x] == farm_id and !visited.contains(location)) {
                    try visited.put(location, {});
                    // std.debug.print("Adding to workflow: {}", .{location});
                    try queue.append(location);
                } else if (farm.items[location.y].items[location.x] != farm_id) {
                    var add = true;
                    for (all_directions) |d| {
                        const new_loc = d.move(location);
                        if (found_outer_perimeter.contains(OuterPerimeter{ .location = new_loc, .direction = dir })) {
                            add = false;
                        }
                    }
                    if (add) {
                        std.debug.print("1 - {} {} {c} {c}\n", .{ location, dir, farm_id, farm.items[location.y].items[location.x] });
                        perimeter += 1;
                    }
                    try found_outer_perimeter.put(OuterPerimeter{ .location = location, .direction = dir }, {});
                    for (dir.neighbours()) |move_to| {
                        var check_loc = current;
                        while (check_loc.inbounds(farm) and farm.items[check_loc.y].items[check_loc.x] == farm_id) {
                            const potential_connected = dir.move(check_loc);
                            if (potential_connected.inbounds(farm) and farm.items[potential_connected.y].items[potential_connected.x] == farm_id) {
                                break;
                            }
                            try found_outer_perimeter.put(OuterPerimeter{ .location = potential_connected, .direction = dir }, {});
                            check_loc = move_to.move(check_loc);
                        }
                    }
                }
            } else {
                var add = true;
                for (all_directions) |d| {
                    const new_loc = d.move(location);
                    if (found_outer_perimeter.contains(OuterPerimeter{ .location = new_loc, .direction = dir })) {
                        add = false;
                    }
                }
                if (add) {
                    // var ite = found_outer_perimeter.iterator();
                    // while (ite.next()) |a| {
                    //     std.debug.print("{}\n", .{a.key_ptr});
                    // }
                    std.debug.print("2 - {} {}\n", .{ location, dir });
                    perimeter += 1;
                }
                try found_outer_perimeter.put(OuterPerimeter{ .location = location, .direction = dir }, {});
                for (dir.neighbours()) |move_to| {
                    var check_loc = current;
                    while (check_loc.inbounds(farm) and farm.items[check_loc.y].items[check_loc.x] == farm_id) {
                        const potential_connected = dir.move(check_loc);
                        if (potential_connected.inbounds(farm) and farm.items[potential_connected.y].items[potential_connected.x] == farm_id) {
                            break;
                        }
                        try found_outer_perimeter.put(OuterPerimeter{ .location = potential_connected, .direction = dir }, {});
                        check_loc = move_to.move(check_loc);
                    }
                }
            }
        }
    }

    return ResultFloodFill{
        .area = area,
        .perimeter = perimeter,
    };
}

const all_directions: [4]Direction = .{ Direction.north, Direction.south, Direction.east, Direction.west };

const Direction = enum {
    north,
    south,
    east,
    west,
    fn move(self: Direction, location: Location) Location {
        return switch (self) {
            .north => Location{ .x = location.x, .y = location.y -% 1 },
            .south => Location{ .x = location.x, .y = location.y +% 1 },
            .east => Location{ .x = location.x +% 1, .y = location.y },
            .west => Location{ .x = location.x -% 1, .y = location.y },
        };
    }
    fn reverse(self: Direction) Direction {
        return switch (self) {
            .north => .south,
            .south => .north,
            .east => .west,
            .west => .east,
        };
    }
    fn neighbours(self: Direction) [2]Direction {
        return switch (self) {
            .north => .{ .east, .west },
            .south => .{ .east, .west },
            .east => .{ .north, .south },
            .west => .{ .north, .south },
        };
    }
};

const Input = struct {
    farm: std.ArrayList(std.ArrayList(u8)),
};

fn parse(allocator: std.mem.Allocator) !Input {
    const stdinFile = std.io.getStdIn();
    var br = std.io.bufferedReader(stdinFile.reader());
    const stdin = br.reader();

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var line_itr = std.mem.split(u8, buf[0..size], "\n");

    var grid = std.ArrayList(std.ArrayList(u8)).init(allocator);

    while (line_itr.next()) |line| {
        if (line.len == 0) continue;
        var l = std.ArrayList(u8).init(allocator);
        for (line) |ch| {
            try l.append(ch);
        }
        try grid.append(l);
    }

    return Input{ .farm = grid };
}
