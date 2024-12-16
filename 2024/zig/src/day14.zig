const std = @import("std");

var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("wow", .{});
    };

    const input = try parse(allocator);
    defer input.robots.deinit();
    print_pos(&input.robots);
    std.debug.print("\n\n", .{});

    const ans_pt1 = try part1(&input);
    std.debug.print("Part 1: {}\n", .{ans_pt1});

    const ans_pt2 = try part2(&input);
    std.debug.print("Part 2: {}\n", .{ans_pt2});
}

const Pair = struct {
    x: isize,
    y: isize,
};

const Robot = struct {
    position: Pair,
    velocity: Pair,
};

const Input = struct { robots: std.ArrayList(Robot) };

const width = 101;
const height = 103;

fn part1(input: *const Input) !usize {
    var robots = try input.robots.clone();
    defer robots.deinit();
    for (0..100) |_| {
        for (robots.items) |*r| {
            simulate_robot(r);
        }
    }

    var q1: usize = 0;
    var q2: usize = 0;
    var q3: usize = 0;
    var q4: usize = 0;
    for (robots.items) |r| {
        if (r.position.x < width / 2 and r.position.y < height / 2) {
            q1 += 1;
        }
        if (r.position.x < width / 2 and r.position.y > (height / 2)) {
            q2 += 1;
        }
        if (r.position.x > (width / 2) and r.position.y < height / 2) {
            q3 += 1;
        }
        if (r.position.x > (width / 2) and r.position.y > (height / 2)) {
            q4 += 1;
        }
    }

    // std.debug.print("{} {} {} {}", .{ q1, q2, q3, q4 });

    return q1 * q2 * q3 * q4;
}

fn part2(input: *const Input) !usize {
    var robots = try input.robots.clone();
    defer robots.deinit();
    for (1..10000) |i| {
        for (robots.items) |*r| {
            simulate_robot(r);
        }
        var consecutive = false;
        for (robots.items) |r| {
            var consecutive_bit: u16 = 0;
            const one: u16 = 1;
            for (robots.items) |l| {
                if (r.position.x < l.position.x and r.position.x + 16 > l.position.x and r.position.y == l.position.y) {
                    consecutive_bit |= (one << @intCast(l.position.x - r.position.x));
                }
            }
            if (consecutive_bit == 0b1111_1111_1100_0000) {
                consecutive = true;
            }
        }
        if (consecutive) {
            std.debug.print("\x1b[2J", .{});
            std.debug.print("\x1b[H", .{});
            std.debug.print("Current: {}\n", .{i});
            print_pos(&robots);
            std.debug.print("\n\n", .{});
            std.posix.nanosleep(1, 0);
        }
    }

    return 0;
}

fn cast(T: type, v: anytype) T {
    return @intCast(v);
}

fn print_pos(robots: *const std.ArrayList(Robot)) void {
    for (0..height) |y| {
        for (0..width) |x| {
            var count: usize = 0;
            for (robots.items) |r| {
                if (r.position.x == x and r.position.y == y) {
                    count += 1;
                }
            }

            if (count == 0) {
                std.debug.print(".", .{});
            } else {
                std.debug.print("{}", .{count});
            }
        }

        std.debug.print("\n", .{});
    }
}

fn simulate_robot(robot: *Robot) void {
    robot.position.x += robot.velocity.x;
    robot.position.y += robot.velocity.y;

    while (robot.position.x < 0) {
        robot.position.x += width;
    }

    while (robot.position.y < 0) {
        robot.position.y += height;
    }

    while (robot.position.x >= width) {
        robot.position.x -= width;
    }
    while (robot.position.y >= height) {
        robot.position.y -= height;
    }
}

fn parse(allocator: std.mem.Allocator) !Input {
    const stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    var stdin = br.reader();

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var robot_itr = std.mem.split(u8, buf[0..size], "\n");

    var robots = std.ArrayList(Robot).init(allocator);

    while (robot_itr.next()) |robot| {
        if (robot.len == 0) continue;
        var list = std.mem.split(u8, robot, " ");
        const pos_str = list.next().?;
        const vel_str = list.next().?;

        var pos_itr = std.mem.split(u8, pos_str[2..], ",");

        const pos = Pair{
            .x = try std.fmt.parseInt(isize, pos_itr.next().?, 10),
            .y = try std.fmt.parseInt(isize, pos_itr.next().?, 10),
        };

        var vel_itr = std.mem.split(u8, vel_str[2..], ",");

        const vel = Pair{
            .x = try std.fmt.parseInt(isize, vel_itr.next().?, 10),
            .y = try std.fmt.parseInt(isize, vel_itr.next().?, 10),
        };

        try robots.append(Robot{ .position = pos, .velocity = vel });
    }

    return Input{ .robots = robots };
}
