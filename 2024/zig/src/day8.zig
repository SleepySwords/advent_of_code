const std = @import("std");

const HashMap = std.AutoArrayHashMap(u8, std.ArrayList(u8));
const List = std.ArrayList(u8);
var GPA = std.heap.GeneralPurposeAllocator(.{}){};

const equation_size = u64;

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("wow", .{});
    };
    var parsed = try parse(allocator);
    defer {
        var itr = parsed.attenas.iterator();
        while (itr.next()) |o| {
            o.value_ptr.deinit();
        }
        parsed.attenas.deinit();
    }

    const ans_part1 = try part1(&parsed, allocator);
    std.debug.print("Part 1: {d}\n", .{ans_part1});

    const ans_part2 = try part2(&parsed, allocator);
    std.debug.print("Part 2: {d}\n", .{ans_part2});
}

const Location = struct {
    x: u8,
    y: u8,
};

const ParsedInput = struct { attenas: std.AutoHashMap(u8, std.ArrayList(Location)), width: usize, height: usize };

pub fn part1(input: *ParsedInput, allocator: std.mem.Allocator) !usize {
    var total = std.AutoHashMap(Location, void).init(allocator);
    defer total.deinit();
    var itr = input.attenas.iterator();
    while (itr.next()) |o| {
        for (o.value_ptr.items, 0..) |first, i| {
            for (o.value_ptr.items[(i + 1)..]) |second| {
                if (2 * first.x -% second.x < input.width and 2 * first.y -% second.y < input.height) {
                    try total.put(Location{ .x = 2 * first.x -% second.x, .y = 2 * first.y -% second.y }, {});
                }
                if (2 * second.x -% first.x < input.width and 2 * second.y -% first.y < input.height) {
                    try total.put(Location{ .x = 2 * second.x -% first.x, .y = 2 * second.y -% first.y }, {});
                }
            }
        }
    }
    return total.count();
}

pub fn part2(input: *ParsedInput, allocator: std.mem.Allocator) !usize {
    var total = std.AutoHashMap(Location, void).init(allocator);
    defer total.deinit();
    var itr = input.attenas.iterator();
    while (itr.next()) |o| {
        for (o.value_ptr.items, 0..) |first, i| {
            for (o.value_ptr.items[(i + 1)..]) |second| {
                const firstX = cast(isize, first.x);
                const firstY = cast(isize, first.y);
                const secondX = cast(isize, second.x);
                const secondY = cast(isize, second.y);
                var offsetX = firstX - secondX;
                var offsetY = firstY - secondY;
                const gcd = std.math.gcd(if (offsetX < 0)
                    cast(usize, -offsetX)
                else
                    cast(usize, offsetX), if (offsetY < 0)
                    cast(usize, -offsetY)
                else
                    cast(usize, offsetY));

                offsetX = @divExact(offsetX, cast(isize, gcd));
                offsetY = @divExact(offsetY, cast(isize, gcd));

                var x: isize = firstX;
                var y: isize = firstY;

                while (0 <= x and x < input.width and 0 <= y and y < input.height) {
                    try total.put(Location{ .x = @intCast(x), .y = @intCast(y) }, {});
                    x += offsetX;
                    y += offsetY;
                }

                y = firstY;
                x = firstX;

                while (0 <= x and x < input.width and 0 <= y and y < input.height) {
                    try total.put(Location{ .x = @intCast(x), .y = @intCast(y) }, {});
                    x -= offsetX;
                    y -= offsetY;
                }
            }
        }
    }
    return total.count();
}

inline fn cast(T: type, v: anytype) T {
    return @intCast(v);
}

pub fn parse(allocator: std.mem.Allocator) !ParsedInput {
    const stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    const stdin = br.reader();

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var input_itr = std.mem.split(u8, buf[0..size], "\n");

    var attenas = std.AutoHashMap(u8, std.ArrayList(Location)).init(allocator);
    var width: usize = 0;
    var height: usize = 0;

    while (input_itr.next()) |line| {
        if (line.len <= 0) continue;
        width = 0;
        for (line) |ch| {
            if (ch != '.') {
                const attenaLocation = Location{
                    .x = @intCast(width),
                    .y = @intCast(height),
                };

                const entry = try attenas.getOrPutValue(ch, std.ArrayList(Location).init(allocator));
                try entry.value_ptr.*.append(attenaLocation);
            }
            width += 1;
        }
        height += 1;
    }

    return ParsedInput{ .attenas = attenas, .width = width, .height = height };
}
