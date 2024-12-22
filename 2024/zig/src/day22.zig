const std = @import("std");

var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    // defer if (GPA.deinit() == std.heap.Check.leak) {
    //     std.debug.print("Wow", .{});
    // };

    const input = try parse(allocator);
    defer {
        input.initial_numbers.deinit();
    }

    var cache = std.ArrayList(std.AutoHashMap(usize, u8)).init(allocator);
    var t = std.AutoHashMap(usize, usize).init(allocator);

    for (input.initial_numbers.items, 0..) |*num, n| {
        var previous: [5]usize = .{ 0, 0, 0, 0, num.* };
        try cache.append(std.AutoHashMap(usize, u8).init(allocator));
        for (0..2000) |i| {
            num.* = next_number(num.*);
            for (0..4) |k| {
                previous[k] = previous[k + 1];
            }
            previous[4] = num.*;
            if (i >= 3) {
                const first: isize = @rem(cast(isize, previous[0]), 10);
                const second: isize = @rem(cast(isize, previous[1]), 10);
                const third: isize = @rem(cast(isize, previous[2]), 10);
                const fourth: isize = @rem(cast(isize, previous[3]), 10);
                const fifth: isize = @rem(cast(isize, previous[4]), 10);

                const diff1 = second - first;
                const diff2 = third - second;
                const diff3 = fourth - third;
                const diff4 = fifth - fourth;

                const hash = (((diff1 + 9) * 19 + (diff2 + 9)) * 19 + (diff3 + 9)) * 19 + (diff4 + 9);
                if (!cache.items[n].contains(@intCast(hash))) {
                    try cache.items[n].put(@intCast(hash), @intCast(num.* % 10));
                    const result = try t.getOrPutValue(@intCast(hash), 0);
                    result.value_ptr.* += num.* % 10;
                }
            }
        }
    }

    std.debug.print("{?}\n", .{cache.items[0].get(51787)});

    var most: usize = 0;
    for (0..19 * 19 * 19 * 19) |hash| {
        if (t.get(hash)) |total| {
            if (total > most) {
                most = total;
                std.debug.print("{} {} \n", .{ most, hash });
            }
        }
    }

    var total: usize = 0;
    for (input.initial_numbers.items) |num| {
        total += num;
    }
    std.debug.print("Part 1: {}\n", .{total});
    std.debug.print("Part 2: {}\n", .{most});
}

inline fn cast(T: type, v: anytype) T {
    return @intCast(v);
}

fn next_number(secret_number: usize) usize {
    var new_secret = secret_number;

    var value = new_secret << 6;
    new_secret ^= value;
    new_secret &= 0b0_1111_1111_1111_1111_1111_1111;

    value = new_secret >> 5;
    new_secret ^= value;
    new_secret &= 0b0_1111_1111_1111_1111_1111_1111;

    value = new_secret << 11;
    new_secret ^= value;
    new_secret &= 0b0_1111_1111_1111_1111_1111_1111;

    return new_secret;
}

fn parse(allocator: std.mem.Allocator) !Input {
    var stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    var stdin = br.reader();

    var sequences = std.ArrayList(usize).init(allocator);

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var line_itr = std.mem.split(u8, buf[0..size], "\n");
    while (line_itr.next()) |line| {
        if (line.len == 0) continue;
        try sequences.append(try std.fmt.parseInt(usize, line, 10));
    }

    return Input{
        .initial_numbers = sequences,
    };
}

const Input = struct {
    initial_numbers: std.ArrayList(usize),
};
