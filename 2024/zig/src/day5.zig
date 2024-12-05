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
    defer {
        var itr = parsed.first.iterator();
        while (itr.next()) |entry| {
            entry.value_ptr.*.deinit();
        }
        parsed.first.deinit();
        for (parsed.second.items) |entry| {
            entry.deinit();
        }
        parsed.second.deinit();
    }

    const ans_part1 = try part1(&parsed, allocator);
    std.debug.print("Part 1: {d}\n", .{ans_part1});

    const ans_part2 = try part2(&parsed, allocator);
    std.debug.print("Part 2: {d}\n", .{ans_part2});
}

const ParsedInput = struct {
    first: HashMap,
    second: std.ArrayList(List),
};

pub fn part1(input: *ParsedInput, allocator: std.mem.Allocator) !usize {
    var total: usize = 0;

    next: for (input.second.items) |update| {
        var cannotBeAfter = std.AutoArrayHashMap(u8, u8).init(allocator);
        defer cannotBeAfter.deinit();
        for (update.items) |item| {
            if (cannotBeAfter.get(item) != null) {
                continue :next;
            }
            if (input.first.get(item)) |dependecies| {
                for (dependecies.items) |dependency| {
                    try cannotBeAfter.put(dependency, dependency);
                }
            }
        }

        total += update.items[update.items.len / 2];
    }

    return total;
}

fn sort_alg(dependency_list: HashMap, x: u8, y: u8) bool {
    if (dependency_list.get(x)) |dependencies| {
        for (dependencies.items) |o| {
            if (y == o) return false;
        }
    }
    return true;
}

pub fn part2(input: *ParsedInput, allocator: std.mem.Allocator) !usize {
    var total: usize = 0;

    next: for (input.second.items) |update| {
        var cannotBeAfter = std.AutoArrayHashMap(u8, u8).init(allocator);
        defer cannotBeAfter.deinit();
        for (update.items) |item| {
            if (cannotBeAfter.get(item) != null) {
                std.sort.pdq(u8, update.items, input.first, sort_alg);

                total += update.items[update.items.len / 2];
                continue :next;
            }
            if (input.first.get(item)) |dependecies| {
                for (dependecies.items) |dependency| {
                    try cannotBeAfter.put(dependency, dependency);
                }
            }
        }
    }
    return total;
}

pub fn parse(allocator: std.mem.Allocator) !ParsedInput {
    const stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    const stdin = br.reader();

    var buf: [10000000]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var input_itr = std.mem.split(u8, buf[0..size], "\n\n");

    const dependency_graph = input_itr.next().?;

    var hash_map = HashMap.init(allocator);
    var dependency_itr = std.mem.split(u8, dependency_graph, "\n");
    while (dependency_itr.next()) |dependency| {
        var line = std.mem.split(u8, dependency, "|");
        const tail = try std.fmt.parseInt(u8, line.next().?, 10);
        const head = try std.fmt.parseInt(u8, line.next().?, 10);
        if (hash_map.getPtr(head)) |m| {
            try m.append(tail);
        } else {
            var l = List.init(allocator);
            try l.append(tail);
            try hash_map.put(head, l);
        }
    }

    const update_list_input = input_itr.next().?;
    var update_list = std.ArrayList(List).init(allocator);
    var update_list_itr = std.mem.split(u8, update_list_input, "\n");
    while (update_list_itr.next()) |line| {
        if (line.len == 0) continue;
        var update_list_entry = List.init(allocator);
        var values = std.mem.split(u8, line, ",");
        while (values.next()) |value| {
            try update_list_entry.append(try std.fmt.parseInt(u8, value, 10));
        }
        try update_list.append(update_list_entry);
    }

    return ParsedInput{ .first = hash_map, .second = update_list };
}
