const std = @import("std");

var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("Mmeory leak detected", .{});
    };
    var input = try parse(allocator);
    defer {
        var itr = input.patterns.valueIterator();
        while (itr.next()) |o| {
            for (o.items) |f| f.deinit();
            o.deinit();
        }
        input.patterns.deinit();

        for (input.examples.items) |e| e.deinit();
        input.examples.deinit();
    }

    const ans_part1 = try part1(&input, allocator);
    std.debug.print("Part 1: {}\n", .{ans_part1});

    const ans_part2 = try part2(&input, allocator);
    std.debug.print("Part 2: {}\n", .{ans_part2});
}

fn part1(input: *Input, allocator: std.mem.Allocator) !usize {
    var count: usize = 0;
    for (input.examples.items) |example| {
        const ok = try bfs(example.items, &input.patterns, allocator);
        if (ok) count += 1;
    }
    return count;
}

fn part2(input: *Input, allocator: std.mem.Allocator) !usize {
    var memorised = std.StringHashMap(usize).init(allocator);
    defer memorised.deinit();
    var total: usize = 0;
    for (input.examples.items) |example| {
        const ok = try dfs(example.items, &input.patterns, allocator, &memorised);
        total += ok;
    }
    return total;
}

fn bfs(initial: []u8, all_patterns: *std.AutoHashMap(u8, std.ArrayList(String)), allocator: std.mem.Allocator) !bool {
    var queue = std.ArrayList([]u8).init(allocator);
    defer queue.deinit();

    var visited = std.StringHashMap(void).init(allocator);
    defer visited.deinit();

    try queue.append(initial);

    while (queue.items.len != 0) {
        const str = queue.pop();
        if (str.len == 0) return true;
        std.debug.print("{s}\n", .{str});

        if (all_patterns.get(str[0])) |patterns| {
            pattern_loop: for (patterns.items) |pattern| {
                if (pattern.items.len > str.len) {
                    continue :pattern_loop;
                }
                for (pattern.items, 0..) |ch, i| {
                    if (ch != str[i]) {
                        continue :pattern_loop;
                    }
                }
                if (!visited.contains(str[pattern.items.len..])) {
                    try visited.put(str[pattern.items.len..], {});
                    try queue.append(str[pattern.items.len..]);
                }
            }
        } else {
            continue;
        }
    }

    return false;
}

fn dfs(str: []u8, all_patterns: *std.AutoHashMap(u8, std.ArrayList(String)), allocator: std.mem.Allocator, memorised: *std.StringHashMap(usize)) !usize {
    if (str.len == 0) {
        return 1;
    }
    var total: usize = 0;

    if (all_patterns.get(str[0])) |patterns| {
        pattern_loop: for (patterns.items) |pattern| {
            if (pattern.items.len > str.len) {
                continue :pattern_loop;
            }
            for (pattern.items, 0..) |ch, i| {
                if (ch != str[i]) {
                    continue :pattern_loop;
                }
            }
            if (memorised.get(str[pattern.items.len..])) |re| {
                total += re;
            } else {
                const res = try dfs(str[pattern.items.len..], all_patterns, allocator, memorised);
                total += res;
                try memorised.put(str[pattern.items.len..], res);
            }
        }
    }

    return total;
}

const String = std.ArrayList(u8);

const Input = struct {
    patterns: std.AutoHashMap(u8, std.ArrayList(String)),
    examples: std.ArrayList(String),
};

const State = struct {
    input: String,
};

fn parse(allocator: std.mem.Allocator) !Input {
    const stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    var stdin = br.reader();

    var buf: [65_536]u8 = undefined;

    const size = try stdin.readAll(&buf);
    var line_itr = std.mem.split(u8, buf[0..size], "\n\n");
    const patterns_str = line_itr.next().?;

    var patterns = std.AutoHashMap(u8, std.ArrayList(String)).init(allocator);

    var patterns_itr = std.mem.split(u8, patterns_str, ", ");

    while (patterns_itr.next()) |pattern| {
        if (pattern.len == 0) continue;
        var pattern_arr = std.ArrayList(u8).init(allocator);
        for (pattern) |k| {
            try pattern_arr.append(k);
        }
        var p = try patterns.getOrPutValue(pattern[0], std.ArrayList(String).init(allocator));
        try p.value_ptr.append(pattern_arr);
    }

    var examples = std.ArrayList(std.ArrayList(u8)).init(allocator);
    const examples_str = line_itr.next().?;
    var examples_itr = std.mem.split(u8, examples_str, "\n");

    while (examples_itr.next()) |ex| {
        if (ex.len == 0) continue;
        var example = std.ArrayList(u8).init(allocator);
        try example.appendSlice(ex);
        try examples.append(example);
    }

    return Input{ .patterns = patterns, .examples = examples };
}
