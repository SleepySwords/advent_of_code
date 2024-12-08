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
        for (parsed.callibration.items) |o| {
            o.components.deinit();
        }
        parsed.callibration.deinit();
    }

    const ans_part1 = try part1(&parsed, allocator);
    std.debug.print("Part 1: {d}\n", .{ans_part1});

    const ans_part2 = try part2(&parsed, allocator);
    std.debug.print("Part 2: {d}\n", .{ans_part2});
}

const CallibrationEquation = struct {
    total: equation_size,
    components: std.ArrayList(equation_size),
};

const ParsedInput = struct { callibration: std.ArrayList(CallibrationEquation) };

pub fn part1(input: *ParsedInput, _: std.mem.Allocator) !usize {
    var total: usize = 0;
    for (input.callibration.items) |*equation| {
        if (is_valid(equation, 0, 0, false)) {
            total += equation.total;
        }
    }
    return total;
}

pub fn part2(input: *ParsedInput, _: std.mem.Allocator) !usize {
    var total: usize = 0;
    for (input.callibration.items) |*equation| {
        if (is_valid(equation, 0, 0, true)) {
            total += equation.total;
        }
    }
    return total;
}

pub fn is_valid(equation: *CallibrationEquation, index: usize, total: equation_size, include_concat: bool) bool {
    if (index == equation.components.items.len) {
        return total == equation.total;
    } else {
        const component = equation.components.items[index];
        return is_valid(equation, index + 1, total * component, include_concat) or
            is_valid(equation, index + 1, total + component, include_concat) or
            (include_concat and is_valid(equation, index + 1, (total * std.math.pow(equation_size, 10, std.math.log10(component) + 1)) + component, include_concat));
    }
}

pub fn parse(allocator: std.mem.Allocator) !ParsedInput {
    const stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    const stdin = br.reader();

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var input_itr = std.mem.split(u8, buf[0..size], "\n");

    var callibration_equations = std.ArrayList(CallibrationEquation).init(allocator);

    while (input_itr.next()) |line| {
        if (line.len <= 0) continue;
        var line_itr = std.mem.split(u8, line, ": ");
        const total = try std.fmt.parseInt(equation_size, line_itr.next().?, 10);
        var components_itr = std.mem.split(u8, line_itr.next().?, " ");
        var list = std.ArrayList(equation_size).init(allocator);
        while (components_itr.next()) |component| {
            try list.append(try std.fmt.parseInt(equation_size, component, 10));
        }

        try callibration_equations.append(CallibrationEquation{
            .total = total,
            .components = list,
        });
    }

    return ParsedInput{ .callibration = callibration_equations };
}
