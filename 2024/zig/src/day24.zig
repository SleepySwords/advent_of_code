const std = @import("std");

var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    // defer if (GPA.deinit() == std.heap.Check.leak) {
    //     std.debug.print("Wow", .{});
    // };
    var input = try parse(allocator);
    defer input.initial_values.deinit();
    defer input.operation.deinit();

    var final_value: usize = 0;

    for (0..46) |i| {
        var e = std.ArrayList(u8).init(allocator);

        const max_len = 20;
        var buf: [max_len]u8 = undefined;
        const numAsString = try std.fmt.bufPrint(&buf, "z{:0>2}", .{i});
        try e.appendSlice(numAsString);
        const ok = try calculate_value(e, &input.initial_values, &input.operation);
        final_value |= @as(usize, ok) << cast(u6, i);
    }
    std.debug.print("Part 1: {}\n", .{final_value});

    for (0..46) |i| {
        if (try needs_inspection(i, &input.operation)) {
            std.debug.print(": z{:0>2}\n", .{i});
        }
    }
}

inline fn cast(T: type, v: anytype) T {
    return @intCast(v);
}

const Name = std.ArrayList(u8);

const Pair = struct { lhs: Name, rhs: Name };

const OperationType = enum {
    o_and,
    o_xor,
    o_or,
};

const Operation = struct {
    operators: Pair,
    type: OperationType,
};

fn calculate_value(value: std.ArrayList(u8), calculated: *std.StringHashMap(u1), operations: *std.StringHashMap(Operation)) !u1 {
    if (operations.get(value.items[0..])) |operation| {
        const lhs_value = lhs: {
            if (calculated.get(operation.operators.lhs.items[0..])) |v| {
                break :lhs v;
            } else {
                const lhs = try calculate_value(operation.operators.lhs, calculated, operations);
                try calculated.put(operation.operators.lhs.items[0..], lhs);
                break :lhs lhs;
            }
        };
        const rhs_value = rhs: {
            if (calculated.get(operation.operators.rhs.items[0..])) |v| {
                break :rhs v;
            } else {
                const rhs = try calculate_value(operation.operators.rhs, calculated, operations);
                try calculated.put(operation.operators.rhs.items[0..], rhs);
                break :rhs rhs;
            }
        };

        const to_value = switch (operation.type) {
            .o_or => lhs_value | rhs_value,
            .o_xor => lhs_value ^ rhs_value,
            .o_and => lhs_value & rhs_value,
        };
        try calculated.put(value.items[0..], to_value);
        return to_value;
    } else {
        std.debug.print("Invalid\n", .{});
        return 0;
    }
}

fn parse(allocator: std.mem.Allocator) !Input {
    var stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    var stdin = br.reader();

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var line_itr = std.mem.split(u8, buf[0..size], "\n\n");
    const initial_str = line_itr.next().?;

    var initial_values = std.StringHashMap(u1).init(allocator);

    var initial_itr = std.mem.split(u8, initial_str, "\n");
    while (initial_itr.next()) |line| {
        if (line.len == 0) continue;

        const initial_value = try std.fmt.parseInt(u1, line[5..6], 10);
        try initial_values.put(line[0..3], initial_value);
    }

    var operation = std.StringHashMap(Operation).init(allocator);

    const operation_str = line_itr.next().?;
    var operation_itr = std.mem.split(u8, operation_str, "\n");
    while (operation_itr.next()) |line| {
        if (line.len == 0) continue;

        var spl = std.mem.split(u8, line, " ");
        const lhs = spl.next().?;
        const op = spl.next().?;
        const rhs = spl.next().?;
        _ = spl.next();
        const result = spl.next().?;

        var op_type: OperationType = undefined;

        if (std.mem.eql(u8, op, "XOR")) {
            op_type = .o_xor;
        } else if (std.mem.eql(u8, op, "OR")) {
            op_type = .o_or;
        } else {
            op_type = .o_and;
        }

        var lhs_arr = std.ArrayList(u8).init(allocator);
        try lhs_arr.appendSlice(lhs);

        var rhs_arr = std.ArrayList(u8).init(allocator);
        try rhs_arr.appendSlice(rhs);

        const oper = Operation{ .type = op_type, .operators = .{ .lhs = lhs_arr, .rhs = rhs_arr } };

        try operation.put(result, oper);
    }

    return Input{ .initial_values = initial_values, .operation = operation };
}

// Working out
// T := A XOR B
// L := A AND B
// K : = Cin XOR T
//
//
// Cout := L OR K
// S := T XOR Cin
//
//
// after finding incorrect values, manually inspected them to find the other pair.
fn needs_inspection(value: usize, operations: *std.StringHashMap(Operation)) !bool {
    const max_len = 20;
    var buf: [max_len]u8 = undefined;
    const numAsString = try std.fmt.bufPrint(&buf, "z{:0>2}", .{value});

    if (operations.get(numAsString)) |v| {
        // 45 has been manually verified, it does not require xor.
        if (value == 45 or value == 0 or value == 1) {
            return false;
        }
        if (v.type != .o_xor) {
            std.debug.print("Final operation is not xor", .{});
            return true;
        }

        if (operations.get(v.operators.lhs.items[0..])) |l| {
            if (l.type == .o_xor) {
                // Must be the input A and B for the final A XOR B XOR Cin
                var buf_x: [max_len]u8 = undefined;
                const x_str = try std.fmt.bufPrint(&buf_x, "x{:0>2}", .{value});
                if (!std.mem.eql(u8, l.operators.lhs.items[0..], x_str) and !std.mem.eql(u8, l.operators.rhs.items[0..], x_str)) {
                    std.debug.print("Subcomponent {s} is not an an xor of a and b", .{v.operators.lhs.items[0..]});
                    return true;
                }
            } else if (l.type == .o_or) {
                // Must be the carry
            } else {
                std.debug.print("Subcomponent {s} is not the carry or an xor of a and b", .{v.operators.rhs.items[0..]});
                return true;
            }
        } else {
            std.debug.print("Uses primative operator", .{});
            return true;
        }

        if (operations.get(v.operators.rhs.items[0..])) |l| {
            if (l.type == .o_xor) {
                // Must be the input A and B for the final A XOR B XOR Cin
                var buf_x: [max_len]u8 = undefined;
                const x_str = try std.fmt.bufPrint(&buf_x, "x{:0>2}", .{value});
                if (!std.mem.eql(u8, l.operators.lhs.items[0..], x_str) and !std.mem.eql(u8, l.operators.rhs.items[0..], x_str)) {
                    std.debug.print("Subcomponent {s} is not an an xor of a and b", .{v.operators.rhs.items[0..]});
                    return true;
                }
            } else if (l.type == .o_or) {
                // Must be the carry
            } else {
                std.debug.print("Subcomponent {s} is not the carry or an xor of a and b", .{v.operators.rhs.items[0..]});
                return true;
            }
        } else {
            std.debug.print("Uses primative operator", .{});
            return true;
        }
        return false;
    }

    std.debug.print("Did not find?!", .{});
    return true;
}

const Input = struct {
    operation: std.StringHashMap(Operation),
    initial_values: std.StringHashMap(u1),
};
