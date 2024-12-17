const std = @import("std");
var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("Mmeory leak detected", .{});
    };

    var parsed = try parse(allocator);
    defer {
        parsed.memory.deinit();
        parsed.registers.deinit();
    }
    try run_program(&parsed, allocator);
    try run_program2(&parsed, allocator);
}

fn evaluate_combo(value: u3, state: *const State) usize {
    return switch (value) {
        0...3 => value,
        4 => get_reg_value(.A, state),
        5 => get_reg_value(.B, state),
        6 => get_reg_value(.C, state),
        7 => 0,
    };
}

fn evaluate_literal(value: u3) usize {
    return value;
}

fn get_reg_ptr(register: Register, state: *State) *usize {
    return switch (register) {
        .A => &state.register_a,
        .B => &state.register_b,
        .C => &state.register_c,
    };
}

fn get_reg_value(register: Register, state: *const State) usize {
    return switch (register) {
        .A => state.register_a,
        .B => state.register_b,
        .C => state.register_c,
    };
}

fn run_program(input: *Input, allocator: std.mem.Allocator) !void {
    var state = State{
        .register_a = input.registers.items[0],
        .register_b = input.registers.items[1],
        .register_c = input.registers.items[2],
        .instruction_pointer = 0,
    };
    var output = std.ArrayList(usize).init(allocator);
    defer output.deinit();
    while (state.instruction_pointer < input.memory.items.len) {
        const o = evaluate_instruction(input.memory.items[state.instruction_pointer], input.memory.items[state.instruction_pointer + 1], &state);
        if (o) |a| {
            try output.append(a);
        }
    }
    std.debug.print("Reg A: {}\nReg B: {}\nReg C: {}\n", .{ get_reg_value(.A, &state), get_reg_value(.B, &state), get_reg_value(.C, &state) });
    std.debug.print("Part 1: ", .{});
    for (output.items) |o| {
        std.debug.print("{},", .{o});
    }
    std.debug.print("\n", .{});
}

fn run_program_recursive(state: *State, outputs: usize, input: *Input, allocator: std.mem.Allocator, invalid: *std.AutoHashMap(State, void)) !?std.ArrayList(u8) {
    if (invalid.contains(state.*)) return null;
    if (state.instruction_pointer >= input.memory.items.len) return std.ArrayList(u8).init(allocator);

    const previous_state = state.*;

    const result = evaluate_instruction(input.memory.items[state.instruction_pointer], input.memory.items[state.instruction_pointer + 1], state);
    if (result) |a| {
        if (input.memory.items.len <= outputs or input.memory.items[outputs] != a) {
            // std.debug.print("Invalid\n", .{});
            // return output;
        }
    }
    var output = try run_program_recursive(state, if (result != null) outputs + 1 else outputs, input, allocator, invalid);
    if (output == null) {
        try invalid.put(previous_state, {});
        return null;
    }
    if (result) |a| {
        try output.?.append(a);

        var contains = false;
        for (input.memory.items) |m| {
            if (a == m) {
                contains = true;
            }
        }

        if (!contains) {
            try invalid.put(previous_state, {});
            output.?.deinit();
            return null;
        }

        if (output.?.items.len < input.memory.items.len) {
            var is_equal = true;
            for (output.?.items, 0..) |o, i| {
                if (o != input.memory.items[input.memory.items.len - 1 - i]) {
                    try invalid.put(previous_state, {});
                    is_equal = false;
                    output.?.deinit();
                    return null;
                }
            }
        }
    }

    return output.?;
}

fn run_program2(input: *Input, allocator: std.mem.Allocator) !void {
    var cache = std.AutoHashMap(State, void).init(allocator);
    defer {
        cache.deinit();
    }
    var current = std.ArrayList(State).init(allocator);
    defer current.deinit();
    var register_a: usize = 0;
    outer: while (true) {
        var state = State{
            .register_a = register_a,
            .register_b = input.registers.items[1],
            .register_c = input.registers.items[2],
            .instruction_pointer = 0,
        };
        std.debug.print("Attempting Register A: {}\n", .{register_a});
        const output = try run_program_recursive(&state, 0, input, allocator, &cache);
        if (output) |ot| {
            defer ot.deinit();
            var is_equal = ot.items.len == input.memory.items.len;
            if (is_equal) {
                for (ot.items, 0..) |o, i| {
                    if (o != input.memory.items[input.memory.items.len - 1 - i]) {
                        is_equal = false;
                        break;
                    }
                }
            }
            if (is_equal) {
                std.debug.print("Part 2: {}\n", .{register_a});
                break :outer;
            }
        }
        register_a += 1;
    }
}

fn evaluate_instruction(instr: u3, operand: u3, state: *State) ?u8 {
    switch (instr) {
        // adv
        0 => {
            get_reg_ptr(.A, state).* /= std.math.pow(usize, 2, evaluate_combo(operand, state));
        },
        // bxl
        1 => {
            get_reg_ptr(.B, state).* ^= evaluate_literal(operand);
        },
        // bst
        2 => {
            get_reg_ptr(.B, state).* = evaluate_combo(operand, state) % 8;
        },
        // jnz
        3 => {
            if (get_reg_value(.A, state) != 0) {
                state.instruction_pointer = evaluate_literal(operand);
                return null;
            }
        },
        // bxc
        4 => {
            get_reg_ptr(.B, state).* ^= get_reg_value(.C, state);
        },
        // out
        5 => {
            state.instruction_pointer += 2;
            return @intCast(evaluate_combo(operand, state) % 8);
        },
        // bdv
        6 => {
            get_reg_ptr(.B, state).* = get_reg_value(.A, state) / std.math.pow(usize, 2, evaluate_combo(operand, state));
        },
        7 => {
            get_reg_ptr(.C, state).* = get_reg_value(.A, state) / std.math.pow(usize, 2, evaluate_combo(operand, state));
        },
    }
    state.instruction_pointer += 2;
    return null;
}

const Input = struct {
    registers: std.ArrayList(usize),
    memory: std.ArrayList(u3),
};

const State = struct {
    register_a: usize,
    register_b: usize,
    register_c: usize,
    instruction_pointer: usize,
};

const Register = enum(u3) { A = 0, B = 1, C = 2 };

fn parse(allocator: std.mem.Allocator) !Input {
    const stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    var stdin = br.reader();

    var buf: [65_536]u8 = undefined;

    const size = try stdin.readAll(&buf);
    var input_itr = std.mem.split(u8, buf[0..size], "\n\n");
    const register_str = input_itr.next().?;
    const program_str = input_itr.next().?;

    var registers = std.ArrayList(usize).init(allocator);

    var register_itr = std.mem.split(u8, register_str, "\n");
    while (register_itr.next()) |reg| {
        const register = try std.fmt.parseInt(usize, reg["Register _: ".len..], 10);
        try registers.append(register);
    }

    var program = std.ArrayList(u3).init(allocator);

    var program_itr = std.mem.split(u8, program_str["Program: ".len..], ",");
    while (program_itr.next()) |v| {
        if (v.len == 0) continue;
        try program.append(try std.fmt.parseInt(u3, std.mem.trim(u8, v, " \n"), 10));
    }

    return Input{ .registers = registers, .memory = program };
}
