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
    // try run_program2(&parsed);
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
    return &state.registers.items[@intFromEnum(register)];
}

fn get_reg_value(register: Register, state: *const State) usize {
    return state.registers.items[@intFromEnum(register)];
}

fn run_program(init_state: *State, allocator: std.mem.Allocator) !void {
    var state = init_state.*;
    var output = std.ArrayList(usize).init(allocator);
    defer output.deinit();
    while (state.instruction_pointer < state.memory.items.len) {
        const o = evaluate_instruction(state.memory.items[state.instruction_pointer], state.memory.items[state.instruction_pointer + 1], &state);
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

fn run_program2(init_state: *State, allocator: std.mem.Allocator) !void {
    var cache = std.AutoHashMap(State, void).init(allocator);
    var register_a: usize = 0;
    ok: while (true) {
        var state = init_state.*;
        std.debug.print("Attempting Register A: {}\n", .{register_a});
        get_reg_ptr(.A, &state).* = register_a;
        var matched: usize = 0;
        while (state.instruction_pointer < state.memory.items.len) {
            if (cache.contains(state)) {}
            const o = evaluate_instruction(state.memory.items[state.instruction_pointer], state.memory.items[state.instruction_pointer + 1], &state);
            if (o) |a| {
                if (state.memory.items[matched] == a) {
                    matched += 1;
                } else {
                    break;
                }
            }
            cache.put(state, {});
        }
        if (matched == state.memory.items.len) {
            std.debug.print("Part 2: {}\n", .{register_a});
            break :ok;
        }
        register_a += 1;
    }
}

fn evaluate_instruction(instr: u3, operand: u3, state: *State) ?usize {
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
            return (evaluate_combo(operand, state) % 8);
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

const State = struct {
    registers: std.ArrayList(usize),
    memory: std.ArrayList(u3),
    instruction_pointer: usize,
};

const Register = enum(u3) { A = 0, B = 1, C = 2 };

fn parse(allocator: std.mem.Allocator) !State {
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

    return State{ .registers = registers, .memory = program, .instruction_pointer = 0 };
}
