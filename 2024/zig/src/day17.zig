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
    const output = try run_program(State{
        .register_a = parsed.registers.items[0],
        .register_b = parsed.registers.items[1],
        .register_c = parsed.registers.items[2],
        .instruction_pointer = 0,
    }, &parsed, allocator);
    defer output.deinit();
    std.debug.print("Part 1: ", .{});
    for (output.items) |o| {
        std.debug.print("{},", .{o});
    }
    std.debug.print("\n", .{});

    try part2(&parsed, allocator);
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

fn run_program(initial_state: State, input: *Input, allocator: std.mem.Allocator) !std.ArrayList(usize) {
    var state = initial_state;
    var output = std.ArrayList(usize).init(allocator);
    while (state.instruction_pointer < input.memory.items.len) {
        const instr = input.memory.items[state.instruction_pointer];
        const operand = input.memory.items[state.instruction_pointer + 1];
        if (evaluate_instruction(instr, operand, &state)) |result| {
            try output.append(result);
        }
    }

    return output;
}

// Working out
//
// B = A % 8; // A & (111)
// B = B ^ 2; // B ^ (010)
// C = A >> B; // Theoretical max of shifting by 7
// B = B ^ 7; // B ^ (111) // Acts as ~B
// B = B ^ C;
// A = A >> 3;
// OUT B % 8; // B & (111)
// JUMP 0;

// 10000001111
// B = 111
// B = 101
// C = 100000
// B =    010
// B =    000
//
//
// Only 10 bits at a time can affect the output, hence we can cycle through
// eliminating invalid registers, and adding to ones that produce valid values.

fn part2(input: *Input, allocator: std.mem.Allocator) !void {
    var count: usize = 0;
    var valid = std.ArrayList(usize).init(allocator);
    defer valid.deinit();
    var register_a: usize = 0;
    while (true) {
        const state = State{
            .register_a = register_a,
            .register_b = input.registers.items[1],
            .register_c = input.registers.items[2],
            .instruction_pointer = 0,
        };
        const output = try run_program(state, input, allocator);
        defer output.deinit();
        if (output.items[0] == input.memory.items[0]) {
            count += 1;
            try valid.append(register_a);
        }
        if (register_a > 0b00011_1111_1111) {
            break;
        }
        register_a += 1;
    }

    for (1..input.memory.items.len) |i| {
        var valid_buffer = std.ArrayList(usize).init(allocator);
        defer valid_buffer.deinit();
        for (0..(0b111 + 1)) |mask| {
            for (valid.items) |valid_reg_a| {
                const reg_a = valid_reg_a | (mask << @intCast(7 + (3 * i)));
                const state = State{
                    .register_a = reg_a,
                    .register_b = input.registers.items[1],
                    .register_c = input.registers.items[2],
                    .instruction_pointer = 0,
                };
                const output = try run_program(state, input, allocator);
                defer output.deinit();
                if (output.items.len > i) {
                    if (output.items[i] == input.memory.items[i]) {
                        try valid_buffer.append(reg_a);
                    }
                }
            }
        }

        {
            const swap = valid;
            valid = valid_buffer;
            valid_buffer = swap;
            valid_buffer.clearRetainingCapacity();
        }
    }

    var lowest: usize = std.math.maxInt(usize);
    for (valid.items) |v| {
        if (v < lowest) lowest = v;
    }
    std.debug.print("Part 2: {}\n", .{lowest});
}

fn evaluate_instruction(instr: u3, operand: u3, state: *State) ?u8 {
    switch (instr) {
        // adv
        0 => {
            get_reg_ptr(.A, state).* >>= @intCast(evaluate_combo(operand, state));
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
            get_reg_ptr(.B, state).* = get_reg_value(.A, state) >> @intCast(evaluate_combo(operand, state));
        },
        7 => {
            get_reg_ptr(.C, state).* = get_reg_value(.A, state) >> @intCast(evaluate_combo(operand, state));
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
