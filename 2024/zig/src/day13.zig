const std = @import("std");

var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("wow", .{});
    };

    const input = try parse(allocator);
    defer input.games.deinit();
    for (input.games.items) |game| {
        std.debug.print("{}\n", .{game});
    }

    const ans_pt1 = part1(&input);
    std.debug.print("Part 1: {}\n", .{ans_pt1});

    const ans_pt2 = part2(&input);
    std.debug.print("Part 2: {}\n", .{ans_pt2});
}

const Pair = struct { x: usize, y: usize };

const Game = struct { button_a: Pair, button_b: Pair, prize: Pair };

const Input = struct { games: std.ArrayList(Game) };

// a_x^a + b_x^b = prize_x
// a_y^a + b_y^b = prize_y

fn part1(input: *const Input) usize {
    var total: usize = 0;
    for (input.games.items) |game| {
        var a: usize = 0;
        a_loop: while (true) : (a += 1) {
            if ((game.button_a.x * a) > game.prize.x) {
                break :a_loop;
            }
            if ((game.button_a.y * a) > game.prize.y) {
                break :a_loop;
            }
            var b: usize = 0;
            while (true) : (b += 1) {
                if ((game.button_a.x * a) + (game.button_b.x * b) > game.prize.x) {
                    break;
                }

                if ((game.button_a.y * a) + (game.button_b.y * b) > game.prize.y) {
                    break;
                }
                if ((game.button_a.x * a) + (game.button_b.x * b) == game.prize.x and (game.button_a.y * a) + (game.button_b.y * b) == game.prize.y) {
                    total += a * 3 + b;
                }
            }
        }
    }

    return total;
}

fn find_t(game: *const Game) f64 {
    const a_x: f64 = @floatFromInt(game.button_a.x);
    const a_y: f64 = @floatFromInt(game.button_a.y);
    const b_x: f64 = @floatFromInt(game.button_b.x);
    const b_y: f64 = @floatFromInt(game.button_b.y);
    const prize_x: f64 = @floatFromInt(game.prize.x);
    const prize_y: f64 = @floatFromInt(game.prize.y);

    const numerator = prize_x - (a_x / a_y) * prize_y;
    const denominator = b_x - (a_x * b_y) / a_y;

    return numerator / denominator;
}

fn find_t_2(game: *const Game) isize {
    const a_x: isize = @intCast(game.button_a.x);
    const a_y: isize = @intCast(game.button_a.y);
    const b_x: isize = @intCast(game.button_b.x);
    const b_y: isize = @intCast(game.button_b.y);
    const prize_x: isize = @intCast(game.prize.x);
    const prize_y: isize = @intCast(game.prize.y);

    const numerator = a_y * prize_x - a_x * prize_y;
    const denominator = a_y * b_x - a_x * b_y;

    return @divTrunc(numerator, denominator);
}

fn find_s(game: *const Game) isize {
    const a_x: isize = @intCast(game.button_a.x);
    const a_y: isize = @intCast(game.button_a.y);
    const b_x: isize = @intCast(game.button_b.x);
    const b_y: isize = @intCast(game.button_b.y);
    const prize_x: isize = @intCast(game.prize.x);
    const prize_y: isize = @intCast(game.prize.y);

    const numerator = b_y * prize_x - b_x * prize_y;
    const denominator = b_y * a_x - b_x * a_y;

    return @divTrunc(numerator, denominator);
}

fn part2(input: *const Input) usize {
    var total: usize = 0;
    for (input.games.items) |game| {
        const new_game = Game{ .button_a = game.button_a, .button_b = game.button_b, .prize = Pair{
            .x = game.prize.x + 10000000000000,
            .y = game.prize.y + 10000000000000,
        } };

        std.debug.print("{}\n", .{find_t_2(&new_game)});

        const t = find_t_2(&new_game);
        const s = find_s(&new_game);
        if (t >= 0 and s >= 0) {
            const t_u: usize = @intCast(t);
            const s_u: usize = @intCast(s);
            if ((new_game.button_a.x * s_u) + (new_game.button_b.x * t_u) == new_game.prize.x and (new_game.button_a.y * s_u) + (new_game.button_b.y * t_u) == new_game.prize.y) {
                total += s_u * 3 + t_u;
            } else {
                std.debug.print("{} {} {} {} \n", .{ s_u, t_u, (new_game.button_a.x * s_u) + (new_game.button_b.x * t_u), (new_game.button_a.y * s_u) + (new_game.button_b.y * t_u) });
            }
        }
    }

    return total;
}

fn parse(allocator: std.mem.Allocator) !Input {
    const stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    var stdin = br.reader();

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var games = std.ArrayList(Game).init(allocator);

    var game_itr = std.mem.split(u8, buf[0..size], "\n\n");
    while (game_itr.next()) |game_str| {
        var rows = std.mem.split(u8, game_str, "\n");
        const button_a_str = rows.next().?["Button A: ".len..];
        var button_a_offsets = std.mem.split(u8, button_a_str, ", ");
        const button_a = Pair{
            .x = try std.fmt.parseInt(usize, button_a_offsets.next().?[2..], 10),
            .y = try std.fmt.parseInt(usize, button_a_offsets.next().?[2..], 10),
        };

        const button_b_str = rows.next().?["Button B: ".len..];
        var button_b_offsets = std.mem.split(u8, button_b_str, ", ");
        const button_b = Pair{
            .x = try std.fmt.parseInt(usize, button_b_offsets.next().?[2..], 10),
            .y = try std.fmt.parseInt(usize, button_b_offsets.next().?[2..], 10),
        };

        const prize_str = rows.next().?["Prize: ".len..];
        var prize_offsets = std.mem.split(u8, prize_str, ", ");
        const prize = Pair{
            .x = try std.fmt.parseInt(usize, prize_offsets.next().?[2..], 10),
            .y = try std.fmt.parseInt(usize, prize_offsets.next().?[2..], 10),
        };

        const game = Game{ .button_a = button_a, .button_b = button_b, .prize = prize };

        try games.append(game);
    }

    return Input{ .games = games };
}
