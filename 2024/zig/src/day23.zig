const std = @import("std");

var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("Wow", .{});
    };

    var input = try parse(allocator);
    defer {
        for (input.verticies.items) |o| o.deinit();
        input.verticies.deinit();
        var itr = input.data.iterator();
        while (itr.next()) |entry| {
            for (entry.value_ptr.items) |o| o.deinit();
            entry.value_ptr.deinit();
        }
        input.data.deinit();
    }

    const count = try three_clique_count(&input, allocator);
    std.debug.print("Part 1: {}\n", .{count});

    var r = std.StringHashMap(void).init(allocator);
    defer r.deinit();
    var p = std.StringHashMap(void).init(allocator);
    for (input.verticies.items) |v| {
        try p.put(v.items, {});
    }
    defer p.deinit();
    var x = std.StringHashMap(void).init(allocator);
    defer x.deinit();
    var cliques = std.ArrayList(std.StringHashMap(void)).init(allocator);
    defer cliques.deinit();
    try maximum_clique(&r, &p, &x, &input, &cliques, allocator);

    var max_index: usize = 0;
    var max_found: usize = 0;

    for (cliques.items, 0..) |clique, i| {
        if (max_found < clique.count()) {
            max_index = i;
            max_found = clique.count();
        }
    }

    var max_clique = std.ArrayList([]const u8).init(allocator);
    defer max_clique.deinit();
    var clique_itr = cliques.items[max_index].keyIterator();
    while (clique_itr.next()) |cli| {
        try max_clique.append(cli.*);
    }

    std.sort.pdq([]const u8, max_clique.items, {}, struct {
        pub fn compare(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.order(u8, a, b).compare(std.math.CompareOperator.lt);
        }
    }.compare);

    std.debug.print("Part 2: ", .{});
    for (max_clique.items, 0..) |cli, i| {
        std.debug.print("{s}", .{cli});
        if (i != max_clique.items.len - 1) {
            std.debug.print(",", .{});
        }
    }

    std.debug.print("\n", .{});

    for (cliques.items) |*clique| {
        clique.deinit();
    }
}

fn three_clique_count(graph: *const Graph, allocator: std.mem.Allocator) !usize {
    var visited = std.StringHashMap(void).init(allocator);
    defer visited.deinit();
    var count_num: usize = 0;

    for (graph.verticies.items) |ver| {
        if (ver.items[0] != 't') {
            continue;
        }
        try visited.put(ver.items, {});
        if (graph.data.get(ver.items)) |edges| {
            for (edges.items, 0..) |first, i| {
                if (visited.contains(first.items)) continue;
                for (edges.items[i..]) |second| {
                    if (visited.contains(second.items)) continue;
                    if (std.mem.eql(u8, first.items, second.items)) {
                        continue;
                    }

                    if (graph.data.get(first.items)) |f_edge| {
                        for (f_edge.items) |e| {
                            if (std.mem.eql(u8, e.items, second.items)) {
                                count_num += 1;
                                std.debug.print("{s} {s} {s}\n", .{ first.items, second.items, ver.items });
                            }
                        }
                    }
                }
            }
        }
    }

    return count_num;
}

fn is_connected_vertex(a: []const u8, b: []const u8, graph: *const Graph) bool {
    const edges = graph.data.get(a) orelse return false;
    for (edges.items) |edge| {
        if (std.mem.eql(u8, edge.items, b)) {
            return true;
        }
    }
    return false;
}

fn is_connected(clique: *std.StringHashMap(void), vertex: []const u8, graph: *const Graph) bool {
    const edges = graph.data.get(vertex) orelse return false;
    var itr = clique.iterator();
    while (itr.next()) |v| {
        var found = false;
        for (edges.items) |edge| {
            if (std.mem.eql(u8, edge.items, v.key_ptr.*)) {
                found = true;
            }
        }
        if (!found) {
            return false;
        }
    }

    return true;
}

// https://www.geeksforgeeks.org/maximal-clique-problem-recursive-solution/
fn maximum_clique(r: *std.StringHashMap(void), p: *std.StringHashMap(void), x: *std.StringHashMap(void), graph: *const Graph, cliques_found: *std.ArrayList(std.StringHashMap(void)), allocator: std.mem.Allocator) !void {
    if (p.count() == 0 and x.count() == 0) {
        try cliques_found.append(try r.clone());
    }

    while (p.count() != 0) {
        var new_r = try r.clone();
        defer new_r.deinit();
        var p_itr = p.keyIterator();
        const vertex = p_itr.next();
        try new_r.put(vertex.?.*, {});
        var new_p = std.StringHashMap(void).init(allocator);
        defer new_p.deinit();
        while (p_itr.next()) |v_p| {
            if (is_connected_vertex(v_p.*, vertex.?.*, graph)) {
                try new_p.put(v_p.*, {});
            }
        }
        var new_x = std.StringHashMap(void).init(allocator);
        defer new_x.deinit();
        var x_itr = x.keyIterator();
        while (x_itr.next()) |v_x| {
            if (is_connected_vertex(v_x.*, vertex.?.*, graph)) {
                try new_x.put(v_x.*, {});
            }
        }

        try maximum_clique(&new_r, &new_p, &new_x, graph, cliques_found, allocator);

        try x.put(vertex.?.*, {});
        _ = p.remove(vertex.?.*);
    }
}

fn equivalent_classes(graph: *const Graph, allocator: std.mem.Allocator) !std.ArrayList(std.StringHashMap(void)) {
    var equivalent_classes_list = std.ArrayList(std.StringHashMap(void)).init(allocator);
    var visited = std.StringHashMap(void).init(allocator);
    defer visited.deinit();

    var vertex_itr = graph.verticies.iterator();
    while (vertex_itr.next()) |vertex| {
        if (visited.contains(vertex.key_ptr.*)) continue;

        var equivalent_class = std.StringHashMap(void).init(allocator);

        var queue = std.ArrayList([]const u8).init(allocator);
        defer queue.deinit();
        try queue.append(vertex.key_ptr.*);

        while (queue.items.len != 0) {
            const item = queue.pop();
            if (visited.contains(item)) continue;
            try visited.put(item, {});
            for (graph.data.get(item).?.items) |edge| {
                try queue.append(edge.items);
                try equivalent_class.put(edge.items, {});
            }
        }

        try equivalent_classes_list.append(equivalent_class);
    }

    return equivalent_classes_list;
}

const Graph = struct {
    data: std.StringHashMap(std.ArrayList(std.ArrayList(u8))),
    verticies: std.ArrayList(std.ArrayList(u8)),
};

fn parse(allocator: std.mem.Allocator) !Graph {
    var stdin_file = std.io.getStdIn();
    var br = std.io.bufferedReader(stdin_file.reader());
    var stdin = br.reader();

    var buf: [65_536]u8 = undefined;
    const size = try stdin.readAll(&buf);

    var edges_itr = std.mem.split(u8, buf[0..size], "\n");

    var edges = std.StringHashMap(std.ArrayList(std.ArrayList(u8))).init(allocator);
    var vertecies = std.StringHashMap(void).init(allocator);
    defer vertecies.deinit();

    while (edges_itr.next()) |line| {
        if (line.len == 0) continue;

        var result = try edges.getOrPut(line[0..2]);
        if (!result.found_existing) {
            result.value_ptr.* = std.ArrayList(std.ArrayList(u8)).init(allocator);
        }

        var m = std.ArrayList(u8).init(allocator);
        try m.appendSlice(line[3..5]);
        try result.value_ptr.append(m);

        var other_result = try edges.getOrPut(line[3..5]);
        if (!other_result.found_existing) {
            other_result.value_ptr.* = std.ArrayList(std.ArrayList(u8)).init(allocator);
        }

        var m2 = std.ArrayList(u8).init(allocator);
        try m2.appendSlice(line[0..2]);
        try other_result.value_ptr.append(m2);

        try vertecies.put(line[0..2], {});
        try vertecies.put(line[3..5], {});
    }

    var str_vertecies = std.ArrayList(std.ArrayList(u8)).init(allocator);
    var itr = vertecies.keyIterator();
    while (itr.next()) |o| {
        var name = std.ArrayList(u8).init(allocator);
        try name.appendSlice(o.*);
        try str_vertecies.append(name);
    }

    return Graph{ .data = edges, .verticies = str_vertecies };
}
