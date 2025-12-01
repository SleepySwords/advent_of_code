const std = @import("std");

var GPA = std.heap.GeneralPurposeAllocator(.{}){};

pub fn main() !void {
    const allocator = GPA.allocator();
    defer if (GPA.deinit() == std.heap.Check.leak) {
        std.debug.print("Wow", .{});
    };

    var input = try parse(allocator);
    defer {
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
}

fn three_clique_count(graph: *const Graph, allocator: std.mem.Allocator) !usize {
    var visited = std.StringHashMap(void).init(allocator);
    defer visited.deinit();
    var count_num: usize = 0;

    var itr = graph.verticies.iterator();
    while (itr.next()) |ver| {
        if (ver.key_ptr.*[0] != 't') {
            continue;
        }
        try visited.put(ver.key_ptr.*, {});
        if (graph.data.get(ver.key_ptr.*)) |edges| {
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
                                std.debug.print("{s} {s} {s}\n", .{ first.items, second.items, ver.key_ptr.* });
                            }
                        }
                    }
                }
            }
        }
    }

    return count_num;
}

fn three_clique(graph: *const Graph, allocator: std.mem.Allocator) !std.ArrayList(std.StringHashMap(void)) {
    var visited = std.StringHashMap(void).init(allocator);
    defer visited.deinit();

    var cliques = std.ArrayList(std.StringHashMap(void)).init(allocator);

    var itr = graph.verticies.iterator();
    while (itr.next()) |ver| {
        if (ver.key_ptr.*[0] != 't') {
            continue;
        }
        try visited.put(ver.key_ptr.*, {});
        if (graph.data.get(ver.key_ptr.*)) |edges| {
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
                                var clique = std.StringHashMap(void).init(allocator);
                                try clique.put(first.items, {});
                                try clique.put(second.items, {});
                                try clique.put(ver.key_ptr.*, {});

                                cliques.append(cliques);
                            }
                        }
                    }
                }
            }
        }
    }

    return cliques;
}

fn grow(graph: *Graph, clique: *std.StringHashMap(void)) void {
    var itr = graph.verticies.iterator();
    while (itr.next()) |vertex| {
        if (clique.contains(vertex)) continue;
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
    verticies: std.StringHashMap(void),
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

    return Graph{ .data = edges, .verticies = vertecies };
}
