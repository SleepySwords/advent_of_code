package com.github.sleepyswords

class Node(private val display: String, val connections: MutableList<Node>) {
    fun numberPathsToWithRepeating(node: Node, visited: Boolean, banned: List<Node>): Int {
        if (this == node) {
            return 1
        }
        val list = ArrayList(banned)
        if (display.toCharArray().all(Char::isLowerCase)) {
            list.add(this)
        }
        var paths = 0
        for (n in connections.filter { !list.contains(it) || !visited }) {
            if (n.display == "start") {
                continue
            }
            paths += n.numberPathsToWithRepeating(node, n in list || visited, list)
        }
        return paths
    }

    fun numberPathsTo(node: Node, banned: List<Node>): Int {
        if (this == node) {
            return 1
        }
        val list = ArrayList(banned)
        if (display.toCharArray().all(Char::isLowerCase)) {
            list.add(this)
        }
        var paths = 0
        for (n in connections.filter { !banned.contains(it) }) {
            paths += n.numberPathsTo(node, list)
        }
        return paths
    }
}

fun main() {
    val map = HashMap<String, Node>()
    input.split("\n").forEach {
        val cave1 = it.split("-")[0]
        val cave2 = it.split("-")[1]
        val node1 = map.getOrPut(cave1) { Node(cave1, ArrayList()) }
        val node2 = map.getOrPut(cave2) { Node(cave2, ArrayList()) }
        node1.connections.add(node2)
        node2.connections.add(node1)
    }
    println(map["start"]?.numberPathsTo(map["end"]!!, ArrayList()))
    println(map["start"]?.numberPathsToWithRepeating(map["end"]!!, false, ArrayList()))
}

val input = "start-YA\n" +
        "ps-yq\n" +
        "zt-mu\n" +
        "JS-yi\n" +
        "yq-VJ\n" +
        "QT-ps\n" +
        "start-yq\n" +
        "YA-yi\n" +
        "start-nf\n" +
        "nf-YA\n" +
        "nf-JS\n" +
        "JS-ez\n" +
        "yq-JS\n" +
        "ps-JS\n" +
        "ps-yi\n" +
        "yq-nf\n" +
        "QT-yi\n" +
        "end-QT\n" +
        "nf-yi\n" +
        "zt-QT\n" +
        "end-ez\n" +
        "yq-YA\n" +
        "end-JS"