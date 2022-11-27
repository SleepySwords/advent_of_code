package me.swords1234.adventOfCode.year2020

import java.io.File


fun main() {


    val inputs = File("day7.txt").readLines()

    Util.bench({
        val map: MutableMap<String, Node<String>> = mutableMapOf()

        for (line in inputs) {
            val input = line.dropLast(1).split(" contain ", ", ")


            val root = input[0].replace(" bags", "").replace(" bag", "")

            map.putIfAbsent(root, Node(root))

            val node = map[root]!!

            if (line.contains("no other bags")) continue

            for (i in 1 until input.size) {
                val childString = input[i].replace(" bags", "").replace(" bag", "")
                val num: Int = Regex("(\\d*) (.*)").findAll(childString).toList()[0].destructured.toList()[0].toInt()
                val type = Regex("(\\d*) (.*)").findAll(childString).toList()[0].destructured.toList()[1]

                map.putIfAbsent(type, Node(type))

                val children = map[type]!!
                children.parent.add(node)

                node.children.add(Pair(num, children))
            }
        }

        fun part1(): Int {
            val set: MutableSet<Node<String>> = mutableSetOf()
            map["shiny gold"]!!.countOfUniqueParents(set)

            return (set.size)

        }

        fun part2(): Int {
            return map["shiny gold"]!!.countOfChildren()
        }


        println("Part 1: " + part1())
        println("Part 2: " + part2())
    }, System.out::println)
}

class Node<T>(val value: T) {

    val children: MutableList<Pair<Int, Node<T>>> = mutableListOf()
    val parent: MutableList<Node<T>> = mutableListOf()

    fun countOfChildren(): Int {

        var count = 0

        children.forEach {(num, children) ->
            count += num
            count += num * children.countOfChildren()
        }

        return count
    }

    fun countOfUniqueParents(nodes: MutableSet<Node<T>>) {
        parent.forEach {parent ->
            nodes.add(parent)
            parent.countOfUniqueParents(nodes)
        }
    }


    override fun toString(): String {
        return "$value"
    }
}