package me.swords1234.adventOfCode.year2020

import java.io.File


fun main() {
    fun part1(input: List<String>): Int {

        val set: MutableSet<Char> = mutableSetOf()
        var count = 0

        for (line in input) {
            if (line.isEmpty()) {
                count += set.size
                set.clear()
            }

            set.addAll(line.toCharArray().asList())
        }
        count += set.size
        return count
    }

    fun part2(input: List<String>): Int {


        val list: MutableList<Char> = input[0].toMutableList()
        var count = 0

        for (i in 1 until input.size) {
            val line = input[i]


            if (line.isEmpty()) continue

            if (input[i - 1].isEmpty()) {
                count += list.size
                list.clear()
                list.addAll(line.toMutableList())
                continue
            }


            val iterator = list.iterator()

            while(iterator.hasNext()){
                val item = iterator.next()
                if (!line.contains(item)) iterator.remove()
            }
        }
        count += list.size
        return count
    }

    val lines = File("day6.txt").readLines()
    println("Part 1: " + Util.bench({ part1(lines) }, System.out::println))
    println("Part 2: " + Util.bench({ part2(lines) }, System.out::println))
}