package me.swords1234.adventOfCode.year2020

import java.io.File
import kotlin.streams.toList


@ExperimentalStdlibApi
fun main() {
    fun part1(input: List<String>): Int {
        val map: MutableMap<String, Array<IntRange>> = mutableMapOf()

        val iterator = input.iterator()


        while (true) {
            val line = iterator.next()
            if (line == "your ticket:" || line.isEmpty()) break
            val str = Regex("(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)").matchEntire(line)?.destructured?.toList()!![0]

            val no1 = Regex("(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)").matchEntire(line)?.destructured?.toList()?.get(1)!!.toInt()
            val no2 = Regex("(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)").matchEntire(line)?.destructured?.toList()?.get(2)!!.toInt()

            val no3 = Regex("(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)").matchEntire(line)?.destructured?.toList()?.get(3)!!.toInt()
            val no4 = Regex("(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)").matchEntire(line)?.destructured?.toList()?.get(4)!!.toInt()

            map[str] = arrayOf(no1..no2, no3..no4)
        }
        var check = false
        var ticketScanningErrorRate = 0

        while(iterator.hasNext()) {
            val line = iterator.next()
            if (line == "nearby tickets:") {check = true; continue}

            if (check) {
                numLoop@ for (num in line.split(",").stream().mapToInt(String::toInt)) {
                    for (it in map.values) {
                        for (range in it) {
                            if (num in range) continue@numLoop
                        }
                    }
                    ticketScanningErrorRate += num
                }
            }
        }

        return ticketScanningErrorRate
    }

    fun part2(input: List<String>): Long {
        val ranges: MutableMap<String, Pair<IntRange, IntRange>> = mutableMapOf()

        // Initialising my ticket and ranges for each field
        val myTicket = input[input.indexOf("your ticket:") + 1].split(",").stream().mapToInt(String::toInt).toArray()

        for (line in input) {
            if (line.isEmpty()) break
            val regex = Regex("(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)")

            if (regex.matches(line)) {
                val str = regex.matchEntire(line)?.destructured?.toList()!![0]

                val no1 = regex.matchEntire(line)?.destructured?.toList()?.get(1)!!.toInt()
                val no2 = regex.matchEntire(line)?.destructured?.toList()?.get(2)!!.toInt()

                val no3 = regex.matchEntire(line)?.destructured?.toList()?.get(3)!!.toInt()
                val no4 = regex.matchEntire(line)?.destructured?.toList()?.get(4)!!.toInt()

                ranges[str] = Pair(no1..no2, no3..no4)
            }

        }

        // Processing all the valid tickets
        val nearbyTicket: Array<MutableList<Int>> = Array(myTicket.size) { mutableListOf() }
        tickets@ for(i in (input.indexOf("nearby tickets:") + 1) until input.size) {

            val line = input[i]
            val values = line.split(",").stream().mapToInt(String::toInt).toList()
            for (key in values.indices) {
                val value = values[key]
                if (ranges.values.none {
                            (value in it.first || value in it.second)
                        }) continue@tickets
            }

            for (key in values.indices) {
                val value = values[key]
                nearbyTicket[key].add(value)
            }
        }

        // Finding all the valid positions per field
        val validPositions: Map<String, MutableList<Int>> = buildMap {
            for (key in ranges.keys) put(key, mutableListOf())
        }

        for (pos in nearbyTicket.indices) {
            ranges@ for ((str, ranges) in ranges.entries) {
                for (i in nearbyTicket[pos]) {
                    val (range1, range2) = ranges

                    if (i !in range1 && i !in range2) continue@ranges
                }
                validPositions[str]?.add(pos)
            }
        }

        // If there is only one valid position, remove it from the others, will eventually find all valid positions
        while (validPositions.values.stream().anyMatch { it.size > 1 }) {
            for ((key, value) in validPositions) {
                if (value.size == 1) {
                    for ((key2, value2) in validPositions) {
                        if (key != key2) value2.remove(value[0])
                    }
                }
            }
        }

        // Maps the values to the first field
        val actualPositions = validPositions.mapValues { it.value[0] }

        var depatureMultiplied = 1L

        // Multiply the numers
        for ((key, value) in actualPositions) {
            if (key.startsWith("departure")) depatureMultiplied *= myTicket[value]
        }

        return depatureMultiplied
    }

    val lines = File("day16.txt").readLines()
    println("Part 1: " + part1(lines))
    println("Part 2: " + part2(lines))
}