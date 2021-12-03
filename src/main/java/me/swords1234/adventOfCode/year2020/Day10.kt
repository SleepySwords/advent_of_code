package me.swords1234.adventOfCode.year2020

import java.io.File
import kotlin.math.pow
import kotlin.streams.toList


fun main() {
    fun part1(input: List<String>): Long {
        val adapters = input.stream().mapToLong(String::toLong).toList()
        val max_voltage = adapters.maxOrNull()

        val adapater_volt = max_voltage!! + 3

        var threeVolts = 1
        var oneVolts = 1

        for (i in 1 until adapater_volt) {
            if (adapters.contains(i) && adapters.contains(i + 1)) {
                oneVolts++
            } else if (adapters.contains(i) && adapters.contains(i + 3)) {
                    threeVolts++
            }
        }

        println ("$threeVolts $oneVolts")

        return (threeVolts * oneVolts).toLong()
    }

    fun part2(input: List<String>): Long {
        val adapters = input.stream().mapToLong(String::toLong).toList().sorted()
        val list = mutableListOf<Long>()
        list.add(0)
        list.addAll(adapters)
        list.add(adapters.maxOrNull()!! + 3)
        var previousAdapter = 0
        val removable = mutableListOf<Long>()
        // todo figure why 49 isnt showing. Thinngs that are within 3 of each other must be removed
        // todo if we remove 1, 2 and 3 we can't reach 4.
        var value = 1

        for (adapter in list.indices) {
            if (adapter == 0) continue
            if (adapter >= list.size - 1) continue

            if (list[adapter + 1] - list[adapter - 1] <= 3) {
                if (removable.contains(list[adapter-1]))
                removable.add(list[adapter])
            }
        }



        println(removable)

        println(list)
        
        println(2.0.pow(removable.size))

        return -1
    }

    val lines = File("day10.txt").readLines()
    println("Part 1: " + part1(lines))
    println("Part 2: " + part2(lines))
}