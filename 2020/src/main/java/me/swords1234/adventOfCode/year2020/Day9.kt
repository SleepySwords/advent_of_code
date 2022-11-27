package me.swords1234.adventOfCode.year2020

import java.io.File
import kotlin.math.abs
import kotlin.streams.toList


fun main() {
    fun part1(input: List<String>): Long {
        val numbers = input.stream().mapToLong(String::toLong).toList()

        val preamble: MutableList<Long> = mutableListOf()
        val iterator = numbers.iterator()

        while (preamble.size != 25) { preamble.add(iterator.next()) }

        while (iterator.hasNext()) {
//            println(preamble)
            val bit = iterator.next()
            var boolean = false
            for (num in 0 until 25) {
                if (bit / 2 == preamble[num]) {
                    if (preamble.count { it == abs(bit - preamble[num]) } < 2) continue
                }
                if (preamble.contains(abs(bit - preamble[num]))) {
                    boolean = true
                }
            }
            if (!boolean) return bit

            preamble.removeFirst()
            preamble.add(bit)
        }
        return -1
    }

    fun part2(input: List<String>): Long {
        val numbers = input.stream().mapToLong(String::toLong).toList().iterator()

        val preamble: MutableList<Long> = mutableListOf()
        val number = part1(input)


        while (true) {
            if (preamble.sum() < number) {
                preamble.add(numbers.next())
            }
            if (preamble.sum() > number) {
                preamble.removeFirst()
            }



            if (preamble.sum() == number) {
                return preamble.toSortedSet().first() + preamble.toSortedSet().last()
            }
        }
    }

    val lines = File("day9.txt").readLines()
    println("Part 1: " + part1(lines))
    println("Part 2: " + part2(lines))
}