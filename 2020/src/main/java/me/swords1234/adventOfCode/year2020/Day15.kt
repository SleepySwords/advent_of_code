package me.swords1234.adventOfCode.year2020

import java.io.File


@ExperimentalUnsignedTypes
fun main() {
    fun part1(input: List<String>): Int {
        val mutableMap = mutableMapOf<Int, Int>()
        var turnNumber = input[0].split(",").size + 1
        var nextNum = 0

        input[0].split(",").forEachIndexed { i: Int, s: String ->
            if (i != input[0].split(",").size - 1) mutableMap[s.toInt()] = i + 1
            else nextNum = s.toInt()
        }

        while (turnNumber != 2021) {
            if (!mutableMap.containsKey(nextNum)) {
                mutableMap[nextNum] = turnNumber - 1
                nextNum = 0
            } else {
                val old = nextNum
                nextNum = turnNumber - 1 - mutableMap[nextNum]!!
                mutableMap[old] = turnNumber - 1
            }

            turnNumber++
        }

        return nextNum
    }

    fun part1Debug(input: List<String>) {
        val mutableMap = mutableMapOf<Int, Int>()
        var turnNumber = input[0].split(",").size + 1
        var nextNum = 0

        input[0].split(",").forEachIndexed { i: Int, s: String ->
            if (i != input[0].split(",").size - 1) mutableMap[s.toInt()] = i + 1
            else nextNum = s.toInt()
        }

        println("$turnNumber $nextNum")

        while (turnNumber != 2021) {
            println("${turnNumber - 1} Elf called $nextNum")
            println("Previously called: $mutableMap")
            if (!mutableMap.containsKey(nextNum)) {
                mutableMap[nextNum] = turnNumber - 1
                nextNum = 0
                println("Not called before")
            } else {
                val old = nextNum
                nextNum = turnNumber - 1 - mutableMap[nextNum]!!
                println("${turnNumber - 1} - ${mutableMap[old]} = $nextNum")
                mutableMap[old] = turnNumber - 1
            }

            turnNumber++
        }

        println("$nextNum")
    }

    fun part2(input: List<String>): Int {

        val mutableMap = mutableMapOf<Int, Int>()
        var turnNumber = input[0].split(",").size + 1
        var nextNum = 0

        input[0].split(",").forEachIndexed { i: Int, s: String ->
            if (i != input[0].split(",").size - 1) mutableMap[s.toInt()] = i + 1
            else nextNum = s.toInt()
        }

        while (turnNumber != 30000001) {
            if (!mutableMap.containsKey(nextNum)) {
                mutableMap[nextNum] = turnNumber - 1
                nextNum = 0
            } else {
                val old = nextNum
                nextNum = turnNumber - 1 - mutableMap[nextNum]!!
                mutableMap[old] = turnNumber - 1
            }

            turnNumber++
        }

        return nextNum
    }

    val lines = File("day15.txt").readLines()
    println("Part 1: " + part1(lines))
    println("Part 2: " + part2(lines))
}