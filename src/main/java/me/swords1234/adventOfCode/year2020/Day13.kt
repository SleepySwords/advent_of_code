package me.swords1234.adventOfCode.year2020

import java.io.File
import java.math.BigInteger


fun main() {
    fun part1(input: List<String>): Int {
        val earliestDepatureTime = input[0].toInt()
        var minBusID = Int.MAX_VALUE
        var minMinutes = Int.MAX_VALUE
        input[1].split(",").forEach {
            if (it != "x") {
                if (minMinutes > it.toInt() - (earliestDepatureTime % it.toInt())) {
                    minMinutes =  it.toInt() - (earliestDepatureTime % it.toInt())
                    minBusID = it.toInt()
                }
            }
        }
        return minBusID * minMinutes
    }

    fun part2(input: List<String>): BigInteger {

        var i = BigInteger.valueOf(100000000000000L-1)
        var valueToLog = BigInteger.valueOf(100000000000000L)
        val inputs = input[1].split(",").withIndex()

        loop@ while (true) {
            i++
            if (i > valueToLog) {
                valueToLog *= BigInteger.TEN
                println(i)
            }

            for ((pos, it) in inputs) {
                if (it != "x"
                        && BigInteger.ZERO != ((i + pos.toBigInteger()).mod(it.toBigInteger()))) {
                    continue@loop
                }
            }
            return i+ input[1].split(",").size.toBigInteger() - BigInteger.ONE
        }

    }

    val lines = File("day13.txt").readLines()
    println("Part 1: " + part1(lines))
    println("Part 2: " + part2(lines))
}