package me.swords1234.adventOfCode.year2020

import java.io.File


fun main() {
    val lines = File("day5.txt").readLines()
    println("Part 1: " + Util.bench({ Day5().part1(lines) }, System.out::println))
    println("Part 2: " + Util.bench({ Day5().part2(lines) }, System.out::println))
}

class Day5 {
    fun part1(input: List<String>): Int {

        var largestSeatID = 0

        for (line in input) {
            val seatID = convertBinaryToDecimal(line.replace("F", "0").replace("B", "1")
                .replace("L", "0").replace("R", "1").toLong())
            if (seatID > largestSeatID) largestSeatID = seatID
        }
        return largestSeatID
    }

    fun part2(input: List<String>): Int? {
        val seats: MutableList<Int> = mutableListOf()
        var potentialSeadID = 0

        for (line in input) {
            val seatID = convertBinaryToDecimal(line.replace("F", "0").replace("B", "1")
                    .replace("L", "0").replace("R", "1").toLong())
            seats.add(seatID)
        }

        for (seatID in seats.stream().sorted()) {

            if (seatID == potentialSeadID + 2) {
                return potentialSeadID + 1
            }
            potentialSeadID = seatID
        }
        return 0
    }

    fun main(args: Array<String>) {
        val num: Long = 110110111
        val decimal = convertBinaryToDecimal(num)
        println("$num in binary = $decimal in decimal")
    }

    private fun convertBinaryToDecimal(_num: Long): Int {
        var num = _num
        var decimalNumber = 0
        var i = 0
        var remainder: Long

        while (num.toInt() != 0) {
            remainder = num % 10
            num /= 10
            decimalNumber += (remainder * Math.pow(2.0, i.toDouble())).toInt()
            ++i
        }
        return decimalNumber
    }
}