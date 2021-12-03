package me.swords1234.adventOfCode.year2020

import java.io.File


fun main() {
    Day2().part1()
    Day2().part2()
}

class Day2 {
    fun part1() {

        val lines = File("day2.txt").readLines()

        var valid = 0
        lines.forEach { str ->
            val data = str.split("-", " ", ": ")
            val num = data[3].count { it == data[2].toCharArray()[0] }
            if (num >= Integer.parseInt(data[0]) && num <= Integer.parseInt(data[1])) valid++
        }
        println("Part 1: " + valid)
    }
    fun part2() {

        val lines = File("day2.txt").readLines()

        var valid = 0
        lines.forEach { str ->
            val data = str.split("-", " ", ": ")
            if ((data[3][Integer.parseInt(data[0]) - 1] == data[2].toCharArray()[0]).xor(data[3][Integer.parseInt(data[1]) - 1] == data[2].toCharArray()[0])) {
                valid++
            }
        }
        println("Part 2: " + valid)
    }
}