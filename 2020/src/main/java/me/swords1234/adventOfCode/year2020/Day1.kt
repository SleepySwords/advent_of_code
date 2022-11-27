package me.swords1234.adventOfCode.year2020

import java.io.File
import kotlin.streams.toList


fun main() {
    part1()
    part2()
}

fun part1() {

    val lines = File("day1.txt").readLines()
    val numbers = lines.stream().map(Integer::parseInt).sorted().toList()

    var first = 0
    var last = numbers.size - 1

    while(numbers[first] + numbers[last] != 2020) {
        if (numbers[first] + numbers[last] > 2020) {
            last--
        } else {
            first++
        }
    }

    println("Part 1: " + numbers[first] * numbers[last])
}

fun part2() {

    val lines = File("day1.txt").readLines()
    val numbers = lines.stream().map(Integer::parseInt).toList()

    numbers.forEach { a ->
        numbers.forEach { b ->
            numbers.forEach { c ->
                if (a + b + c == 2020)
                    println("Part 2: " + a * b * c)
            }
        }
    }
}