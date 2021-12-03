package me.swords1234.adventOfCode.year2020

import java.io.File
import java.lang.Math.pow
import kotlin.experimental.and
import kotlin.math.ceil
import kotlin.math.pow


@ExperimentalUnsignedTypes
fun main() {
    fun part1(input: List<String>): ULong {

        var mask = ""
        val memory = Array<ULong>(100000) { 0u }

        input.forEach {instruction ->
            if (instruction.contains("mask = ")) {
                mask = instruction.drop(7)
            } else {

                val address = Regex("mem\\[(\\d*)] = (\\d*)").matchEntire(instruction)!!.destructured.toList()[0].toInt()
                var value = Regex("mem\\[(\\d*)] = (\\d*)").matchEntire(instruction)!!.destructured.toList()[1].toULong()

                value = value or mask.replace("X", "0").toULong(2)
                value = value and mask.replace("X", "1").toULong(2)
                memory[address] = value
            }
        }
        return memory.sum()
    }

    fun part2(input: List<String>): ULong {

        var mask = ""
        val memory = mutableMapOf<ULong, ULong>()

        input.forEach {instruction ->
            if (instruction.contains("mask = ")) {
                mask = instruction.drop(7)
            } else {

                var address = Regex("mem\\[(\\d*)] = (\\d*)").matchEntire(instruction)!!.destructured.toList()[0].toULong()
                val value = Regex("mem\\[(\\d*)] = (\\d*)").matchEntire(instruction)!!.destructured.toList()[1].toULong()

                println("Address: ${StringBuilder(address.toString(2)).padStart(36, '0') as StringBuilder}")
                println("Mask   : $mask")

                val addr = (StringBuilder(address.toString(2)).padStart(36, '0') as StringBuilder).also {
                    mask.forEachIndexed {i, chr ->
                        if (chr == '1' || chr == 'X') it.setCharAt(i, chr)
                    }
                }.toString()

                println("Moded  : $addr")

                for (x in 0 until 2.0.pow(mask.count { it == 'X' }.toDouble()).toInt()) {
                    var tempMask = addr.replace('1', '0')

                    Integer.toBinaryString(x).padStart(mask.count { it == 'X' }, '0').toCharArray().forEach {chr ->
                        tempMask = tempMask.replaceFirst('X', chr)
                    }

                    val ad = addr.replace('X', '0').toULong(2) or tempMask.toULong(2)

                    println("Addr-$x : ${ad.toString(2).padStart(36)}")
                    memory[ad] = value
                }
            }
        }
        return memory.values.sum()
    }

    val lines = File("day14.txt").readLines()
    println("Part 1: " + part1(lines))
    println("Part 2: " + part2(lines))
}