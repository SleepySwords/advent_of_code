package me.swords1234.adventOfCode.year2020

import java.io.File
import java.lang.StringBuilder


fun main() {
    fun part1(input: List<String>): List<Long> {

        val amountOfSharedBorders = mutableMapOf<Long, Long>()
        val borderToIds = mutableMapOf<String, MutableList<Long>>()


        var first = ""
        var id = 0L
        var left = StringBuilder()
        var right = StringBuilder()

        input.forEachIndexed { i, line ->

            if (line.matches(Regex("Tile (.+):"))) {
                id = Regex("Tile (.+):").find(line)!!.destructured.toList()[0].toLong()
                first = input[i + 1]
            } else if (line.isNotEmpty()) {
                left.append(line.first())
                right.append(line.last())
            }

            if (input.size == i + 1 || input[i + 1].isEmpty()) {
                val last = input[i]
                amountOfSharedBorders[id] = 0
                // logic

                for (border in arrayOf(left.toString(), right.toString(), first, last)) {
                    when {
                        borderToIds.contains(border) -> {
                            borderToIds[border]!!.add(id)
                            for (ids in borderToIds[border]!!) {
                                amountOfSharedBorders[ids] = amountOfSharedBorders[ids]!! + 1
                            }
                        }
                        borderToIds.contains(border.reversed()) -> {
                            borderToIds[border.reversed()]!!.add(id)
                            for (ids in borderToIds[border.reversed()]!!) {
                                amountOfSharedBorders[ids] = amountOfSharedBorders[ids]!! + 1
                            }
                        }
                        else -> {
                            borderToIds[border] = mutableListOf(id)
                        }
                    }
                }

                left = StringBuilder()
                right = StringBuilder()
            }
        }

        println(amountOfSharedBorders)

        return amountOfSharedBorders.filter { it.value == 2L }.map { it.key }

    }

    fun part2(input: List<String>): List<Long> {

        val amountOfSharedBorders = mutableMapOf<Long, Long>()
        val borderToIds = mutableMapOf<String, MutableList<Long>>()


        var first = ""
        var id = 0L
        var left = StringBuilder()
        var right = StringBuilder()

        input.forEachIndexed { i, line ->

            if (line.matches(Regex("Tile (.+):"))) {
                id = Regex("Tile (.+):").find(line)!!.destructured.toList()[0].toLong()
                first = input[i + 1]
            } else if (line.isNotEmpty()) {
                left.append(line.first())
                right.append(line.last())
            }

            if (input.size == i + 1 || input[i + 1].isEmpty()) {
                val last = input[i]
                amountOfSharedBorders[id] = 0
                // logic

                for (border in arrayOf(left.toString(), right.toString(), first, last)) {
                    when {
                        borderToIds.contains(border) -> {
                            borderToIds[border]!!.add(id)
                            for (ids in borderToIds[border]!!) {
                                amountOfSharedBorders[ids] = amountOfSharedBorders[ids]!! + 1
                            }
                        }
                        borderToIds.contains(border.reversed()) -> {
                            borderToIds[border.reversed()]!!.add(id)
                            for (ids in borderToIds[border.reversed()]!!) {
                                amountOfSharedBorders[ids] = amountOfSharedBorders[ids]!! + 1
                            }
                        }
                        else -> {
                            borderToIds[border] = mutableListOf(id)
                        }
                    }
                }

                left = StringBuilder()
                right = StringBuilder()
            }
        }

        println(amountOfSharedBorders)

        return amountOfSharedBorders.filter { it.value == 2L }.map { it.key }
    }

    val lines = File("day20.txt").readLines()
    println("Part 1: " + part1(lines).stream().reduce(1) { a,b -> a * b })
    println("Part 2: " + part2(lines))
}