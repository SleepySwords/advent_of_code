package me.swords1234.adventOfCode.year2020

import java.io.File
import java.lang.IllegalArgumentException
import kotlin.math.abs
import kotlin.math.sign


fun main() {
    fun part1(input: List<String>): Int {
        var x = 0
        var y = 0
        var direction = Direction.EAST
        input.forEach {str ->
            if (!Regex("(\\w)(\\d*)").matches(str)) throw IllegalArgumentException("Invalid input: $str")
            val dir = Regex("(\\w)(\\d*)").matchEntire(str)!!.destructured!!.toList()!!.get(0);
            val value = Regex("(\\w)(\\d*)").matchEntire(str)!!.destructured!!.toList()!!.get(1)!!.toInt();

            when (dir) {
                "N" -> y += value
                "S" -> y -= value
                "E" -> x += value
                "W" -> x -= value
            }

            if (dir == "R") {
                direction = Direction.values()[((value / 90) + direction.ordinal) % Direction.values().size]
            }

            if (dir == "L") {
                direction = Direction.values()[abs(((Direction.values().size - ((value / 90)) + direction.ordinal)) % Direction.values().size)]
            }

            if (dir == "F") {
                when (direction) {
                    Direction.NORTH -> y += value
                    Direction.SOUTH -> y -= value
                    Direction.EAST -> x += value
                    Direction.WEST -> x -= value
                }
            }

            println("X: $x Y: $y Direction: $direction")
        }

        return abs(x) + abs(y)
    }

    fun part2(input: List<String>): Int {

        var xWaypoint = 10
        var yWaypoint = 1
        var x = 0
        var y = 0
        input.forEach {str ->
            if (!Regex("(\\w)(\\d*)").matches(str)) throw IllegalArgumentException("Invalid input: $str")
            val dir = Regex("(\\w)(\\d*)").matchEntire(str)!!.destructured!!.toList()!!.get(0)
            val value = Regex("(\\w)(\\d*)").matchEntire(str)!!.destructured!!.toList()!!.get(1)!!.toInt()

            when (dir) {
                "N" -> yWaypoint += value
                "S" -> yWaypoint -= value
                "E" -> xWaypoint += value
                "W" -> xWaypoint -= value
            }

            if (dir == "R") {

                val xSign = sign(xWaypoint.toDouble())
                val ySign = sign(yWaypoint.toDouble())


                when (value) {
                    90 -> {
                        val oldY = yWaypoint
                        yWaypoint = -xWaypoint
                        xWaypoint = oldY
                    }
                    180 -> {
                        xWaypoint = -xWaypoint
                        yWaypoint = -yWaypoint
                    }
                    270 -> {

                        val oldX = xWaypoint
                        xWaypoint = -yWaypoint
                        yWaypoint = oldX
                    }
                }
            }

            if (dir == "L") {

                val xSign = sign(xWaypoint.toDouble())
                val ySign = sign(yWaypoint.toDouble())


                when (value) {
                    90 -> {
                        val oldX = xWaypoint
                        xWaypoint = -yWaypoint
                        yWaypoint = oldX
                    }
                    180 -> {
                        xWaypoint = -xWaypoint
                        yWaypoint = -yWaypoint
                    }
                    270 -> {

                        val oldY = yWaypoint
                        yWaypoint = -xWaypoint
                        xWaypoint = oldY
                    }
                }
            }


            if (dir == "F") {
                x += xWaypoint * value
                y += yWaypoint * value
            }

            println("X: $x Y: $y Waypoints: $xWaypoint $yWaypoint")
        }

        return abs(x) + abs(y)
    }

    val lines = File("day12.txt").readLines()
    println("Part 1: " + part1(lines))
    println("Part 2: " + part2(lines))
}

enum class Direction {
    NORTH, EAST, SOUTH, WEST
}