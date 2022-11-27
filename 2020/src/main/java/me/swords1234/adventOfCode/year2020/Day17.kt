package me.swords1234.adventOfCode.year2020

import java.io.File
import kotlin.math.abs


fun main() {
    fun part1(input: List<String>): Int {


        val directions = Array((3*3*3) - 1) {
            return@Array if (it == 13) {
                Position(1, 1, 1)
            } else {
                Position((it % 3) - 1, ((it / 3) % 3) - 1, (it / 9) - 1)
            }
        }


        var activeCubes: MutableSet<Position> = mutableSetOf()

        var bottom = Position(Integer.MAX_VALUE, Integer.MAX_VALUE, 0)
        var top = Position(Integer.MIN_VALUE, Integer.MIN_VALUE, 0)

        input.forEachIndexed { y, str ->
            str.toCharArray().forEachIndexed { x, char ->
                if (bottom.x > x) bottom.x = x
                if (bottom.y > y) bottom.y = y

                if (top.x < x) top.x = x
                if (top.y < y) top.y = y
                if (char == '#') activeCubes.add(Position(x, y, 0))
            }
        }

//        val array2 = arrayOf(arrayOf(".", ".", ".", ".", "."), arrayOf(".", ".", ".", ".", "."), arrayOf(".", ".", ".", ".", "."), arrayOf(".", ".", ".", ".", "."))
//
//        activeCubes.stream().filter { it.z == 0 }.forEach {
//            array2[it.y][it.x] = "#"
//        }
//
//        for (arr in array2) {
//            for (char in arr) {
//                print(char)
//            }
//            println()
//        }


        repeat(6) {
            var writeIn = activeCubes.toMutableSet()

            for (x in bottom.x - 1..top.x + 1) {
                for (y in bottom.y - 1..top.y + 1) {
                    for (z in bottom.z - 1..top.z + 1) {
                        var activeAround = 0
                        val checkPosition = Position(x, y, z)
                        for (direction in directions) {
                            if (activeCubes.contains(direction + checkPosition)) {
                                activeAround++
                            }
                        }
                        if (activeCubes.contains(Position(x, y, z))) {
                            // Active
                            if ((activeAround != 3 && activeAround != 2)) {
                                writeIn.remove(checkPosition)
                            }
                        } else {
                            // Inactive
                            if (activeAround == 3) {
                                writeIn.add(checkPosition)
                                if (bottom.x > x) bottom.x = x
                                if (bottom.y > y) bottom.y = y
                                if (bottom.z > z) bottom.z = z

                                if (top.x < x) top.x = x
                                if (top.y < y) top.y = y
                                if (top.z < z) top.z = z
                            }
                        }
                    }
                }
            }

            activeCubes = writeIn
        }

//        val array = arrayOf(
//                arrayOf(".", ".", ".", ".", "."),
//                arrayOf(".", ".", ".", ".", "."),
//                arrayOf(".", ".", ".", ".", "."),
//                arrayOf(".", ".", ".", ".", "."),
//                arrayOf(".", ".", ".", ".", "."),
//                arrayOf(".", ".", ".", ".", "."))
//
//        activeCubes.stream().filter { it.z == 2 }.forEach {
//            array[it.y + 1][it.x + 1] = "#"
//        }
//
//        for (arr in array) {
//            for (char in arr) {
//                print(char)
//            }
//            println()
//        }

        return activeCubes.size
    }

    fun part2(input: List<String>): Int {


        val directions = Array((3*3*3*3) - 1) {
            return@Array if (it == 40) {
                Position4D(1, 1, 1, 1)
            } else {
                Position4D((it % 3) - 1, ((it / 3) % 3) - 1, ((it / 9) % 3) - 1, (it / 27) - 1)
            }
        }

        var activeCubes: MutableSet<Position4D> = mutableSetOf()

        var bottom = Position4D(Integer.MAX_VALUE, Integer.MAX_VALUE, 0, 0)
        var top = Position4D(Integer.MIN_VALUE, Integer.MIN_VALUE, 0, 0)

        input.forEachIndexed { y, str ->
            str.toCharArray().forEachIndexed { x, char ->
                if (bottom.x > x) bottom.x = x
                if (bottom.y > y) bottom.y = y

                if (top.x < x) top.x = x
                if (top.y < y) top.y = y
                if (char == '#') activeCubes.add(Position4D(x, y, 0, 0))
            }
        }


        repeat(6) {
            var writeIn = activeCubes.toMutableSet()

            for (x in bottom.x - 1..top.x + 1) {
                for (y in bottom.y - 1..top.y + 1) {
                    for (z in bottom.z - 1..top.z + 1) {
                        for (w in bottom.w - 1..top.w + 1) {
                            var activeAround = 0
                            val checkPosition = Position4D(x, y, z, w)
                            for (direction in directions) {
                                if (activeCubes.contains(direction + checkPosition)) {
                                    activeAround++
                                }
                            }
                            if (activeCubes.contains(Position4D(x, y, z, w))) {
                                // Active
                                if ((activeAround != 3 && activeAround != 2)) {
                                    writeIn.remove(checkPosition)
                                }
                            } else {
                                // Inactive
                                if (activeAround == 3) {
                                    writeIn.add(checkPosition)
                                    if (bottom.x > x) bottom.x = x
                                    if (bottom.y > y) bottom.y = y
                                    if (bottom.z > z) bottom.z = z
                                    if (bottom.w > w) bottom.w = w

                                    if (top.x < x) top.x = x
                                    if (top.y < y) top.y = y
                                    if (top.z < z) top.z = z
                                    if (top.w < w) top.w = w
                                }
                            }
                        }
                    }
                }
            }

            activeCubes = writeIn
        }

//        val array = arrayOf(
//                arrayOf(".", ".", ".", ".", "."),
//                arrayOf(".", ".", ".", ".", "."),
//                arrayOf(".", ".", ".", ".", "."),
//                arrayOf(".", ".", ".", ".", "."),
//                arrayOf(".", ".", ".", ".", "."),
//                arrayOf(".", ".", ".", ".", "."))
//
//        activeCubes.stream().filter { it.z == 2 }.forEach {
//            array[it.y + 1][it.x + 1] = "#"
//        }
//
//        for (arr in array) {
//            for (char in arr) {
//                print(char)
//            }
//            println()
//        }

        return activeCubes.size
    }

    val lines = File("day17.txt").readLines()
    println("Part 1: " + part1(lines))
    println("Part 2: " + part2(lines))
}

class Position(var x: Int, var y: Int, var z: Int) {

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Position

        if (x != other.x) return false
        if (y != other.y) return false
        if (z != other.z) return false

        return true
    }

    override fun hashCode(): Int {
        var result = x
        result = 31 * result + y
        result = 31 * result + z
        return result
    }

    override fun toString(): String {
        return "{$x,$y,$z}"
    }

    operator fun plus(position: Position): Position {
        return Position(position.x + x, position.y + y, position.z + z)
    }
}
class Position4D(var x: Int, var y: Int, var z: Int, var w: Int) {

    override fun equals(other: Any?): Boolean {
        if (this === other) return true
        if (javaClass != other?.javaClass) return false

        other as Position4D

        if (x != other.x) return false
        if (y != other.y) return false
        if (z != other.z) return false
        if (w != other.w) return false

        return true
    }

    override fun hashCode(): Int {
        var result = x
        result = 31 * result + y
        result = 31 * result + z
        result = 31 * result + w
        return result
    }

    override fun toString(): String {
        return "{$x,$y,$z,$w}"
    }

    operator fun plus(position: Position4D): Position4D {
        return Position4D(position.x + x, position.y + y, position.z + z, position.w + w)
    }
}