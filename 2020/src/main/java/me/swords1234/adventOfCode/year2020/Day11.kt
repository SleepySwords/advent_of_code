package me.swords1234.adventOfCode.year2020

import java.io.File



fun isInBounds(x: Int, y: Int, array: Array<Array<State>>): Boolean {
    return (y >= 0 && y < array[0].size) && (x >= 0 && x < array.size)
}

fun printing(array: Array<Array<State>>) {
    for (x in array.indices) {
        for (y in array[0].indices) {
            if (array[x][y] == State.OCCUPIED) print("#")
            if (array[x][y] == State.EMPTY) print("L")
            if (array[x][y] == State.FLOOR) print(".")
        }
        println()
    }
}

fun run(array: Array<Array<State>>): Array<Array<State>> {

    val modifiable = Array(array.size) {
        array[it].copyOf()
    }


    for (x in modifiable.indices) {
        for (y in modifiable[0].indices) {

            var occupied = 0

            for (delta_x in -1..1) {
                for (delta_y in -1..1) {

                    if (delta_x == 0 && delta_y == 0) continue

                    if (isInBounds(x + delta_x, y + delta_y, array)) {
                        if (array[x + delta_x][y + delta_y] == State.OCCUPIED) occupied++
                    }
                }
            }


            when(array[x][y]) {
                State.OCCUPIED -> {
                    if (occupied >= 4) modifiable[x][y] = State.EMPTY
                }
                State.EMPTY -> {
                    if (occupied == 0) modifiable[x][y] = State.OCCUPIED
                }
                else -> continue
            }

        }
    }

    return modifiable
}

fun findClosestSeat(xInit: Int, yInit: Int, xGradient: Int, yGradient: Int, array: Array<Array<State>>): State {
    var x = xInit + xGradient
    var y = yInit + yGradient

    while (isInBounds(x, y, array)) {
        if (array[x][y] != State.FLOOR) return array[x][y]
        x += xGradient
        y += yGradient
    }
    return State.FLOOR
}

fun runPart2(array: Array<Array<State>>): Array<Array<State>> {

    val modifiable = Array(array.size) {
        array[it].copyOf()
    }


    for (x in modifiable.indices) {
        for (y in modifiable[0].indices) {

            var occupied = 0

            for (delta_x in -1..1) {
                for (delta_y in -1..1) {

                    if (delta_x == 0 && delta_y == 0) continue

                    if (isInBounds(x + delta_x, y + delta_y, array)) {
                        if (findClosestSeat(x, y, delta_x, delta_y, array) == State.OCCUPIED) occupied++
                    }
                }
            }


            when(array[x][y]) {
                State.OCCUPIED -> {
                    if (occupied >= 5) modifiable[x][y] = State.EMPTY
                }
                State.EMPTY -> {
                    if (occupied == 0) modifiable[x][y] = State.OCCUPIED
                }
                else -> continue
            }

        }
    }

    return modifiable
}

fun main() {

    fun part1(input: List<String>): Long {
        var array: Array<Array<State>> = Array(input.size) { x ->
            return@Array Array(input[0].length) {y ->
                when {
                    input[x].toCharArray()[y] == '.' -> State.FLOOR
                    input[x].toCharArray()[y] == '#' -> State.OCCUPIED
                    else -> State.EMPTY
                }
            }
        }

        println(array[4].contentToString())
        println(array[4][1])
        while (true) {
            val newArray = run(array);
            if (newArray.contentDeepEquals(array)) {
                return array.flatten().count { it == State.OCCUPIED }.toLong()
            }
            array = newArray
            printing(array)
            println()
        }
        return -1
    }

    fun part2(input: List<String>): Long {
        var array: Array<Array<State>> = Array(input.size) { x ->
            return@Array Array(input[0].length) {y ->
                when {
                    input[x].toCharArray()[y] == '.' -> State.FLOOR
                    input[x].toCharArray()[y] == '#' -> State.OCCUPIED
                    else -> State.EMPTY
                }
            }
        }

        println(array[4].contentToString())
        println(array[4][1])
        while (true) {
            val newArray = runPart2(array);
            if (newArray.contentDeepEquals(array)) {
                return array.flatten().count { it == State.OCCUPIED }.toLong()
            }
            array = newArray
            printing(array)
            println()
        }
        return -1
    }

    val lines = File("day11.txt").readLines()
    println("Part 1: " + part1(lines))
    println("Part 2: " + part2(lines))
}

enum class State {
    FLOOR, OCCUPIED, EMPTY
}