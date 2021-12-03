package me.swords1234.adventOfCode.year2020

import java.io.File
import java.lang.StringBuilder


fun main() {
    fun part1(input: List<String>): String {
        var pickedUp = mutableListOf<Int>()
        val cups = mutableListOf<Int>()
        println(input[0].toCharArray().map { it.toString().toInt() })
        cups.addAll(input[0].toCharArray().map { it.toString().toInt() })
        println(cups)

        for ((currentCup, i) in (0 until 10).withIndex()) {
            println("-- move ${currentCup + 1} --")
            val index = currentCup % cups.size
            val oldValue = cups[index]
            var destinationCup = cups[index] - 1

            if (destinationCup == 0) {
                destinationCup = cups.size
            }
            print("cups:")
            cups.forEach {
                if (cups.indexOf(it) == index && index == 0) print(" ($it)")
                else if (cups.indexOf(it) == index) print("($it)")
                else print(" $it ")
            }
            println()

            while (destinationCup in arrayOf(cups[(index + 1) % cups.size], cups[(index + 2) % cups.size], cups[(index + 3) % cups.size])) {
                if (destinationCup == 1) destinationCup = cups.size - 1
                else destinationCup -= 1
            }

            for (i in 1..3) {
                if (index + 1 >= cups.size) pickedUp.add(cups.removeAt(0))
                else pickedUp.add(cups.removeAt((index + 1) % cups.size))
            }
            print("pick up: ")
            pickedUp.forEach {
                print(" $it")
            }
            println()

            pickedUp.reverse()

            pickedUp.forEach {
                cups.add(cups.indexOf(destinationCup) + 1, it)
            }

            for (i in index until cups.indexOf(oldValue)) {
                cups.add(cups.removeAt(0))
            }

            println("destination: $destinationCup")
            println()
            pickedUp.clear()
        }

        for (i in 0 until cups.indexOf(1)) {
            cups.add(cups.removeAt(0))
        }

        cups.remove(1)

        val str = StringBuilder()

        cups.forEach {
            str.append(it)
        }

        return str.toString()

    }


    fun part2(input: List<String>) {
    }

    val lines = File("day23.txt").readLines()
    println("Part 1: " + part1(lines))
    println("Part 2: " + part2(lines))
}