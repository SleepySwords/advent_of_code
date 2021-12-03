package me.swords1234.adventOfCode.year2020

import java.io.File


fun main() {
    Day3().part1()
    Day3().part2()
}

class Day3 {
    fun part1() {

        val lines = File("day3.txt").readLines()

        val slope: Int = 1
        val height: Int = lines.size
        val min_width = slope * height

        val trees: Array<Array<Boolean>> = Array(min_width) { x ->
            return@Array Array(height) { y ->
                lines[y][x % lines[0].length] == '#'
            }
        }

        var y = 0
        var trees_collided = 0

        while (y != height) {
            if (trees[y * slope][y]) trees_collided++

            y++
        }
        println("Part 1: " + trees_collided)
    }

    fun part2() {

        val lines = File("day3.txt").readLines()

        val max_slope: Int = 7
        val height: Int = lines.size
        val min_width = max_slope * height

        val trees: Array<Array<Boolean>> = Array(min_width) { x ->
            return@Array Array(height) { y ->
                lines[y][x % lines[0].length] == '#'
            }
        }

        val slopes: Array<Double> = arrayOf(1.0, 3.0, 5.0, 7.0, 0.5)

        var treesCollided = 1L

        for (slope in slopes) {
            var treeCollidedPerSlope = 0
            var y = 0
            while (y != height) {

                if ((y * slope) % 1 != 0.0) {y++; continue}

                if (trees[(y * slope).toInt()][y]) treeCollidedPerSlope++

                y++
            }
            treesCollided *= treeCollidedPerSlope
        }
        println("Part 2: " + treesCollided)
    }
}