package me.swords1234.adventOfCode.year2020

import java.io.File


fun main() {
    Day4().part1()
    Day4().part2()
}

class Day4 {


    fun part1() {


        val lines = File("day4.txt").readLines()

        var fields = 0
        var valid_passports = 0

        lines.forEachIndexed { index, line ->

            if (line.isEmpty()) {
                if (fields == 7) valid_passports++
                fields = 0

            } else {

                fields += (line.split(" ", ":").size / 2) - if (line.contains("cid")) { 1 } else { 0 }
            }
        }
        if (fields == 7) valid_passports++
        println("Part 1: " + valid_passports)
    }

    fun part2() {

        val lines = File("day4.txt").readLines()

        var fields = 0
        var valid_passports = 0

        lines.forEachIndexed { index, line ->

            if (line.isEmpty()) {
                if (fields == 7) valid_passports++
                fields = 0

            } else {

                for (x in line.split(" ", ":").indices step 2) {
                    val key = line.split(" ", ":")[x]
                    val value = line.split(" ", ":")[x + 1]
                    if (key == "byr" && (value.toInt() in 1920..2002)) fields += 1
                    if (key == "iyr" && (value.toInt() in 2010..2020)) fields += 1
                    if (key == "eyr" && (value.toInt() in 2020..2030)) fields += 1
                    if (key == "hgt") {
                        if (value.endsWith("cm") && (value.dropLast(2).toInt() in 150..193)) fields += 1
                        if (value.endsWith("in") && (value.dropLast(2).toInt() in 59..76)) fields += 1
                    }
                    if (key == "hcl" && value.matches(Regex("#([a-fA-F0-9]{6})"))) fields += 1
                    if (key == "ecl" && value.matches(Regex("(amb|blu|brn|gry|grn|hzl|oth)"))) fields += 1
                    if (key == "pid" && value.matches(Regex("\\d{9}"))) fields += 1
                }
            }
        }
        if (fields == 7) valid_passports++
        println("Part 2: " + valid_passports)
    }

//    fun part2() {
//
//        val lines = File("day3.txt").readLines()
//
//        val max_slope: Int = 7
//        val height: Int = lines.size
//        val min_width = max_slope * height
//
//        val trees: Array<Array<Boolean>> = Array(min_width) { x ->
//            return@Array Array(height) { y ->
//                lines[y][x % lines[0].length] == '#'
//            }
//        }
//
//        val slopes: Array<Double> = arrayOf(1.0, 3.0, 5.0, 7.0, 0.5)
//
//        var treesCollided = 1L
//
//        for (slope in slopes) {
//            var treeCollidedPerSlope = 0
//            var y = 0
//            while (y != height) {
//
//                if ((y * slope) % 1 != 0.0) {y++; continue}
//
//                if (trees[(y * slope).toInt()][y]) treeCollidedPerSlope++
//
//                y++
//            }
//            treesCollided *= treeCollidedPerSlope
//        }
//        println(treesCollided)
//    }
//
//    fun part1() {
//
//        val lines = File("day4.txt").readLines()
//
//        val passports = mutableListOf<MutableMap<String, String>>()
//
//        var passport_index = 0
//
//        for (line in lines) {
//            if (line == "\n") { passport_index++; continue; }
//
//            if (passports[passport_index] == null) {
//                passports[passport_index] = mutableMapOf<String, String>()
//            }
//        }
//
//        val slope: Int = 1
//        val height: Int = lines.size
//        val min_width = slope * height
//
//        val trees: Array<Array<Boolean>> = Array(min_width) { x ->
//            return@Array Array(height) { y ->
//                lines[y][x % lines[0].length] == '#'
//            }
//        }
//
//        var y = 0
//        var trees_collided = 0
//
//        while (y != height) {
//            if (trees[y * slope][y]) trees_collided++
//
//            y++
//        }
//        println(trees_collided)
//    }
//
//    fun part2() {
//
//        val lines = File("day3.txt").readLines()
//
//        val max_slope: Int = 7
//        val height: Int = lines.size
//        val min_width = max_slope * height
//
//        val trees: Array<Array<Boolean>> = Array(min_width) { x ->
//            return@Array Array(height) { y ->
//                lines[y][x % lines[0].length] == '#'
//            }
//        }
//
//        val slopes: Array<Double> = arrayOf(1.0, 3.0, 5.0, 7.0, 0.5)
//
//        var treesCollided = 1L
//
//        for (slope in slopes) {
//            var treeCollidedPerSlope = 0
//            var y = 0
//            while (y != height) {
//
//                if ((y * slope) % 1 != 0.0) {y++; continue}
//
//                if (trees[(y * slope).toInt()][y]) treeCollidedPerSlope++
//
//                y++
//            }
//            treesCollided *= treeCollidedPerSlope
//        }
//        println(treesCollided)
//    }
}