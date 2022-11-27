package me.swords1234.adventOfCode.year2020

import java.io.File


fun main() {
    val map = mutableMapOf<Int, String>()

    // Returns, Substring and boolean
    fun matchesRule(input: String, reg: String): Pair<Boolean, Int> {
        when {
            reg.contains("|") -> {
                reg.split(" | ").forEach {
                    if (matchesRule(input, it).first) return matchesRule(input, it)
                }
            }
            reg.matches(Regex("\".+\"")) -> {
                val chars = Regex("\"(.+)\"").find(reg)!!.destructured.toList()[0]
                if (input.startsWith(chars)) return Pair(true, chars.length)
                return Pair(false, 0)
            }
            else -> {
                var substring = 0
                reg.split(" ").forEach {
                    val (result, additionalSubstring)
                            = matchesRule(input.substring(substring), map[it.toInt()]!!)
                    substring += additionalSubstring
                    if (!result) {
                        return Pair(false, 0)
                    }
                }
                return Pair(true, substring)
            }
        }

        return Pair(false, 0)
    }
    fun part1(input: List<String>): Long {
        val regex = Regex("(\\d+): (.+)")
        for (i in 0..input.indexOf("")) {
            if (!input[i].matches(regex)) continue
            val list = regex.matchEntire(input[i])!!.destructured.toList()
            map[list[0].toInt()] = list[1]
        }

        return input.stream().filter {
            val (result, substring) = matchesRule(it, map[0]!!)
            return@filter (result && it.length == substring)
        }.count()
    }

    fun part2(input: List<String>): Long {
        val regex = Regex("(\\d+): (.+)")
        for (i in 0..input.indexOf("")) {
            if (!input[i].matches(regex)) continue
            val list = regex.matchEntire(input[i])!!.destructured.toList()
            if (list[0].toInt() == 8) {
                map[list[0].toInt()] = "42 | 42 8"
                continue
            }
            if (list[0].toInt() == 11) {
                map[list[0].toInt()] = "42 31 | 42 11 31"
                continue
            }
            map[list[0].toInt()] = list[1]
        }

        input.stream().filter {
            val (result, substring) = matchesRule(it, map[0]!!)
            return@filter (result && it.length == substring)
        }.forEach {
            println(it)
        }

        return input.stream().filter {
            val (result, substring) = matchesRule(it, map[0]!!)
            return@filter (result && it.length == substring)
        }.count()
    }

    val lines = File("day19.txt").readLines()
    println("Prt 1: " + part1(lines))
    println("Part 2: " + part2(lines))
}
