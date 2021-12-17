package com.github.sleepyswords.framework

import java.nio.file.Files
import java.nio.file.Path

abstract class Day(val day: Int) {
    abstract fun part1(input: String): String

    abstract fun part2(input: String): String

    fun start() {
        val path = Path.of("inputs/")
        if (!Files.exists(path)) Files.createDirectory(path)
        val day = Path.of("inputs/day${day}.txt")
        if (!Files.exists(day)) Files.createFile(day)
    }

}
fun main() {
    object : Day(1) {
        override fun part1(input: String): String {
            TODO("Not yet implemented")
        }

        override fun part2(input: String): String {
            TODO("Not yet implemented")
        }

    }.start();
}
