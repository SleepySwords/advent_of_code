package com.github.sleepyswords.framework

import java.net.URI
import java.net.http.HttpClient
import java.net.http.HttpRequest
import java.net.http.HttpResponse
import java.nio.file.Files
import java.nio.file.Path

abstract class Day(val day: Int) {
    abstract fun part1(input: String): String

    abstract fun part2(input: String): String

    fun readInput(session: String): String {
        val path = Path.of("inputs/")
        if (!Files.exists(path)) Files.createDirectory(path)
        val dayPath = Path.of("inputs/day${day}.txt")
        if (!Files.exists(dayPath)) {
            Files.createFile(dayPath)
            val client = HttpClient.newBuilder().build()
            val request = HttpRequest.newBuilder()
                .uri(URI.create("https://adventofcode.com/2021/day/${day}/input"))
                .header("cookie", "session=$session")
                .build()
            println("eafeakf")
            val input = client.send(request, HttpResponse.BodyHandlers.ofString()).body();
            Files.writeString(dayPath, input)
            return input.trimEnd()
        }
        return Files.readString(dayPath).trimEnd()
    }

    fun run(session: String) {
        println("Part 1: ${part1(readInput(session))}")
        println("Part 2: ${part2(readInput(session))}")
    }
}
fun main() {
    object : Day(1) {
        override fun part1(input: String): String {
            return "Not Implemented"
        }

        override fun part2(input: String): String {
            return "Not Implemented"
        }

    }.run(System.getenv("AOC_SESSION"));
}
