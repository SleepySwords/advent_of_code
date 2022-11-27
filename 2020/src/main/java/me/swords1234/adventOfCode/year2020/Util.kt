package me.swords1234.adventOfCode.year2020

import java.time.Instant

object Util {
    fun <T> bench(block: () -> T, callback: (String) -> Unit): T {
        val pre = Instant.now().toEpochMilli()
        val result = block()
        val post = Instant.now().toEpochMilli()
        callback("${post - pre}ms")
        return result
    }
}