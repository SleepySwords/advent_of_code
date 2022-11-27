package com.github.sleepyswords.framework

import java.nio.file.Path

object Utils {
    fun getInput(day: Int) {
        Path.of("inputs/${day}.txt")
    }
}