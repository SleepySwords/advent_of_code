package com.github.sleepyswords

import java.lang.Integer.min
import kotlin.math.absoluteValue
import kotlin.math.max
import kotlin.math.sign

class Day17 {
    class Projectile(var position: Pair<Int, Int>, var velocity: Pair<Int, Int>) {
        fun updatePosition() {
            position = Pair(position.first + velocity.first, position.second + velocity.second)
            velocity = Pair(velocity.first - velocity.first.sign, velocity.second - 1)
        }

        fun isHitBox(pos1: Pair<Int, Int>, pos2: Pair<Int, Int>): Boolean {
            val maxX = max(pos1.first, pos2.first)
            val minX = min(pos1.first, pos2.first)
            val maxY = max(pos1.second, pos2.second)
            val minY = min(pos1.second, pos2.second)
            if (position.first in minX..maxX) {
                if (position.second in minY..maxY) {
                    return true
                }
            }

            if (position.second < minY && velocity.second <= 0) {
                return false
            }

            updatePosition()
            return isHitBox(pos1, pos2)
        }
    }

    fun part1(): Int {
        val pos1 = Pair(85, -163)
        val pos2 = Pair(145, -108)
        var height = Int.MIN_VALUE
        val maxX = max(pos1.first, pos2.first)
        val maxY = max(pos1.second, pos2.second)
        val min_x_p = if (maxX.sign == 1) { 1 } else { maxX }
        val min_y_p = if (maxY.sign == 1) { 1 } else { maxY }
        val max_x_p = if (maxX.sign == 1) { maxX } else { 1 }
        val max_y_p = if (maxY.sign == 1) { maxY } else { 1 }
        for (p_y in (min_y_p..max_y_p + 500)) {
            for (p_x in (min_x_p..max_x_p)) {
                if (Projectile(Pair(0, 0), Pair(p_x, p_y)).isHitBox(Pair(85, -163), Pair(145, -108))) {
                    if (height < (p_y * (p_y + 1) / 2)) {
                        height = (p_y * (p_y + 1) / 2)
                    }
                }
            }
        }
        return height
    }

    fun part2(): Int{
        val pos1 = Pair(85, -163)
        val pos2 = Pair(145, -108)
        var distinct = 0
        val maxX = max(pos1.first, pos2.first)
        val maxY = max(pos1.second, pos2.second)
        val minY = min(pos1.second, pos2.second)
        for (p_y in (minY..maxY.absoluteValue + 500)) {
            for (p_x in (1..maxX)) {
                if (Projectile(Pair(0, 0), Pair(p_x, p_y)).isHitBox(pos1, pos2)) {
                    distinct++
                }
            }
        }
        return distinct
    }
}

fun main() {
    println("Part 1: ${Day17().part1()}")
    println("Part 2: ${Day17().part2()}")
}