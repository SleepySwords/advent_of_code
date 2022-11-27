package me.swords1234.adventOfCode.year2020

import java.io.File
import java.math.BigInteger


fun main() {

    fun findExpressionV2(_str: String): BigInteger {
        val addMatch = Regex(".*?(\\d+)\\s*\\+\\s*(\\d+).*?")
        val multiplyMatch = Regex(".*?(\\d+)\\s*\\*\\s*(\\d+).*?")


        var str = _str

        while (str.matches(addMatch)) {
            val matches = addMatch.matchEntire(str)!!.destructured.toList()
            str = str.replace(Regex("${matches[0]}\\s*\\+\\s*${matches[1]}"), (matches[0].toBigInteger() + matches[1].toBigInteger()).toString())
        }

        while (str.matches(multiplyMatch)) {
            val matches = multiplyMatch.matchEntire(str)!!.destructured.toList()
            str = str.replace(Regex("${matches[0]}\\s*\\*\\s*${matches[1]}"), (matches[0].toBigInteger() * matches[1].toBigInteger()).toString())

        }

        return str.trim().toBigInteger()
    }

    fun findExpression(str: String): Long {
        val charMatch = Regex("\\s*(.+)\\s*([+*])\\s*(.+)\\s*")
        return if (charMatch.matches(str)) {
            val list = charMatch.matchEntire(str)!!.destructured.toList()
            if (list[1] == "+") {
                findExpression(list[0]) + findExpression(list[2])
            } else {
                findExpression(list[0]) * findExpression(list[2])
            }

        } else {
            str.trim().toLong()
        }
    }


    fun part1(input: List<String>): BigInteger {
        val tokens = input[0].toCharArray()

        var finalValue = BigInteger.ZERO

        input.forEach { _expression ->
            var expression = _expression
            val braket = Regex("\\(([^\\n()]*)\\)")

            while (braket.containsMatchIn(expression)) {
                val iterator = braket.findAll(expression).iterator()

                while (iterator.hasNext()) {
                    val str = iterator.next().destructured.toList()[0]
                    expression = expression.replace("($str)", findExpression(str).toString())
                }
            }

            if (findExpression(expression) < 0) println(_expression)

            finalValue += findExpression(expression).toBigInteger()
        }
        return finalValue
    }

    fun part2(input: List<String>): BigInteger {

        var finalValue = BigInteger.ZERO

        input.forEach { _expression ->
            var expression = _expression
            val bracket = Regex("\\(([^\\n()]*)\\)")

            while (bracket.containsMatchIn(expression)) {
                val iterator = bracket.findAll(expression).iterator()

                while (iterator.hasNext()) {
                    val str = iterator.next().destructured.toList()[0]
                    expression = expression.replace("($str)", findExpressionV2(str).toString())
                }
            }

            if (findExpressionV2(expression) < BigInteger.ZERO) println(_expression)

            finalValue += findExpressionV2(expression)
        }
        return finalValue
    }

    val lines = File("day18.txt").readLines()
    println("Part 1: " + part1(lines))
    println("Part 2: " + part2(lines))
}