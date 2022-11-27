package me.swords1234.adventOfCode.year2020

import java.io.File


fun main() {

    // Returns true if player 1 wins

    fun recursiveCombat(player1Deck: MutableList<Int>, player2Deck: MutableList<Int>): Triple<Boolean, MutableList<Int>, MutableList<Int>> {
        val hashset = mutableSetOf<Pair<List<Int>, List<Int>>>()


        while (!(player1Deck.size == 0 || player2Deck.size == 0)) {
            if (hashset.contains(Pair(player1Deck.toList(), player2Deck.toList()))) return Triple(true, player1Deck, player2Deck)
            else hashset.add(Pair(player1Deck.toList(), player2Deck.toList()))

            if (player1Deck[0] < player1Deck.size && player2Deck[0] < player2Deck.size) {
//                println("Playing Subgame")
                if (recursiveCombat(player1Deck.toMutableList().subList(1, player1Deck[0] + 1), player2Deck.toMutableList().subList(1, player2Deck[0] + 1)).first) {
                    player1Deck.add(player1Deck.removeFirst())
                    player1Deck.add(player2Deck.removeFirst())
                } else {
                    player2Deck.add(player2Deck.removeFirst())
                    player2Deck.add(player1Deck.removeFirst())
                }
//                println("Exiting Subgame")
                continue
            }

            if (player1Deck[0] > player2Deck[0]) {
                player1Deck.add(player1Deck.removeFirst())
                player1Deck.add(player2Deck.removeFirst())
            } else if (player2Deck[0] > player1Deck[0]) {
                player2Deck.add(player2Deck.removeFirst())
                player2Deck.add(player1Deck.removeFirst())
            }

//            println("Player1: $player1Deck")
//            println("Player2: $player2Deck")
//            println()
        }
        return Triple(player2Deck.size == 0, player1Deck, player2Deck)
    }

    fun part1(input: List<String>): Long {
        val player1Deck = mutableListOf<Int>()
        val player2Deck = mutableListOf<Int>()
        for (line in 0 until input.indexOf("")) {
            if (!Regex("Player \\d:").matches(input[line])) player1Deck.add(input[line].toInt())
        }
        for (line in input.indexOf("") + 1 until input.size) {
            if (!Regex("Player \\d:").matches(input[line])) player2Deck.add(input[line].toInt())
        }

        println(player1Deck)
        println(player2Deck)

        while (!(player1Deck.size == 0 || player2Deck.size == 0)) {
            if (player1Deck[0] > player2Deck[0]) {
                player1Deck.add(player1Deck.removeFirst())
                player1Deck.add(player2Deck.removeFirst())
            } else if (player2Deck[0] > player1Deck[0]) {
                player2Deck.add(player2Deck.removeFirst())
                player2Deck.add(player1Deck.removeFirst())
            }

            println(player1Deck)
            println(player2Deck)
        }

        println(player1Deck)
        println(player2Deck)
        return if (player1Deck.size == 0) {
            var number = 0L
            for (i in 0 until player2Deck.size) {
                number += player2Deck[i] * (player2Deck.size - i)
            }

            number
        }else {
            var number = 0L
            for (i in 0 until player1Deck.size) {
                number += player1Deck[i] * (player1Deck.size - i)
            }
            number
        }
    }

    fun part2(input: List<String>): Long {
        val player1Decks = mutableListOf<Int>()
        val player2Decks = mutableListOf<Int>()
        for (line in 0 until input.indexOf("")) {
            if (!Regex("Player \\d:").matches(input[line])) player1Decks.add(input[line].toInt())
        }
        for (line in input.indexOf("") + 1 until input.size) {
            if (!Regex("Player \\d:").matches(input[line])) player2Decks.add(input[line].toInt())
        }
        val (result, player1Deck, player2Deck) = (recursiveCombat(player1Decks, player2Decks))
        return if (!result) {
            var number = 0L
            for (i in 0 until player2Deck.size) {
                number += player2Deck[i] * (player2Deck.size - i)
            }

            number
        }else {
            var number = 0L
            for (i in 0 until player1Deck.size) {
                number += player1Deck[i] * (player1Deck.size - i)
            }
            number
        }
    }

    val lines = File("day22.txt").readLines()
    println("Part 1: " + part1(lines))
    println("Part 2: " + part2(lines))
}