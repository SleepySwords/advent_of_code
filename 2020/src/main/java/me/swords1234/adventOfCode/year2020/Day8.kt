package me.swords1234.adventOfCode.year2020

import java.io.File


fun main() {
    fun part1(input: List<String>) = AssembyExecutor(createInstructions(input)).findLoopCode()

    fun part2(input: List<String>): Int {

        AssembyExecutor(createInstructions(input)).findNOPCandidates().forEach { modified ->
            val instructions = createInstructions(input)
            instructions[modified].type = AssembyExecutor.Instruction.Type.JMP
            val executor = AssembyExecutor(instructions)

            if (!executor.ifCodeLoops()) return executor.accumalator
        }

        AssembyExecutor(createInstructions(input)).findJMPCandidates().forEach { modified ->
            val instructions = createInstructions(input)
            instructions[modified].type = AssembyExecutor.Instruction.Type.NOP
            val executor = AssembyExecutor(instructions)

            if (!executor.ifCodeLoops()) return executor.accumalator
        }

        return -1
    }

    val lines = File("day8.txt").readLines()
    println("Part 1: " + part1(lines))
    println("Part 2: " + part2(lines))
}

fun createInstructions(string: List<String>): Array<AssembyExecutor.Instruction> {

    return Array(string.size) {
        val values = Regex("(.+) (.)(\\d+)").findAll(string[it]).toList()[0].destructured.toList()
        val type = AssembyExecutor.Instruction.Type.valueOf(values[0].toUpperCase())
        val value = values[2].toInt() * if (values[1] == "+") { 1 } else { -1 }
        return@Array AssembyExecutor.Instruction(type, value)
    }
}

class AssembyExecutor(val instructions: Array<Instruction>) {
    var accumalator: Int = 0
    var instructionPTR: Int = 0

    val executedInstructions: MutableSet<Int> = mutableSetOf()

    fun execute() {
        while (instructionPTR <= instructions.size) {

            if (!executedInstructions.contains(instructionPTR)) executedInstructions.add(instructionPTR)
            else {
                println("$accumalator")
                break
            }

            val instruction = instructions[instructionPTR]
            if (instruction.type == Instruction.Type.JMP) {
                instructionPTR += instruction.value
                continue
            }
            if (instruction.type == Instruction.Type.ACC) {
                accumalator += instruction.value
            }

            instructionPTR++

        }
    }

    fun reset() {
        accumalator = 0
        instructionPTR = 0
        executedInstructions.clear()
    }

    fun findNOPCandidates(): MutableList<Int> {

        val candidates: MutableList<Int> = mutableListOf()

        findLoopCode()
        executedInstructions.forEach {ptr ->
            if (instructions[ptr].type == Instruction.Type.NOP && !executedInstructions.contains(ptr + instructions[ptr].value)) {
                candidates.add(ptr)
            }

        }
        return candidates
    }

    fun findJMPCandidates(): MutableList<Int> {

        val candidates: MutableList<Int> = mutableListOf()

        findLoopCode()
        executedInstructions.forEach {ptr ->
            if (instructions[ptr].type == Instruction.Type.JMP && !executedInstructions.contains(ptr + 1)) {
                candidates.add(ptr)
            }

        }
        return candidates
    }

    fun findLoopCode(): Int {
        return if (ifCodeLoops()) {
            accumalator
        } else {
            -1
        }
    }

    fun ifCodeLoops(): Boolean {
        while (instructionPTR < instructions.size) {

            if (!executedInstructions.contains(instructionPTR)) executedInstructions.add(instructionPTR)
            else {
                return true
            }

            val instruction = instructions[instructionPTR]
            if (instruction.type == Instruction.Type.JMP) {
                instructionPTR += instruction.value
                continue
            }
            if (instruction.type == Instruction.Type.ACC) {
                accumalator += instruction.value
            }

            instructionPTR++
        }
        return false
    }

    class Instruction(var type: Type, val value: Int) {

        enum class Type {
            ACC, NOP, JMP
        }

        override fun toString(): String = "$type $value"
    }
}