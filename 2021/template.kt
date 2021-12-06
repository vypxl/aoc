import java.io.File

typealias TInput_DAY_ = List<Int>
typealias TOutput_DAY_ = Int

private fun part1(input: TInput_DAY_): TOutput_DAY_ = 0

private fun part2(input: TInput_DAY_): TOutput_DAY_ = 1

private fun parse(raw_input: String): TInput_DAY_ = nums(raw_input)

fun main() {
    val rawInput = File("_DAY_.in").readText()
    val input = parse(rawInput)

    println("Solution for Part 1: ${part1(input)}")
    println("Solution for Part 2: ${part2(input)}")
}

// Solution part 1:
// Solution part 2:
