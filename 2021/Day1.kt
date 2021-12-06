import java.io.File

typealias TInput1 = List<Int>
typealias TOutput1 = Int

private fun part1(input: TInput1): TOutput1 = input.windowed(2, 1).map { it[1] > it[0] }.count { it }

private fun part2(input: TInput1): TOutput1 = part1(input.windowed(3, 1).map { it.sum() })

private fun parse(raw_input: String): TInput1 = nums(raw_input)

fun main() {
    val rawInput = File("1.in").readText()
    val input = parse(rawInput)

    println("Solution for Part 1: ${part1(input)}")
    println("Solution for Part 2: ${part2(input)}")
}

// Solution part 1: 1121
// Solution part 2: 1065
