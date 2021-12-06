import java.io.File

typealias TInput6 = List<Int>
typealias TOutput6 = Long

private fun calc(input: TInput6, n: Int): TOutput6 {
    val memo = HashMap<Long, Long>();

    fun calcIt(f: Long): Long = memo.getOrPut(f) {
        val fx = 0.toLong().coerceAtLeast(n - f - 1) / 7 + (if (n - f > 0) 1 else 0)
        1 + (1..fx).sumOf { calcIt(f + 2 + it * 7) }
    }

    return input.sumOf { calcIt(it.toLong()) }
}

private fun part1(input: TInput6): TOutput6 = calc(input, 80)

private fun part2(input: TInput6): TOutput6 = calc(input, 256)

fun main() {
    val rawInput = File("6.in").readText()
    val input = nums(rawInput)

    println("Solution for Part 1: ${part1(input)}")
    println("Solution for Part 2: ${part2(input)}")
}

// Solution for Part 1: 355386
// Solution for Part 2: 1613415325809
