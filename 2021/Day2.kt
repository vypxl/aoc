import java.io.File

data class Command(val kind: String, val x: Int)

typealias TInput2 = List<Command>
typealias TOutput2 = Int

private fun part1(input: TInput2): TOutput2 =
    input.fold(Pair(0, 0)) { (d, p), (kind, x) -> when(kind) {
        "forward" -> Pair(d, p + x)
        "up" -> Pair(d - x, p)
        "down" -> Pair(d + x, p)
        else -> Pair(d, p)
    } }.let { (d, p) -> d*p }

private fun part2(input: TInput2): TOutput2 =
    input.fold(Triple(0, 0, 0)) { (d, p, a), (kind, x) -> when(kind) {
        "forward" -> Triple(d + a * x, p + x, a)
        "up" -> Triple(d, p, a - x)
        "down" -> Triple(d, p, a + x)
        else -> Triple(d, p, a)
    } }.let { (d, p, _) -> d*p }

private fun parse(raw_input: String): TInput2 =
    lines(raw_input).map { val xs = it.split(" "); Command(xs[0], xs[1].toInt()) }

fun main() {
    val rawInput = File("2.in").readText()
    val input = parse(rawInput)

    println("Solution for Part 1: ${part1(input)}")
    println("Solution for Part 2: ${part2(input)}")
}

// Solution part 1: 1804520
// Solution part 2: 1971095320
