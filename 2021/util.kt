fun nums(s: String, by: Regex = Regex("-?\\d+")) = by.findAll(s).map { it.value.toInt() }.toList()
fun lines(s: String) = s.lines().filter { it.isNotEmpty() }
fun lines_nums(s: String) = lines(s).map { nums(it) }

fun tr(s: String, a: String, b: String) = s.map { val i = a.indexOf(it); if (i != -1) b[i] else it }.joinToString("")
