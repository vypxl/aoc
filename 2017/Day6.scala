import scala.collection.mutable

val in = "10	3	15	10	5	15	5	15	9	2	5	8	5	2	3	6"
//val in = "0 2 7 0"
val banks = in.split("\\s+").map(_.toInt).toArray

val seen = mutable.ArrayBuffer[List[Int]]()
println(seen.map(_.toList).mkString)

var steps = 0

do {
	seen += banks.clone.toList
	val toDistr = banks.indexOf(banks.max)
	var x = banks(toDistr)
	banks(toDistr) = 0
	for(i <- (1 to x).map(y => (y+toDistr) % banks.length)) banks(i) += 1
	steps += 1
	//println(seen.map(_.toList).mkString(";"), steps)
} while(!(seen contains banks.toList))
println(steps - seen.indexOf(banks.toList))
