val in = 312051

class circle(val num: Int) {
	def last = if(num == 0) 0 else math.pow(num * 2 - 1, 2).toInt
	def first = if(num == 0 || num == 1) num else (new circle(num - 1).last + 1)
	def corners = List(first + length - 2, first + length*2 - 3, first + length*3 - 4, last)
	def middles = (for(v <- corners) yield v - length / 2).toList
	def length = num * 2 - 1
	def distances = (for(i <- first to last) yield (i,
		if(middles contains i) num - 1
		else
			if(corners contains i) num - 1 + length / 2
			else {
				val which = middles(middles.zipWithIndex.map(x => (math.abs(x._1 - i), x._2)).min._2)
				num - 1 + math.abs(which - i)
			}
	)).toList
	def distanceOf(n: Int) = distances.find(_._1 == n)
	override def toString = s"Circle: $num"
}

def circleOf(num: Int) =
	new circle({
		var res = -1; var i = 1;
		while(res == -1) { if(num <= math.pow(i * 2 - 1, 2).toInt) { res = i };	i += 1 }
		res
	})
println(circleOf(in).distanceOf(in).get._2)

val grid = scala.collection.mutable.Map((0, 0) -> 1, (1, 0) -> 1)

val dirs = List((0, 1), (-1, 0), (0, -1), (1, 0))
val neighbours = List((-1, 1), (0, 1), (1, 1), (1, 0), (1, -1), (0, -1), (-1, -1), (-1, 0))
var last = (1,0)
var dir = 0
def add(a: (Int, Int), b: (Int, Int)): (Int, Int) = (a._1 + b._1, a._2 + b._2)
var v = 0
while(v <= in) {
	last = add(last, dirs(dir))
	v = neighbours.map(x => grid.getOrElse(add(last, x), 0)).sum
	grid(last) = v
	println(dir, last, v)
	if(v > in) println(s"found $v")
	dir =
		if(last._1 > 0 && last._2 < 0 && last._1 == -last._2) dir
	 		else if((last._1 > 0 && last._2 < 0 && last._1 == -last._2+1) || math.abs(last._1) == math.abs(last._2))
				 (dir + 1) % dirs.length
				 else dir
}
