import scala.collection.mutable.ListBuffer

val input = "jzgqcdpd"

def knot(input: String) = {
	val lengths = (input.map(_.toByte.toInt).toList ++ List(17, 31, 73, 47, 23)).toArray
	val nums = (0 to 255).toArray

	var pos = 0
	var skip = 0

	val len = nums.length
	def round() =
		lengths.foreach(length => {
      val section = (pos until pos + length).foldLeft(List[Int]())((lst, i) => nums(i % len) :: lst)
      for(i <- pos until pos + length) nums(i % len) = section(i - pos)
      pos = (pos + length + skip) % len
      skip += 1
    })

	for(_ <- 1 to 64) round()
	def pad(num: Int) = if(num < 16) "0" + num.toHexString else num.toHexString
	(for(i <- 0 to 15) yield nums.slice(i*16, (i+1)*16).reduceLeft(_^_)).map(pad).mkString
}

def grid(input: String) = {
	val hashes = (0 to 127).map(i => knot(input + "-" + i))
	def pad(i: Int) =
		(if(i.toBinaryString.length < 2) "000" else if(i.toBinaryString.length < 3) "00" else if(i.toBinaryString.length < 4) "0" else "") + i.toBinaryString
	(0 to 127).map(i => hashes(i).flatMap(x => pad(Integer.parseInt(x.toString, 16))).map(_.toString.toByte).toArray).toArray
}

type Region = Set[(Int, Int)]

def getSquaresInUse(input: String) = {
	(0 to 127).flatMap { line =>
		knot(s"$input-$line")
			.flatMap(_.asDigit.toBinaryString.reverse.padTo(4, '0').reverse)
			.zipWithIndex
			.filter(_._1 == '1')
			.map(s => (line, s._2))
	}.toSet
}

def runSecond(): Unit = {
	val squaresInUse = getSquaresInUse("jzgqcdpd")

	val allRegions = squaresInUse.foldLeft(Set.empty[Region]) {
		case (regions, square) if !regions.exists(_.contains(square)) =>
			regions + fillRegion(square, squaresInUse)
		case (regions, _) =>
			regions
	}

	println(allRegions.size)
}

def fillRegion(square: (Int, Int), unusedSquares: Region): Region = {
	val neighbours = Seq(
		square.copy(_1 = square._1 - 1),
		square.copy(_1 = square._1 + 1),
		square.copy(_2 = square._2 - 1),
		square.copy(_2 = square._2 + 1)
	).filter(unusedSquares.contains)

	Set(square) ++ neighbours.flatMap(fillRegion(_, unusedSquares - square -- neighbours))
}

runSecond()

//println(grid(input).map(_.toList).toList)

//println(regions(grid(input, 0)))
