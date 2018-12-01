
def knot(input: String) = {
	val lengths = (input.map(_.toByte.toInt).toList ++ List(17, 31, 73, 47, 23)).map(_.toInt).toArray
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

println(knot(""))
