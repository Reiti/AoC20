import util.Util

object Day17 {
  def main(args: Array[String]): Unit = {
    val input: List[String] = Util.loadDayLines(17)

    val map: Map[(Int, Int, Int), Char] = input.zipWithIndex.map(f => f._1.zipWithIndex.map(g => ((f._2, g._2, 0), g._1))).flatten.toMap
    
    //Part 1
    println(step(step(step(step(step(step(map)))))).values.count(_ == '#'))
    
    val map4d: Map[(Int, Int, Int, Int), Char] = input.zipWithIndex.map(f => f._1.zipWithIndex.map(g => ((f._2, g._2, 0, 0), g._1))).flatten.toMap
    
    //Part 2
    println(step4d(step4d(step4d(step4d(step4d(step4d(map4d)))))).values.count(_ == '#'))
  }
  
  def step(map: Map[(Int, Int, Int), Char]): Map[(Int, Int, Int), Char] = {
    val xR = range(map.keys.map(_._1).toList)
    val yR = range(map.keys.map(_._2).toList)
    val zR = range(map.keys.map(_._3).toList)
    
    ((xR._1 - 1) to (xR._2 + 1)).map(x => {
      ((yR._1 - 1) to (yR._2 + 1)).map(y => {
        ((zR._1 - 1) to (zR._2 + 1)).map(z => {
          val c: Char = map.getOrElse((x,y,z), '.')
          val count: Int = countNeighbors(map, x, y, z)
          if c == '#' && count != 2 && count != 3 then
            ((x, y, z), '.')
          else if c == '.' && count == 3 then
            ((x, y, z), '#')
          else
            ((x, y, z), c)
        })
      }).flatten
    }).flatten.toMap
  }
  
  def range(range: List[Int]): (Int, Int) = {
    (range.min, range.max)
  }
  
  def countNeighbors(map: Map[(Int, Int, Int), Char], x: Int, y: Int, z: Int): Int = {
    for
      i <- -1 to 1
      j <- -1 to 1
      k <- -1 to 1
      if i!=0 || j!=0 || k!=0
    yield
      map.getOrElse((x+i, y+j, z+k), '.')
  }.count(_ == '#')
  
  def pprint(map: Map[(Int, Int, Int), Char]): Unit = {
    val xR = range(map.keys.map(_._1).toList)
    val yR = range(map.keys.map(_._2).toList)
    val zR = range(map.keys.map(_._3).toList)
    
    for
      z <- (zR._1 to zR._2)
    do { 
      println("z: " + z)
      for
        x <- (xR._1 to xR._2)
      do 
        for 
          y <- (yR._1 to yR._2)
        do
          print(map((x,y,z)))
        println()
    }
  }
  
  def countNeighbors4d(map: Map[(Int, Int, Int, Int), Char], x: Int, y: Int, z: Int, w: Int): Int = {
    for
    i <- -1 to 1
    j <- -1 to 1
    k <- -1 to 1
    l <- -1 to 1
    if i!=0 || j!=0 || k!=0 || l != 0
      yield
        map.getOrElse((x+i, y+j, z+k, w+l), '.')
  }.count(_ == '#')

  def step4d(map: Map[(Int, Int, Int, Int), Char]): Map[(Int, Int, Int, Int), Char] = {
    val xR = range(map.keys.map(_._1).toList)
    val yR = range(map.keys.map(_._2).toList)
    val zR = range(map.keys.map(_._3).toList)
    val wR = range(map.keys.map(_._4).toList)
    ((xR._1 - 1) to (xR._2 + 1)).map(x => {
      ((yR._1 - 1) to (yR._2 + 1)).map(y => {
        ((zR._1 - 1) to (zR._2 + 1)).map(z => {
          ((wR._1 - 1) to (wR._2 + 1)).map(w => {
            val c: Char = map.getOrElse((x, y, z, w), '.')
            val count: Int = countNeighbors4d(map, x, y, z, w)
            if c == '#' && count != 2 && count != 3 then
              ((x, y, z, w), '.')
            else if c == '.' && count == 3 then
              ((x, y, z, w), '#')
            else
              ((x, y, z, w), c)
          })
        }).flatten
      }).flatten
    }).flatten.toMap
  }
}
