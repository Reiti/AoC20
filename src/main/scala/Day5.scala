import util.Util

object Day5 {
  def main(args: Array[String]): Unit = {
    val boardingPasses: List[Int] = Util.loadDayLines(5)
      .map(_.replaceAll("[BR]", "1").replaceAll("[FL]", "0"))
      .map(Integer.parseInt(_, 2))

    //Part 1
    println(boardingPasses.max)
    
    //Part2
    println(boardingPasses.sorted.sliding(2).find(i => i(1) - i(0) == 2).get(0) + 1)
  }
}
