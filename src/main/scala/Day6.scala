import util.Util

object Day6 {
  def main(args: Array[String]): Unit = {
    val answers: Array[Array[Set[Char]]] = Util.loadDay(6)
      .split("\n\n")
      .map(_.split("\n").map(_.toSet))
    
    //Part 1
    println(answers.map(_.reduce(_ union _)).map(_.size).sum)
    
    //Part 2
    println(answers.map(_.reduce(_ intersect _)).map(_.size).sum)
  }
}
