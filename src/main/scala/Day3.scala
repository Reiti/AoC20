import util.Util

import scala.annotation.tailrec

object Day3 {
  def main(args: Array[String]): Unit = {
    val map:  Array[Array[Char]] = Util.loadDayLines(3).map(_.split("").map(_(0))).toArray
    
    //Part 1
    println(countTrees(map, 3, 1))
    
    val slopes: List[(Int, Int)] = List((1, 1), (3, 1), (5, 1), (7, 1), (1, 2))
    
    //Part 2
    println((for slope <- slopes yield countTrees(map, slope._1, slope._2)).foldLeft(1)(_ * _))
  }
  
  def countTrees(map: Array[Array[Char]], slopeX: Int, slopeY: Int): Int = {
    @tailrec
    def countTreesH(x: Int, y: Int, acc: Int): Int = {
      val xPos: Int = (x + slopeX) % map(0).size
      val yPos: Int = y + slopeY
      if yPos >= map.size then
        acc
      else if map(yPos)(xPos) == '#' then
        countTreesH(xPos, yPos, acc + 1)
      else
        countTreesH(xPos, yPos, acc)
    }
    
    countTreesH(0, 0, 0)
  }
}
