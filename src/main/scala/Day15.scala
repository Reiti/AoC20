import util.Util

import scala.annotation.tailrec

object Day15 {
  def main(args: Array[String]): Unit = {
    val input: List[Int] = Util.loadDayLines(15)(0).split(",").map(_.toInt).toList.reverse
    val last = input.tail.reverse.zipWithIndex.map(e => (e._1, e._2)).toMap

    //Part 1
    println(play(last, input.size-1, 2020, input.head))
    
    //Part 2
    println(play(last, input.size-1, 30000000, input.head))
  }
  
  @tailrec
  def play(last: Map[Int, Int], idx: Int, which: Int, num: Int): Int = {
    if idx+1 == which then 
      num
    else
      last.get(num) match {
        case Some(i) => play(last.updated(num, idx), idx + 1, which, idx - i)
        case _ => play(last.updated(num, idx), idx + 1, which, 0)
      }
  }
}
