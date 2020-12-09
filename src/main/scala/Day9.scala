import util.Util

import scala.annotation.tailrec

object Day9 {
  def main(args: Array[String]): Unit = {
    def numbers: List[Long] = Util.loadDayLines(9).map(_.trim.toLong);

    def invalidNumber = numbers.sliding(26).find(pre => !valid(pre)).get.reverse.head
    
    //Part 1
    println(invalidNumber);
    
    val summingSet = findSummingSet(numbers, invalidNumber);
    
    //Part2
    println(summingSet.min + summingSet.max)
  }
  
  def valid(numbers: List[Long]): Boolean = numbers.reverse match {
    case x :: xs => {
      for
        x <- xs
        y <- xs
        if x != y
      yield (x+y)
    }.contains(x)
    case _ => false
  }
  
  def findSummingSet(numbers: List[Long], target: Long): List[Long] = {
    @tailrec
    def findSummingSetH(window: List[Long], remaining: List[Long], sum: Long): List[Long] = {
      if sum == target then
        window
      else if sum < target then
        val next = remaining.head
        findSummingSetH(window appended next, remaining.tail, sum + next)
      else
        val oldest = window.head
        findSummingSetH(window.tail, remaining, sum - oldest)
    }
    findSummingSetH(List(), numbers, 0);
  }
}
