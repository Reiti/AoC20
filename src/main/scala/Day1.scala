import util.Util

import scala.annotation.tailrec

object Day1 {
  def main(args: Array[String]): Unit = {
    val expenses: List[Int] = Util.loadDayInts(1);

    //Part 1
    println(sumToX(expenses, 2020).get)
    
    //Part 2
    println(tripleSum(expenses))
  }
  
  @tailrec
  def sumToX(expenses: List[Int], target: Int): Option[Int] = expenses match {
    case x :: xs => xs find { y => x + y == target} match {
      case Some(y) => Some(x * y)
      case None => sumToX(xs, target)
    }
    case _ => None;  
  }
  
  @tailrec
  def tripleSum(expenses: List[Int]): Int = expenses match {
    case x :: xs => sumToX(xs, 2020 - x) match {
      case Some(y) => x*y
      case None => tripleSum(xs);
    }
    case _ => 0;
  }
}
