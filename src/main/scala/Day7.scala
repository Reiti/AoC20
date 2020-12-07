import util.Util

import scala.annotation.tailrec
import scala.collection.immutable.Queue

object Day7 {
  def main(args: Array[String]): Unit = {
    val input: List[String] = Util.loadDayLines(7)

    val bags: List[Array[String]] = input.map(_.split("(bags\\ contain)|(bag[s]?,)|(bag[s]?\\.)").map(_.trim))
    
    val bagsMap: Map[String, Set[(Int, String)]] = bags.map(bag => (bag.head.trim, prep(bag.tail))).toMap
    
    //Part 1
    println(bagsMap.keySet.filter(!_.equals("shiny gold")).count(key => find(key, "shiny gold", bagsMap)))
    
    //Part 2
    println(countContained("shiny gold", bagsMap))
  }
  
  def prep(bags: Array[String]): Set[(Int, String)] = {
    bags.map(x => {
      if x equals "no other" then
        (1, x)
      else
        (x.takeWhile(_.isDigit).toInt, x.dropWhile(_.isDigit).trim)
    }).toSet
  }
  
  def find(from: String, color: String, bags: Map[String, Set[(Int, String)]]): Boolean = {
    @tailrec
    def findH(acc: Queue[String]): Boolean = acc match {
      case x +: xs => 
       if x equals color then
         true
       else
         findH(xs.enqueueAll(bags.getOrElse(x, Set()).map(_._2)))
      case _ => false
    }
    findH(Queue(from))
  }
  
  def countContained(from: String, bags: Map[String, Set[(Int, String)]]): Int = {
    @tailrec
    def countContainedH(acc: List[(Int, String)], count: Int): Int = acc match {
      case x::xs => 
        if bags.contains(x._2) then
          countContainedH(xs.prependedAll(bags(x._2).map(v => (v._1 * x._1, v._2))), count + x._1)
        else
          countContainedH(xs, count)
      case _ => count - 1
    }
    countContainedH(List((1, from)), 0)
  }
}
