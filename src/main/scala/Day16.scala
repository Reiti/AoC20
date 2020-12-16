import util.Util

import scala.annotation.tailrec

object Day16 {
  def main(args: Array[String]): Unit = {
    val input: Array[String] = Util.loadDay(16).split("\n\n")
        

    val rules: Map[String, List[Range]] = parseRules(input(0))
    val my: List[Int] = input(1).split("\n")(1).split(",").map(_.toInt).toList
    val nearby: List[List[Int]] =  input(2).split("\n").drop(1).map(_.split(",").map(_.toInt).toList).toList
    
    //Part 1
    println(nearby.flatMap(_.filter(f => !possiblyValid(f, rules.values.toList))).sum)
    
    val onlyValid: List[List[Int]] = nearby.filter(!_.exists(f => !possiblyValid(f, rules.values.toList)))
    val allPositions: List[(String, List[Int])] = rules.toList.map((name, range) => (name, findMatchingPositions(range, onlyValid)))
    val uniquePositions: List[(String, Int)] = findUniquePositions(allPositions, List())
    
    //Part 2
    println(uniquePositions.filter(pos => pos._1.startsWith("departure")).map(pos => my(pos._2).toLong).product)
  }
  
  def parseRules(rules: String): Map[String, List[Range]] = {
    rules.split("\n").map(rule => {
      val s = rule.split(":")
      val name = s(0)
      (name, s(1).split("or").map(_.trim).map(range => {
        val sp = range.split("-")
        (sp(0).toInt to sp(1).toInt)
      }).toList)
    }).toMap
  }
  
  def possiblyValid(field: Int, ranges: List[List[Range]]): Boolean = {
    ranges.exists(v => (v(0) contains field) || (v(1) contains field))
  }

  def findMatchingPositions(range: List[Range], nearby: List[List[Int]]): List[Int] = {
    (0 to nearby(0).size - 1).filter(i => {
      nearby.forall(ticket => (range(0) contains ticket(i)) || (range(1) contains ticket(i)))
    }).toList
  }
  
  @tailrec
  def findUniquePositions(pos: List[(String, List[Int])], found: List[(String, Int)]): List[(String, Int)] = {
    if pos.size == found.size then
      found
    else
      val curr: (String, List[Int]) = pos.find(p => p._2.size == 1).get
      findUniquePositions(pos.map(p => (p._1, p._2.filter(_ != curr._2(0)))), (curr._1, curr._2(0)) +: found)
  }
}
