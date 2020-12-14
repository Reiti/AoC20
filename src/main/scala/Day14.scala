import util.Util

import scala.annotation.tailrec

object Day14 {
  def main(args: Array[String]): Unit = {
    val program: List[String] = Util.loadDayLines(14)
    
    //Part 1
    println(run(program, "", Map()).values.sum)
    
    //Part 2
    println(runV2(program, "", Map()).values.sum)
  }
  
  @tailrec
  def run(program: List[String], mask: String, memory: Map[Long, Long]): Map[Long, Long] = program match {
    case x :: xs => x match {
      case s"mask = $value" => run(xs, value, memory)
      case s"mem[$addr] = $value" => 
        val n: Long = java.lang.Long.parseLong(java.lang.Long.toBinaryString(value.toLong).reverse.padTo(36, "0").reverse.zip(mask).map((c, m) => if m == 'X' then c else m).mkString, 2)
        run(xs, mask, memory.updated(addr.toLong, n.toLong))
      case _ => Map()
    }
    case _ => memory
  }
  
  def runV2(program: List[String], mask: String, memory: Map[Long, Long]): Map[Long, Long] = program match {
    case x :: xs => x match {
      case s"mask = $value" => runV2(xs, value, memory)
      case s"mem[$addr] = $value" =>
        val n: String = java.lang.Long.toBinaryString(addr.toLong).reverse.padTo(36, "0").reverse.zip(mask).map((c, m) => {
          if m == '0' then c else m
        }).mkString
        val floatingCount: Int = n.count(_.equals('X'))
        val floating: List[String] = (0 to (1 << floatingCount) - 1).map(e => java.lang.Long.toBinaryString(e).reverse.padTo(floatingCount, '0').reverse).toList
        val adresses: List[Long] = floating.map(e => replaceFloating(n.toList, e, ""))
        runV2(xs, mask, updateMemory(adresses, memory, value.toLong))
      case _ => Map()
    }
    case _ => memory
  }
  
  @tailrec
  def replaceFloating(n: List[Char], floating: String, result: String): Long = n match {
    case x :: xs => 
      if x == 'X' then {
        replaceFloating(xs, floating.tail, result :+ floating.head)
      } else
        replaceFloating(xs, floating, result :+ x)
    case _ => java.lang.Long.parseLong(result, 2)
  }
  
  @tailrec
  def updateMemory(adresses: List[Long], memory: Map[Long, Long], value: Long): Map[Long, Long] = adresses match {
    case x :: xs => updateMemory(xs, memory.updated(x, value), value)
    case _ => memory
  }

}
