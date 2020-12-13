import util.Util

object Day13 {
  def main(args: Array[String]): Unit = {
    val input: List[String] = Util.loadDayLines(13)
    val arrival: Int = input(0).toInt
    val schedule: Array[String] = input(1).split(",")
    
    
    val earliest: (Int, Int) = schedule.filter(_ != "x").map(ts => {
      val time: Int = ts.toInt
      (time, time - (arrival % time))
    }).minBy(_._2)
    
    //Part 1
    println(earliest._1 * earliest._2)
    
    val plan: Array[(Int, Int)] = schedule.zipWithIndex.filter(_._1 != "x").map(e => {
      val m = e._1.toInt
      val a = (m - e._2) % m
      
      (a, m)
    })
    
    //Part 2
    println(crt(plan))
  }
  
  def crt(plan: Array[(Int, Int)]): BigInt = {
    val M: BigInt = plan.map(e => BigInt(e._2)).product
    val mk: List[BigInt] = plan.map(e => M/BigInt(e._2)).toList
    val yk: List[BigInt] = mk.zip(plan.map(_._2)).map(e => e._1.modInverse(e._2)).toList
    
    Util.zip3(plan.map(e => BigInt(e._1)).toList, mk, yk).map(e => e._1 * e._2 * e._3).sum.mod(M)
  }
}
