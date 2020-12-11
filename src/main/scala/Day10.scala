import util.Util

object Day10 {
  def main(args: Array[String]): Unit = {
    val adapters: List[Int] = (0 +: Util.loadDayInts(10).sorted)
    
    val differences: List[Int] = (adapters :+ (adapters.max + 3)).sliding(2).map(pair => pair(1) - pair(0)).toList
    
    //Part 1
    println(differences.count(_ == 1) * (differences.count(_ == 3)))
    
    val blockIndizes: List[Int] = differences.zipWithIndex.filter(_._1 == 3).map(_._2 + 1)
    
    val blockSizes: List[Int] = (0 +: blockIndizes).sliding(2).map(bounds => bounds(1) - bounds(0)).toList
    
    //Part 2
    println(blockSizes.map(paths).product)
  }
  
  def paths(n: Int): BigInt = {
    if n > 1 then
      (1 to 3).map(hop => n - hop).map(paths).sum
    else if n < 0 then
      0
    else 
      n
  }
}
