import util.Util

object Day10 {
  def main(args: Array[String]): Unit = {
    val adapters: List[Int] = (0 +: Util.loadDayInts(10).sorted)
    
    val differences: List[Int] = (adapters :+ (adapters.max + 3)).sliding(2).map(pair => pair(1) - pair(0)).toList
    
    //Part 1
    println(differences.count(_ == 1) * (differences.count(_ == 3)))
    
    val splitIndizes: List[Int] = differences.zipWithIndex.filter(_._1 == 3).map(_._2 + 1)
    
    val partialLists: List[List[Int]] = (0 +: splitIndizes).sliding(2).map(bounds => adapters.slice(bounds(0), bounds(1))).toList
    
    //Part 2
    println(partialLists.map(paths).product)
  }
  
  def paths(adapters: List[Int]): BigInt = {
    if adapters.size > 1 then
      (1 to 3).map(idx => adapters.drop(idx)).map(paths).sum
    else
      adapters.size
  }
}
