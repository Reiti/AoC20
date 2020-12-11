import Day11.get
import util.Util

import scala.annotation.tailrec

object Day11 {
  def main(args: Array[String]): Unit = {
    val map: List[String] = Util.loadDayLines(11);
    
    //Part 1
    println(evolve(map, List(), countEmptyAdjacent, 4).map(_.count(_ == '#')).sum)
    
    //Part 2
    println(evolve(map, List(), countEmptyVisible, 3).map(_.count(_ == '#')).sum)
  }
  
  @tailrec
  def evolve(map: List[String], previous: List[String], neighborhood: (Int, Int, List[String]) => Int, limit: Int): List[String] = {
    if map equals previous then
      map
    else
      val newMap = {
        for
          y <- 0 to map.size - 1
        yield {
          for
            x <- 0 to map(0).size - 1
          yield {
            if map(y)(x) == 'L' && neighborhood(x,y,map) == 8 then
              '#'
            else if map(y)(x) == '#' && neighborhood(x,y,map) <= limit then
              'L'
            else
              map(y)(x)
          }
        }.mkString
      }
      evolve(newMap.toList, map, neighborhood, limit)
  }
  
  def countEmptyAdjacent(x: Int, y: Int, map: List[String]): Int = {
    for 
      i <- -1 to 1
      j <- -1 to 1
      if i != 0 || j != 0
    yield get(x + i, y + j, map)
  }.count(pos => pos.getOrElse('.') == 'L' || pos.getOrElse('.') == '.')
  
  def countEmptyVisible(x: Int, y: Int, map: List[String]): Int = {
    for
      i <- -1 to 1
      j <- -1 to 1
      if i != 0 || j != 0 
    yield findVisible(x,y, (i, j), map)
  }.count(pos => pos.getOrElse('.') == 'L' || pos.getOrElse('.') == '.')
  
  def get(x: Int, y: Int, map: List[String]): Option[Char] = map.lift(y).flatMap(_.lift(x))
  
  @tailrec
  def findVisible(x: Int, y: Int, direction: (Int, Int), map: List[String]): Option[Char] = get(x + direction._1, y + direction._2, map) match {
    case Some(c) => 
      if c == '.' then
        findVisible(x + direction._1, y + direction._2, direction, map)
      else
        Some(c)
    case _ => None
  }
}
