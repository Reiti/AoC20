import util.Util

object Day2 {
  def main(args: Array[String]): Unit = {
    val input: List[Array[String]] = Util.loadDayLines(2).map(line => line.split(" "))
    
    //Part 1
    println(countValid(input, countPolicy))
    
    //Part 2
    println(countValid(input, posPolicy))
  }
  
  def countPolicy(min: Int, max: Int, char: Char, password: String): Boolean = {
    val count: Int = password.count(_ == char)
    
    min <= count && count <= max
  }
  
  def posPolicy(pos1: Int, pos2: Int, char: Char, password: String): Boolean = {
    password.charAt(pos1 - 1) == char ^ password.charAt(pos2 - 1) == char
  }

  def countValid(input: List[Array[String]], policy: (Int, Int, Char, String) => Boolean): Int = {
    input.count(line => {
      val range: Array[String] = line(0).split("-");

      policy(range(0).toInt, range(1).toInt, line(1).substring(0, 1)(0), line(2))
    })
  }
}
