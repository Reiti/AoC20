import util.Util

object Day18 {
  def main(args: Array[String]): Unit = {
    val homework: List[String] = Util.loadDayLines(18)
    
    //Part 1
    println(homework.map(l => parse(l, 0, 0, evaluate)).sum)
    
    //Part 2
    println(homework.map(l => parse(l, 0, 0, evaluateWithPrecendence)).sum)
  }
  
  
  def parse(line: String, oB: Int, curr: Int, evaluation: (String => Long)): Long = {
    if curr == line.size then
      evaluation(line)
    else if line(curr) == '(' then
      parse(line, curr, curr+1, evaluation)
    else if line(curr) == ')' then
      val before = line.substring(0, oB)
      val expr = line.substring(oB + 1, curr)
      val after = line.substring(curr + 1)
      parse(before + evaluation(expr) + after, 0, 0, evaluation)
    else
      parse(line, oB, curr+1, evaluation)
  }
  
  def evaluate(expr: String): Long = {
    expr.split(" ").map(_.trim).foldLeft((0L, ""))((elem, curr) => curr match {
      case "*" => (elem._1, "*")
      case "+" => (elem._1, "+")
      case v => elem._2 match {
        case "*" => (elem._1 * v.toLong, "")
        case "+" => (elem._1 + v.toLong, "")
        case _ => (v.toLong, "")
      }
    })._1
  }
  
  def evaluateWithPrecendence(expr: String): Long = expr.split("\\*").map(_.trim).map(evaluate).product
}
