import util.Util

import scala.annotation.tailrec

object Day8 {
  def main(args: Array[String]): Unit = {
    val program: Map[Int, (String, Int)] = Util.loadDayProgram(8)
    
    //Part 1
    println(runProgram(program).merge)
    
    //Part 2
    println(findTerminatingVariation(program))
  }
  
  def runProgram(program: Map[Int, (String, Int)]): Either[Int, Int] = {
    @tailrec
    def runProgramH(idx: Int, acc: Int, visited: Set[Int]): Either[Int, Int] = {
      if visited contains idx then
        Left(acc)
      else
        program.get(idx) match {
          case Some(("nop", _)) => runProgramH(idx + 1, acc, visited + idx)
          case Some(("acc", v)) => runProgramH(idx + 1, acc + v, visited + idx)
          case Some(("jmp", v)) => runProgramH(idx + v, acc, visited + idx)
          case _ => Right(acc)
        }
    }
    runProgramH(0, 0, Set())
  }
  
  def findTerminatingVariation(program: Map[Int, (String, Int)]): Int = {
    val swappableIndices: List[Int] = program.toList.filter(entry => {
      entry._2._1.equals("nop") || entry._2._1.equals("jmp")
    }).map(_._1)
    val idx = swappableIndices.find(idx => runProgram(switch(program, idx)).isRight).get
    runProgram(switch(program, idx)).merge
  }
  
  def switch(program: Map[Int, (String, Int)], idx: Int): Map[Int, (String, Int)] = program(idx) match {
    case ("nop", v) => program.updated(idx, ("jmp", v))
    case ("jmp", v) => program.updated(idx, ("nop", v))
  }
}
