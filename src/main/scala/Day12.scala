import util.Util

import scala.annotation.tailrec

object Day12 {
  
  def main(args: Array[String]): Unit = {
    val instructions: List[String] = Util.loadDayLines(12)
    
    val dest = move(0, 0, 1, instructions)
    
    //Part 1
    println(Math.abs(dest._1) + Math.abs(dest._2))
    
    val dest2 = moveWaypoint(0, 0, 1, 10, instructions);
    
    //Part 2
    println(Math.abs(dest2._1) + Math.abs(dest2._2))
  }
  
  @tailrec
  def moveWaypoint(xf: Int, yf: Int, xw: Int, yw: Int, instructions: List[String]): (Int, Int) = instructions match {
    case x :: xs => {
      val instruction: Char = x.charAt(0)
      val value: Int = x.substring(1).toInt
      instruction match {
        case 'N' => moveWaypoint(xf, yf, xw + value, yw, xs)
        case 'E' => moveWaypoint(xf, yf, xw, yw + value, xs)
        case 'S' => moveWaypoint(xf, yf, xw - value, yw, xs)
        case 'W' => moveWaypoint(xf, yf, xw, yw - value, xs)
        case 'L' => value match {
          case 90 => moveWaypoint(xf, yf, yw, -xw, xs)
          case 180 => moveWaypoint(xf, yf, -xw, -yw, xs)
          case 270 => moveWaypoint(xf, yf, -yw, xw, xs)
        }
        case 'R' => value match {
          case 90 => moveWaypoint(xf, yf, -yw, xw, xs)
          case 180 => moveWaypoint(xf, yf, -xw, -yw, xs)
          case 270 => moveWaypoint(xf, yf, yw, -xw, xs)
        }
        case 'F' => moveWaypoint(xf + value*xw, yf + value*yw, xw, yw, xs)
      }
    }
    case _ => (xf, yf);
  }
  
  @tailrec
  def move(xc: Int, yc: Int, facing: Int, instructions: List[String]): (Int, Int) = instructions match {
    case x :: xs => {
      val dir = x.charAt(0)
      val value = x.substring(1).toInt
      
      dir match {
        case 'N' => move(xc + value, yc, facing, xs)
        case 'E' => move(xc, yc + value, facing, xs)
        case 'S' => move(xc - value, yc, facing, xs)
        case 'W' => move(xc, yc - value, facing, xs)
        case 'F' => facing match {
          case 0 => move(xc + value, yc, facing, xs)
          case 1 => move(xc, yc + value, facing, xs)
          case 2 => move(xc - value, yc, facing, xs)
          case 3 => move(xc, yc - value, facing, xs)
        }
        case 'L' => 
          val newF = (facing - value/90) % 4
          move(xc, yc, if newF >= 0 then newF else newF+4, xs)
        case 'R' =>
          val newF = (facing + value/90) % 4
          move(xc, yc, if newF >= 0 then newF else newF+4, xs)
      }
    }
    case _ => (xc, yc)
  }
  
  
  
}
