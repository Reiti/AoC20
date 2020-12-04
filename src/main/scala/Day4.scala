import util.Util
import scala.language.implicitConversions

object Day4 {
  def main(args: Array[String]): Unit = {
    val passports: Array[Array[String]] = Util.loadDay(4).split("\n\n").map(_.split("\\s"))
    
    //Part 1
    println(passports.count(allPresent))
    
    //Part 2
    println(passports.count(allPresentAndValid))
  }
  
  def allPresent(passport: Array[String]): Boolean = {
    val fields: List[String] = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")
    fields.forall(field => passport.exists(_.startsWith(field)))
  }
  
  def allPresentAndValid(passport: Array[String]): Boolean = {
    val fields: List[String] = List("byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid")

    given Conversion[String, Int] = Integer.parseInt(_)

    fields forall { field =>
      passport.find(_.startsWith(field)) match {
        case Some(entry) => entry match {
          case s"byr:$value" =>
            1920 <= value && value <= 2002
          case s"iyr:$value" =>
            2010 <= value && value <= 2020
          case s"eyr:$value" =>
            2020 <= value && value <= 2030
          case s"hgt:${value}in" =>
            59 <= value && value <= 76
          case s"hgt:${value}cm" =>
            150 <= value && value <= 193
          case s"hcl:#$value" =>
            value.matches("^[0-9a-f]{6}$")
          case s"ecl:$value" =>
            List("amb", "blu", "brn", "gry", "grn", "hzl", "oth") contains value
          case s"pid:$value" =>
            value.matches("^[0-9]{9}$")
          case value => false
        }
        case None => false
      }
    }
  }
}
