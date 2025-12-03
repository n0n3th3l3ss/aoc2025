package aoc._3

object _3_InputParser {

  
  private def parseLine(line: String): BatteryBank =
    BatteryBank(line)

  def parseLines(lines: Seq[String]): Seq[BatteryBank] =
    lines.map(parseLine)

}
