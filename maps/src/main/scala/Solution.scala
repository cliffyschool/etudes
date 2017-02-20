import java.util.Scanner

case class Solution(entries: Map[String,String]) {

  def get(key: String) = {
    entries.get(key)
      .map(key + "=" + _)
      .getOrElse("Not found")
  }
}

object Solution{

  def apply(e: Seq[(String,String)]): Solution = {
    Solution(e.toMap)
  }

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)

    val numEntries = readIntFromLine(in)
    val entries =
    (0 until numEntries)
      .map(i => readLine(in))
      .map(line => line.split(" "))
      .map(arr => (arr(0), arr(1)))
      .toSeq
    val solution = Solution(entries)
    var done = false
    while (!done) {
      val query = readLine(in)
      done = query.isEmpty
      if (!done)
        println(solution.get(query))
    }
  }
  
  def readLine(in: Scanner) = {
    in.nextLine()
  }
  
  def readIntFromLine(in: Scanner) = {
    readLine(in).toInt
  }

  def readIntsFromLine(in: Scanner) = {
    readLine(in).split(" ").map(s => s.toInt)
  }
}
