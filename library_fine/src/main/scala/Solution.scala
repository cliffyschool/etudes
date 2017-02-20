import java.util.Scanner

case class BookDate(day: Int, month: Int, year: Int)

class Solution {
  val FINE_YEAR_PLUS = 10000
  val FINE_PER_MONTH = 500
  val FINE_PER_DAY = 15

    def calculateFine(due: Seq[Int], returned: Seq[Int]) = {
      val dueDate = BookDate(due(0), due(1), due(2))
      val returnDate = BookDate(returned(0), returned(1), returned(2))

      if (returnDate.year > dueDate.year)
        FINE_YEAR_PLUS
      else if (returnDate.month > dueDate.month &&
               returnDate.year == dueDate.year)
        FINE_PER_MONTH * (returnDate.month - dueDate.month)
      else if (returnDate.day > dueDate.day &&
              returnDate.month == dueDate.month &
              returnDate.year == dueDate.year)
        FINE_PER_DAY * (returnDate.day - dueDate.day)
      else
        0
    }
}

object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()

    val returnDate = readIntsFromLine(in)
      .take(3).toSeq
    val dueDate = readIntsFromLine(in)
      .take(3).toSeq

    val fine = solution.calculateFine(dueDate, returnDate)

    println(fine)
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
