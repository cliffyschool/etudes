import java.util.Scanner

class Solution {
    // TODO
    def doSomething() = ???
}

object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()

    val toPrint = "something"

    System.out.println(toPrint)
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
