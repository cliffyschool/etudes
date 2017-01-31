import java.util.Scanner

class Solution {
    // TODO
    def owedToAnna(itemCosts: Array[Int], indexOfItemAnnaSkipped: Int, amtChargedByBrian: Double) = {
      val annaShouldPay = itemCosts.indices
        .map(idx => if (idx == indexOfItemAnnaSkipped) 0 else itemCosts(idx) / 2.0)
        .sum
      amtChargedByBrian - annaShouldPay
    }
}

object Solution{

  def main(args: Array[String]) : Unit = {
    val in = new Scanner(System.in)
    val solution = new Solution()
    val itemCount_annaSkipped = numItemsAndIndexOfSkippedItem(in)
    val numItems = itemCount_annaSkipped(0)
    val annaSkipped = itemCount_annaSkipped(1)

    val itemCosts = costs(in, numItems)

    val charged = brianCharged(in)
    val owedToAnna = solution.owedToAnna(itemCosts, annaSkipped, charged)

    val toPrint = Option(owedToAnna)
      .filter(owed => !0.0.equals(owed))
      .map(c => c.toInt.toString)
      .getOrElse("Bon Appetit")

    System.out.println(toPrint)
  }

  def numItemsAndIndexOfSkippedItem(in: Scanner) = {
    in.nextLine().split(" ").map(s => s.toInt)
  }

  def costs(in: Scanner, numCosts: Int) = {
    in.nextLine().split(" ").map(s => s.toInt).take(numCosts)
  }

  def brianCharged(in: Scanner) = {
    in.nextLine().toInt
  }
}