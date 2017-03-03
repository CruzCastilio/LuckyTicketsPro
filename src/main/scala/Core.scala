import scala.util.Try
import scala.io.StdIn._

object Core extends Calculations {
  def main(args: Array[String]): Unit = {
    /**
      * User's input.
      **/
    val input = readLine("Enter the number from 1 to 27: ")
    /**
      * Input correctness check.
      */
    val check = Try(input.toInt).toOption
    check match {
      case Some(num) if num >= 1 && num <= 27 =>

        /**
          * Calculation of all possible combinations for entered number.
          */
        val integers = calc(num)
        /**
          * Calculation and output of all possible "lucky tickets"!
          */
        val tickets = (comb(integers.size, integers, integers) zip (for (x <- 1 to integers.size) yield
          integers).flatten).toString.replaceAll("[(),a-zA-Z]", "").split(" ").toList
        println(s"There are ${tickets.size} lucky tickets where the sum of first three integers and last three " +
          s"integers equal to $num.")
        println("Tickets list: " + tickets.mkString(", "))
      case _ => println("Wrong input!")
    }
  }
}

trait Calculations {

  def calc(luckyNumber: Int, x: Int = 0, y: Int = 0, z: Int = 0, accum: Seq[(Int, Int, Int)] = Nil):
  Seq[(Int, Int, Int)] = {
    val tup = if ((x + y + z) == luckyNumber) Seq((x, y, z)) else Nil
    if (z < 9) calc(luckyNumber, x, y, z + 1, accum ++ tup) else {
      if (y < 9) calc(luckyNumber, x, y + 1, 0, accum ++ tup) else {
        if (x < 9) calc(luckyNumber, x + 1, 0, 0, accum ++ tup) else accum ++ tup
      }
    }
  }

  def comb(count: Int, tup: Seq[(Int, Int, Int)], accum: Seq[(Int, Int, Int)] = Nil): Seq[(Int, Int, Int)] = {
    val change: Seq[(Int, Int, Int)] = tup.tail :+ tup.head
    if (count != 1) comb(count - 1, change, accum ++ change) else accum
  }
}