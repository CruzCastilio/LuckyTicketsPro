object Core extends Calculations {
  def main(args: Array[String]): Unit = {
    val integers = calc(0, 0, 0)
    val tickets = (comb(integers.size, integers, integers) zip (for (x <- 1 to integers.size) yield
      integers).flatten).toString.replaceAll("[(),a-zA-Z]", "").split(" ").toList
    println(s"Всего существует ${tickets.size} счастливых билетов со значением сумм первых и последних трех цифр равным 13.")
    println("Список билетов: " + tickets.mkString(", "))
  }
}
trait Calculations {

  def calc(x: Int, y: Int, z: Int, accum: Seq[(Int, Int, Int)] = Nil): Seq[(Int, Int, Int)] = {
    val tup = if ((x + y + z) == 13) Seq((x, y, z)) else Nil
    if (z < 9) calc(x, y, z + 1, accum ++ tup) else {
      if (y < 9) calc(x, y + 1, 0, accum ++ tup) else {
        if (x < 9) calc(x + 1, 0, 0, accum ++ tup) else accum
      }
    }
  }

  def comb(count: Int, tup: Seq[(Int, Int, Int)], accum: Seq[(Int, Int, Int)] = Nil): Seq[(Int, Int, Int)] = {
    val change: Seq[(Int, Int, Int)] = tup.tail :+ tup.head
    if (count != 1) comb(count - 1, change, accum ++ change) else accum
  }
}