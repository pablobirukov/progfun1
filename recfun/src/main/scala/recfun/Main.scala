package recfun


object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
    * Exercise 1
    */
  def pascal(col: Int, row: Int): Int = {
    (col, row) match {
      case (_, 0) => 1
      case (_, 1) => 1
      case (0, _) => 1
      case (x, y) if x == y => 1
      case (c, r) => pascal(c, r - 1) + pascal(c - 1, r - 1)
    }
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    chars.fold(Right(0): Either[String, Int])((acc, char) => {
      acc match {
        case Left(_) => Left("")
        case Right(pBalance: Int) =>
          char match {
            case '(' => Right(pBalance + 1)
            case ')' => if (pBalance > 0) Right(pBalance - 1) else Left("")
            case _ => Right(pBalance)
          }
      }
    }) match {
      case Right(pBalance) => pBalance == 0
      case Left(_) => false
    }
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    money match {
      case 0 => 1
      case moneyRest => if (moneyRest > 0 && coins.nonEmpty) countChange(moneyRest - coins.head, coins) + countChange(moneyRest, coins.tail) else 0
    }
  }
}
