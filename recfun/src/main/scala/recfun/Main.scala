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

  def fact(num: Int): Int = {
    def factIter(num: Int, acc: Int): Int =
      if (num == 0) acc
      else factIter(num - 1, num * acc)

    factIter(num, 1)
  }
  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = fact(r) / (fact(c) * fact(r - c))

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(chars: List[Char], check: Int): Boolean =
      if (chars.isEmpty) check == 0
      else if (chars.head == '(') balanceIter(chars.tail, check + 1)
      else if (chars.head == ')')
        if (check == 0) false
        else balanceIter(chars.tail, check - 1)
      else balanceIter(chars.tail, check)

    balanceIter(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def countChangeIter(money: Int, coins: List[Int]): Int =
      if (money <= 0 || coins.isEmpty) 0
      else if (money - coins.head == 0) 1
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)

    countChangeIter(money, coins.sorted)
  }

}
