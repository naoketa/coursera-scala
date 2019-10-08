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
  def pascal(c: Int, r: Int): Int = {
    if (c == 0 || c == r) 1
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def inner(chars: List[Char], count: Int): Boolean = {
      if (count < 0) return false
      chars match {
        case chars if chars.isEmpty => count == 0
        case '('::_ => inner(chars.tail, count + 1)
        case ')'::_ => inner(chars.tail, count - 1)
        case _ => inner(chars.tail, count);
      }
    }

    inner(chars, 0)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      (money, coins) match {
        case (m, c) if m < 0 || c.isEmpty => 0
        case (m, _) if m == 0 => 1
        case _ => countChange(money - coins.head, coins) + countChange(money, coins.tail)
      }
    }
  }
