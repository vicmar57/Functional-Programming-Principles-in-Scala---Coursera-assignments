package recfun

object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if(r==c || c==0 || r==0) 1
    else pascal(c-1,r-1)+pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    def balanceIter(count: Int,chars: List[Char]): Boolean = {
      if(count <0) false
      else if (count==0 && chars.isEmpty) true
      else if (count!=0 && chars.isEmpty) false
      else if (chars.head == '(') balanceIter(count+1,chars.tail)
      else if (chars.head == ')') balanceIter(count-1,chars.tail)
      else balanceIter(count, chars.tail)
    }
    balanceIter(0, chars)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0 || coins.isEmpty) 0
    else countChange(money-coins.head, coins) + countChange(money, coins.tail)
  }
}
