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

  /** EXERCISE 1
   The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it.
   Write a function that computes the elements of Pascal’s triangle by means of a recursive process.

   Do this exercise by implementing the pascal function in Main.scala,
   which takes a column c and a row r, counting from 0 and returns the number at that spot in the triangle.
   For example, pascal(0,2)=1,pascal(1,2)=2 and pascal(1,3)=3.
   */

  def pascal(c: Int, r: Int): Int = {
    if (r == 0 || c == 0 || r == c) 1
    else if (c > r) 0
    else pascal(c - 1, r - 1) + pascal(c, r - 1)
  }

  /** EXERCISE 2
  Write a recursive function which verifies the balancing of parentheses in a string, which we represent as a List[Char] not a String.

   For example,
   The function should return true for the following strings:
      * (if (zero? x) max (/ 1 x))
      * I told him (that it’s not (yet) done). (But he wasn’t listening)

   The function should return false for the following strings:
      * :-)
      * ())(
   */

  def balance(chars: List[Char]): Boolean = {
    balance(chars, 0)
  }
  private def balance(chars: List[Char], unbalancedCount: Int): Boolean = {
    if (chars.isEmpty) unbalancedCount == 0
    else if (unbalancedCount < 0) false
    else {
      val count =
        if (chars.head == '(') unbalancedCount + 1
        else if (chars.head == ')') unbalancedCount - 1
        else unbalancedCount
      balance(chars.tail, count)
    }
  }

  /** EXERCISE 3
  Write a recursive function that counts how many different ways you can make change for an amount, given a list of coin denominations.
  For example, there are 3 ways to give change for 4 if you have coins with denomination 1 and 2: 1+1+1+1, 1+1+2, 2+2.

  Do this exercise by implementing the countChange function inMain.scala.
  This function takes an amount to change, and a list of unique denominations for the coins.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money <= 0) 0
    else countChangeAux(money, coins.sorted.reverse)
    }
  private def countChangeAux(money: Int, coins: List[Int]): Int = {
    if (coins.isEmpty || money < 0) 0
    else if (money == 0) 1
    else countChangeAux(money - coins.head, coins) + countChangeAux(money, coins.tail)
  }
}
