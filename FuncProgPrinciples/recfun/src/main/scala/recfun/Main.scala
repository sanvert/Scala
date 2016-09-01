package recfun

import scala.collection.mutable.ListBuffer
import scala.annotation.tailrec

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
    if(c == r || c==0) 1
    else if(c < 0 || r < 0) 0
    else 
      pascal(c, r - 1) + pascal(c - 1, r - 1)
  }
  
  def pascalI(c: Int, r: Int): Int = {
    @tailrec
    def loop(row: List[Int], rCur: Int): List[Int] = {
      if (rCur < r) {
        def innerLoop(rowInner: List[Int], rowNext: ListBuffer[Int]): List[Int] = {
          rowInner match {
            case Nil => rowNext.toList
            case x :: tail =>
              if (tail == Nil || tail.size == 0) {
                rowNext += 1
                rowNext.toList
              } else {
                rowNext += (x + tail.head)
                innerLoop(tail, rowNext)
              }
          }
        }

        loop(innerLoop(row, ListBuffer(1)), rCur + 1)
      } else
        row
    }

    if (c == r || c == 0) 1
    else {
      val res = loop(List(1), 0)
      res(c)
    }

  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec
    def loop(charLoc: List[Char], openedCnt: Int): Boolean = {
      charLoc match {
        case Nil => (openedCnt == 0)
        case x :: tail =>
          if (openedCnt < 0) false
          else if (x == '(')
            loop(tail, openedCnt + 1)
          else if (x == ')')
            loop(tail, openedCnt - 1)
          else
            loop(tail, openedCnt)
      }
    }
    loop(chars, 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    def calculate(n: Int, coins: List[Int], m: Int): Int = {
      var acc = 0;
      def iterate(nRem: Int, idx: Int): Unit = {
        if (nRem == 0) acc += 1
        else if (idx < coins.size && nRem > 0) {
          iterate(nRem, idx + 1)
          iterate(nRem - coins(idx), idx)
        }     
      }
      if (n == 0 || coins.size == 0) 0
      else
        iterate(n, m)
        acc
    }
    calculate(money, coins, 0)
  }
}
