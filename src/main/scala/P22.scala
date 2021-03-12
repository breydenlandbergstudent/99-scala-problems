import scala.annotation.tailrec

// Problem 22
// (*) Create a list containing all integers within a given range.
//
//
// Example:
//
// scala> range(4, 9)
// res0: List[Int] = List(4, 5, 6, 7, 8, 9)

object P22 extends App {
    // hmmm...
    def rangeOriginal (K_1 : Int, K_2 : Int) : Option[List[Int]] = {
        @tailrec
        def innerRange (runningList : List[Int], innerK : Int) : Option[List[Int]] = {
            innerK match {
                case number if number <= K_2 => innerRange (runningList ::: List (number), innerK + 1)
                case _ => Some (runningList)
            }
        }
        innerRange (List (), K_1)
    }

    // possible to integrate Option?
    def range (start : Int, end : Int) : List[Int] = {
        if (start <= end) {
            start :: range (start + 1, end)
        }
        else Nil
    }

    println ("P22 solution given K_1 = 3, K_2 = 7 is:   " + rangeOriginal (3, 7))
    println ("P22 solution given K_1 = -5, K_2 = 11 is:   " + rangeOriginal (-5, 11))
    println ("P22 solution given K_1 = 3, K_2 = 7 is:   " + range (3, 7))
    println ("P22 solution given K_1 = -5, K_2 = 11 is:   " + range (-5, 11))
}