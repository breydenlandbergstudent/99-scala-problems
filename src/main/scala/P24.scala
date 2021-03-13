import scala.annotation.tailrec

// Problem 24
// (*) Lotto: Draw N different random numbers from the set 1..M.
//
//
// Example:
//
// scala> lotto(6, 49)
// res0: List[Int] = List(23, 1, 17, 33, 21, 37)

object P24 extends App {
    def lotto (N : Int, M : Int) : Option[List[Int]] = {
        @tailrec
        def innerLotto (runningList : List[Int]) : Option[List[Int]] = {
            runningList match {
                case _ if M < 0 => None
                case _ if runningList.length < N => innerLotto (runningList ::: List (scala.util.Random nextInt M + 1))
                case _ => Some (runningList)
            }
        }
        innerLotto (List ())
    }

    println ("P24 solution given N = 3, M = 7 is:   " + lotto (3, 7))
    println ("P24 solution given N = 5, M = 11 is:   " + lotto (5, 11))
    println ("P24 solution given N = 7, M = 77 is:   " + lotto (7, 77))
    println ("P24 solution given N = 7, M = -5 is:   " + lotto (7, -5))
}