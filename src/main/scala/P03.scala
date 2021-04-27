import scala.annotation.tailrec

// Problem 3
// (*) Find the Kth element of a list.
//
//
// Example:
//
// scala> nth(2, List(1, 1, 2, 3, 5, 8))
// res0: Int = 2

object P03 extends App {
    @tailrec
    def findElementAt[A] (K : Int, list : List[A]) : Option[A] =
        (K, list) match {
            case (_, Nil) => None
            case (0, hd :: _) => Some (hd)
            case (K, _ :: tl) => findElementAt (K - 1, tl)
    }

    println ("P03 solution given K = 3, list = List (0, 1, 2, 3, 4, 5) is:   " + findElementAt (3, List (0, 1, 2, 3, 4, 5)))
    println ("P03 solution given K = 3, list = List () is:   " + findElementAt (3, List ()))
}