import scala.annotation.tailrec

// Problem 1
// (*) Find the last element of a list.
//
//
// Example:
//
// scala> last(List(1, 1, 2, 3, 5, 8))
// res0: Int = 8

object P01 extends App {
    def lastInbuilt (list : List[Any]) : Option[Any] = {
        list lastOption
    }

    @tailrec
    def last (list : List[Any]) : Option[Any] = {
        list match {
            case Nil => None
            case hd :: Nil => Some (hd)
            case _ :: tl => last (tl)
        }
    }

    println ("P01 solution using inbuilt collection method given list = List (0, 1, 2, 3, 4, 5) is:   " + lastInbuilt (List (0, 1, 2, 3, 4, 5)))
    println ("P01 solution given list = List (0, 1, 2, 3, 4, 5) is:   " + last (List (0, 1, 2, 3, 4, 5)))
    println ("P01 solution using inbuilt collection method given list = List () is:   " + lastInbuilt (List ()))
    println ("P01 solution given list = List () is:   " + last (List ()))
}