// Problem 5
// (*) Reverse a list.
//
//
// Example:
//
// scala> reverse(List(1, 1, 2, 3, 5, 8))
// res0: List[Int] = List(8, 5, 3, 2, 1, 1)

object P05 extends App {
    def reverse[A] (list : List[A]) : List[A] = {
        list match {
            case Nil => Nil
            case hd :: Nil => List (hd)
            case hd :: tl => reverse (tl) ::: List (hd)
        }
    }

    println ("P05 solution given list = List (0, 1, 2, 3, 4, 5) is:   " + reverse (List (0, 1, 2, 3, 4, 5)))
    println ("P05 solution given list = List () is:   " + reverse (List ()))
}