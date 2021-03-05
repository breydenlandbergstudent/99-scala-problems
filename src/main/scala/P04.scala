// Problem 4
// (*) Find the number of elements of a list.
//
//
// Example:
//
// scala> length(List(1, 1, 2, 3, 5, 8))
// res0: Int = 6

object P04 extends App {
    def length (list : List[Any]) : Int = {
        list match {
            case Nil => 0
            case _ :: Nil => 1
            case _ :: tl => 1 + length (tl)
        }
    }

    println ("P04 solution given list = List (0, 1, 2, 3, 4, 5) is:   " + length (List (0, 1, 2, 3, 4, 5)))
    println ("P04 solution given list = List () is:   " + length (List ()))
}