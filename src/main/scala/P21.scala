import scala.annotation.tailrec

// Problem 21
// (*) Insert an element at a given position into a list.
//
//
// Example:
//
// scala> insertAt('new, 1, List('a, 'b, 'c, 'd))
// res0: List[Symbol] = List('a, 'new, 'b, 'c, 'd)

object P21 extends App {
    def insertAt (item : Any, K : Int, list : List[Any]) : Option[List[Any]] = {
        @tailrec
        def innerInsert (runningList : List[Any], innerList : List[Any]) : Option[List[Any]] = {
            innerList match {
                case Nil => Some (runningList)
                case hd :: tl if list.length - innerList.length == K => innerInsert (runningList ::: List (item) ::: List (hd), tl)
                case hd :: tl => innerInsert (runningList ::: List (hd), tl)
            }
        }
        innerInsert (List (), list)
    }

    println ("P21 solution given item = \"inserted\", K = 4, list = List (0, 1, 2, 3, 4, 5) is:   " + insertAt ("inserted", 4, List (0, 1, 2, 3, 4, 5)))
    println ("P21 solution given item = \"inserted\", K = 7, list = List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13) is:   " + insertAt ("inserted", 7, List (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)))
    println ("P21 solution given item = \"inserted\", K = 4, list = List () is:   " + insertAt ("inserted", 4, List ()))
}