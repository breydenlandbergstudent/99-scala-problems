import scala.annotation.tailrec

// Problem 16
// (**) Drop every Nth element from a list.
//
//
// Example:
//
// scala> drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k))
// res0: List[Symbol] = List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k)

object P16 extends App {
    def dropN (N : Int, list : List[Any]) : Option[List[Any]] = {
        @tailrec
        def innerDropN (dropped : List[Any], innerList : List[Any]) : Option[List[Any]] = {
            innerList match {
                case Nil => Some (dropped)
                case _ :: tl if (list.length - innerList.length + 1) % N == 1 => innerDropN (dropped, tl)
                case hd :: tl => innerDropN (dropped ::: List (hd) , tl)
            }
        }
        innerDropN (List (), list)
    }

    // can this be done better?

    println ("P16 solution given List ('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'f', 'f', 'f', 'f') is:   "
        + dropN (3, List ('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'f', 'f', 'f', 'f')))
    println ("P16 solution given List () is:   " + dropN (3, List ()))
}