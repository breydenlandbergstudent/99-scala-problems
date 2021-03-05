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
    // uses an inner counter mechanism (not very idiomatic)
    def dropNOriginal (N : Int, list : List[Any]) : Option[List[Any]] = {
        @tailrec
        def innerDropN (dropped : List[Any], innerN : Int, innerList : List[Any]): Option[List[Any]] = {
            innerList match {
                case Nil => Some (dropped)
                case _ :: tl if innerN == 1 => innerDropN (dropped, N, tl)
                case hd :: tl => innerDropN (dropped ::: List (hd) , innerN - 1, tl)
            }
        }
        innerDropN (List (), N, list)
    }

    // not finished
    // TODO
    def dropN (N : Int, list : List[Any]) : Option[List[Any]] = {
        def innerDropN (dropped : List[Any], innerList : List[Any]) : Option[List[Any]] = {
            innerList match {
                case Nil => Some (dropped)
                case hd :: tl => ???
            }
        }
        innerDropN (List (), list)
    }

    println ("P16 original solution given List ('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'f', 'f', 'f', 'f') is:   "
        + dropNOriginal (3, List ('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'f', 'f', 'f', 'f')))
    println ("P16 solution given List ('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'f', 'f', 'f', 'f') is:   "
        + dropN (3, List ('a', 'a', 'a', 'a', 'a', 'b', 'b', 'b', 'b', 'b', 'c', 'c', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'f', 'f', 'f', 'f')))
    println ("P16 original solution given List () is:   " + dropNOriginal (3, List ()))
    println ("P16 solution given List () is:   " + dropN (3, List ()))
}