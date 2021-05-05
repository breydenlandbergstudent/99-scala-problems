import P09.pack
import scala.annotation.tailrec

// Problem 11
// (*) Modified run-length encoding.
// Modify the result of problem P10 in such a way that if an element has no duplicates it is simply copied into the result list.
// Only elements with duplicates are transferred as (N, E) terms.
//
//
// Example:
//
// scala> encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[Any] = List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e))

object P11 extends App {
    def encodeModified (list : List[Any]) : Option[List[Any]] = {
        val packedList = pack (list) get
        @tailrec
        def innerEncodeModified (encoded : List[Any], innerList : List[List[Any]]) : Option[List[Any]] = {
            innerList match {
                case Nil => Some (encoded)
                case hd :: tl if hd.length == 1 => innerEncodeModified (encoded :+ hd.head, tl)
                case hd :: tl => innerEncodeModified (encoded :+ (hd.length, hd.head), tl)
            }
        }
        innerEncodeModified (List (), packedList)
    }

    println ("P11 solution given List ('a', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'd', 'f', 'd', 'd') is:   "
        + encodeModified (List ('a', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'd', 'f', 'd', 'd')))
    println ("P11 solution given List () is:   " + encodeModified (List ()))

    println
}