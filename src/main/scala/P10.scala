import P09.pack
import scala.annotation.tailrec

// Problem 10
// (*) Run-length encoding of a list.
// Use the result of problem P09 to implement the so-called run-length encoding data compression method.
// Consecutive duplicates of elements are encoded as tuples (N, E) where N is the number of duplicates of the element E.
//
//
// Example:
//
// scala> encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
// res0: List[(Int, Symbol)] = List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e))

object P10 extends App{
    def encode[A] (list : List[A]) : Option[List[(Int, A)]] = {
        val packedList = pack (list) get
        @tailrec
        def innerEncode[B] (encoded : List[(Int, B)], innerList : List[List[B]]) : Option[List[(Int, B)]] = {
            innerList match {
                case Nil => Some (encoded)
                case hd :: tl => innerEncode (encoded ::: List ((hd.length, hd.head)), tl)
            }
        }
        innerEncode (List (), packedList)
    }

    println ("P10 solution given List ('a', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'd', 'f', 'd', 'd') is:   "
        + encode (List ('a', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'd', 'f', 'd', 'd')))
    println ("P10 solution given List () is:   " + encode (List ()))
}