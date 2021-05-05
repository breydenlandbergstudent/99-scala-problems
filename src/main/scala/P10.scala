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

object P10 extends App {
    // generic class DOES need to show itself as a type parameter to another generic class (innerEncode in this case) so this might be the best way to use generics
    def encode[_] (list : List[_]) : Option[List[(Int, _)]] = {
        val packedList = pack (list) get
        @tailrec
        def innerEncode[A] (encoded : List[(Int, A)], innerList : List[List[A]]) : Option[List[(Int, A)]] = {
            innerList match {
                case Nil => Some (encoded)
                case hd :: tl => innerEncode (encoded :+ (hd.length, hd.head), tl)
            }
        }
        innerEncode (List (), packedList)
    }

    println ("P10 solution given List ('a', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'd', 'f', 'd', 'd') is:   "
        + encode (List ('a', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'd', 'f', 'd', 'd')))
    println ("P10 solution given List () is:   " + encode (List ()))

    println
}