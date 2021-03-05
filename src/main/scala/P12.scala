import scala.annotation.tailrec

// Problem 12
// (**) Decode a run-length encoded list.
// Given a run-length code list generated as specified in problem P10, construct its uncompressed version.
//
//
// Example:
//
// scala> decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e)))
// res0: List[Symbol] = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)

object P12 extends App {
    def decodeOriginal (list : List[(Int, Any)]) : Option[List[Any]] = {
        @tailrec
        def innerDecode (decoded : List[Any], innerList : List[(Int, Any)]) : Option[List[Any]] = {
            innerList match {
                case Nil => Some (decoded)
                case hd :: tl if hd._1 == 1 => innerDecode (decoded ::: List (hd._2), tl)
                case hd :: tl => innerDecode (decoded ::: List (hd._2), List ((hd._1 - 1, hd._2)) ::: tl)
            }
        }
        innerDecode (List (), list)
    }

    def decode (list : List[(Int, Any)]) : Option[List[Any]] = {
        @tailrec
        def innerDecode (decoded : List[Any], innerList : List[(Int, Any)]) : Option[List[Any]] = {
            innerList match {
                case Nil => Some (decoded)
                case hd :: tl => innerDecode (decoded ::: List.fill (hd._1) (hd._2), tl)
            }
        }
        innerDecode (List (), list)
    }

    println ("P12 original solution given List ('a', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'd', 'f', 'd', 'd') is:   "
        + decodeOriginal (List((1, 'a'), (2, 'b'), (3, 'c'), (4, 'd'), (5, 'e'), (1, 'f'), (1, 'd'), (1, 'f'), (2, 'd'))))
    println ("P12 solution given List ('a', 'b', 'b', 'c', 'c', 'c', 'd', 'd', 'd', 'd', 'e', 'e', 'e', 'e', 'e', 'f', 'd', 'f', 'd', 'd') is:   "
        + decode (List((1, 'a'), (2, 'b'), (3, 'c'), (4, 'd'), (5, 'e'), (1, 'f'), (1, 'd'), (1, 'f'), (2, 'd'))))
    println ("P12 original solution given List () is:   " + decodeOriginal (List()))
    println ("P12 solution given List () is:   " + decode (List()))
}